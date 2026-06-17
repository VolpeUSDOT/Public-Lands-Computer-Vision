from __future__ import annotations

import argparse
import base64
import csv
import json
import os
import re
import time
import tempfile
from dataclasses import asdict, dataclass, field
from datetime import datetime, timedelta, timezone
from functools import lru_cache
from pathlib import Path
from typing import Any, Optional
from zoneinfo import ZoneInfo

os.environ.pop("OPENSSL_FORCE_FIPS_MODE", None)

import cv2
import numpy as np
import requests
from bs4 import BeautifulSoup
from ultralytics import YOLO


DEFAULT_IPCAMLIVE_LANDING_PAGE_URL = "https://www.ipcamlive.com/willowcreektrail"
DEFAULT_WEBCAM_PAGE_URL = DEFAULT_IPCAMLIVE_LANDING_PAGE_URL
DEFAULT_FALLBACK_IMAGE_URL = "https://s94.ipcamlive.com/streams/5ey6dfot2hpuvzbce/snapshot.jpg"
IPCAMLIVE_STREAM_STATE_PATH = "/ajax/getcamerastreamstate.php"
DEFAULT_MODEL_PATH = "yolov8x.pt"
DEFAULT_BASE_OUTPUT_DIR = Path(__file__).resolve().parent.parent / "IP3" / "WCT"
DEFAULT_ARCHIVE_OUTPUT_PATH = DEFAULT_BASE_OUTPUT_DIR / "archivefeed.csv"
DEFAULT_FEED_OUTPUT_PATH = Path(__file__).resolve().parent / "wct_latest_feed.txt"
DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH = Path(__file__).resolve().parent / "wct_latest_annotated.jpg"
DEFAULT_GITHUB_REPOSITORY = "VolpeUSDOT/Public-Lands-Computer-Vision"
DEFAULT_GITHUB_BRANCH = "main"
DEFAULT_GITHUB_JSON_PATH = "IP3/WCT/wct_vehicle_count_latest.json"
DEFAULT_GITHUB_FEED_PATH = "IP3/WCT/wct_latest_feed.txt"
DEFAULT_GITHUB_IMAGE_PATH = "IP3/WCT/wct_latest_annotated.jpg"
DEFAULT_GITHUB_ARCHIVE_CSV_PATH = "IP3/WCT/archivefeed.csv"
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
DEFAULT_OUTPUT_PATH = DEFAULT_BASE_OUTPUT_DIR / "wct_vehicle_count_latest.json"
DEFAULT_ENV_PATH = Path(__file__).resolve().parent / ".env"
VEHICLE_CLASSES = [2, 3, 5, 7]
DEFAULT_LOOP_INTERVAL_SECONDS = 50
DEFAULT_TIMEZONE_NAME = "America/New_York"
DEFAULT_ACTIVE_START_HOUR = 9
DEFAULT_ACTIVE_END_HOUR = 21
TRACK_MATCH_IOU_THRESHOLD = 0.30
TRACK_MAX_MISSED_SECONDS = 180


@dataclass(frozen=True)
class Config:
    webcam_page_url: str = DEFAULT_WEBCAM_PAGE_URL
    fallback_image_url: str = DEFAULT_FALLBACK_IMAGE_URL
    model_path: str = DEFAULT_MODEL_PATH
    output_path: Path = DEFAULT_OUTPUT_PATH
    feed_output_path: Path = DEFAULT_FEED_OUTPUT_PATH
    annotated_image_output_path: Path = DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH
    github_repository: str = DEFAULT_GITHUB_REPOSITORY
    github_branch: str = DEFAULT_GITHUB_BRANCH
    github_json_path: str = DEFAULT_GITHUB_JSON_PATH
    github_feed_path: str = DEFAULT_GITHUB_FEED_PATH
    github_image_path: str = DEFAULT_GITHUB_IMAGE_PATH
    github_archive_csv_path: str = DEFAULT_GITHUB_ARCHIVE_CSV_PATH
    github_token: Optional[str] = None
    publish_to_github: bool = False
    confidence: float = 0.50
    iou: float = 0.45
    image_size: int = 1280
    user_agent: str = DEFAULT_USER_AGENT
    loop_interval_seconds: int = DEFAULT_LOOP_INTERVAL_SECONDS
    archive_output_path: Path = DEFAULT_ARCHIVE_OUTPUT_PATH
    timezone_name: str = DEFAULT_TIMEZONE_NAME
    active_start_hour: int = DEFAULT_ACTIVE_START_HOUR
    active_end_hour: int = DEFAULT_ACTIVE_END_HOUR


@dataclass
class DetectionBox:
    class_id: int
    class_name: str
    confidence: float
    xyxy: list[float]


@dataclass
class ArchiveRow:
    timestamp_utc: str
    vehicle_count: int
    vehicle_index: int
    vehicle_type: str
    confidence: float
    bbox_x1: float
    bbox_y1: float
    bbox_x2: float
    bbox_y2: float


@dataclass
class RunResult:
    status: str
    timestamp_utc: str
    webcam_page_url: str
    image_url: Optional[str] = None
    model_path: str = DEFAULT_MODEL_PATH
    vehicle_count: int = 0
    tracked_vehicle_count: int = 0
    average_vehicle_dwell_seconds: Optional[float] = None
    detections: list[DetectionBox] = field(default_factory=list)
    message: Optional[str] = None
    error: Optional[str] = None


@dataclass
class VehicleTrack:
    track_id: int
    class_id: int
    class_name: str
    first_seen_ts: float
    last_seen_ts: float
    confidence: float
    xyxy: list[float]
    missed_count: int = 0


@dataclass
class TrackerSummary:
    active_vehicle_count: int
    average_vehicle_dwell_seconds: Optional[float]
    completed_vehicle_count: int


class VehicleTracker:
    def __init__(self) -> None:
        self._next_track_id = 1
        self._active_tracks: dict[int, VehicleTrack] = {}
        self._completed_dwell_seconds: list[float] = []

    @staticmethod
    def _track_iou(track: VehicleTrack, detection: DetectionBox) -> float:
        return iou_xyxy(track.xyxy, detection.xyxy)

    def _finalize_track(self, track_id: int) -> None:
        track = self._active_tracks.pop(track_id, None)
        if track is None:
            return
        dwell_seconds = max(0.0, track.last_seen_ts - track.first_seen_ts)
        self._completed_dwell_seconds.append(dwell_seconds)

    def update(self, detections: list[DetectionBox], observed_ts: float) -> TrackerSummary:
        if self._active_tracks:
            stale_track_ids = [
                track_id
                for track_id, track in self._active_tracks.items()
                if observed_ts - track.last_seen_ts >= TRACK_MAX_MISSED_SECONDS
            ]
            for track_id in stale_track_ids:
                self._finalize_track(track_id)

        candidate_matches: list[tuple[float, int, int]] = []
        active_track_items = list(self._active_tracks.items())
        for track_index, (_track_id, track) in enumerate(active_track_items):
            for detection_index, detection in enumerate(detections):
                if track.class_id != detection.class_id:
                    continue
                score = self._track_iou(track, detection)
                if score >= TRACK_MATCH_IOU_THRESHOLD:
                    candidate_matches.append((score, track_index, detection_index))

        candidate_matches.sort(reverse=True)
        matched_tracks: set[int] = set()
        matched_detections: set[int] = set()

        for _score, track_index, detection_index in candidate_matches:
            if track_index in matched_tracks or detection_index in matched_detections:
                continue
            track_id, track = active_track_items[track_index]
            detection = detections[detection_index]
            track.class_id = detection.class_id
            track.class_name = detection.class_name
            track.confidence = detection.confidence
            track.xyxy = list(detection.xyxy)
            track.last_seen_ts = observed_ts
            track.missed_count = 0
            matched_tracks.add(track_index)
            matched_detections.add(detection_index)

        for detection_index, detection in enumerate(detections):
            if detection_index in matched_detections:
                continue
            self._active_tracks[self._next_track_id] = VehicleTrack(
                track_id=self._next_track_id,
                class_id=detection.class_id,
                class_name=detection.class_name,
                first_seen_ts=observed_ts,
                last_seen_ts=observed_ts,
                confidence=detection.confidence,
                xyxy=list(detection.xyxy),
            )
            self._next_track_id += 1

        for track_index, (track_id, track) in enumerate(active_track_items):
            if track_index in matched_tracks:
                continue
            track.missed_count += 1
            if observed_ts - track.last_seen_ts >= TRACK_MAX_MISSED_SECONDS:
                self._finalize_track(track_id)

        average_vehicle_dwell_seconds = self.average_vehicle_dwell_seconds(observed_ts)
        return TrackerSummary(
            active_vehicle_count=len(self._active_tracks),
            average_vehicle_dwell_seconds=average_vehicle_dwell_seconds,
            completed_vehicle_count=len(self._completed_dwell_seconds),
        )

    def average_vehicle_dwell_seconds(self, observed_ts: float) -> Optional[float]:
        durations = list(self._completed_dwell_seconds)
        for track in self._active_tracks.values():
            durations.append(max(0.0, observed_ts - track.first_seen_ts))
        if not durations:
            return None
        return sum(durations) / len(durations)


_VEHICLE_TRACKER = VehicleTracker()


def iou_xyxy(box_a: list[float], box_b: list[float]) -> float:
    ax1, ay1, ax2, ay2 = box_a
    bx1, by1, bx2, by2 = box_b

    inter_x1 = max(ax1, bx1)
    inter_y1 = max(ay1, by1)
    inter_x2 = min(ax2, bx2)
    inter_y2 = min(ay2, by2)

    inter_width = max(0.0, inter_x2 - inter_x1)
    inter_height = max(0.0, inter_y2 - inter_y1)
    inter_area = inter_width * inter_height

    area_a = max(0.0, ax2 - ax1) * max(0.0, ay2 - ay1)
    area_b = max(0.0, bx2 - bx1) * max(0.0, by2 - by1)
    union_area = area_a + area_b - inter_area
    if union_area <= 0.0:
        return 0.0
    return inter_area / union_area


def load_config() -> Config:
    """Load runtime settings from environment variables."""

    load_env_file()
    output_path = Path(os.getenv("OUTPUT_PATH", str(DEFAULT_OUTPUT_PATH))).expanduser()
    feed_output_path = Path(os.getenv("FEED_OUTPUT_PATH", str(DEFAULT_FEED_OUTPUT_PATH))).expanduser()
    annotated_image_output_path = Path(os.getenv("ANNOTATED_IMAGE_OUTPUT_PATH", str(DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH))).expanduser()
    github_token = os.getenv("GITHUB_TOKEN") or os.getenv("GH_TOKEN")
    publish_to_github = os.getenv("PUBLISH_TO_GITHUB", "").strip().lower() in {"1", "true", "yes", "on"}
    return Config(
        webcam_page_url=os.getenv("WEBCAM_PAGE_URL", DEFAULT_WEBCAM_PAGE_URL),
        fallback_image_url=os.getenv("FALLBACK_IMAGE_URL", DEFAULT_FALLBACK_IMAGE_URL),
        model_path=os.getenv("MODEL_PATH", DEFAULT_MODEL_PATH),
        output_path=output_path,
        feed_output_path=feed_output_path,
        annotated_image_output_path=annotated_image_output_path,
        github_repository=os.getenv("GITHUB_REPOSITORY", DEFAULT_GITHUB_REPOSITORY),
        github_branch=os.getenv("GITHUB_BRANCH", DEFAULT_GITHUB_BRANCH),
        github_json_path=os.getenv("GITHUB_JSON_PATH", DEFAULT_GITHUB_JSON_PATH),
        github_feed_path=os.getenv("GITHUB_FEED_PATH", DEFAULT_GITHUB_FEED_PATH),
        github_image_path=os.getenv("GITHUB_IMAGE_PATH", DEFAULT_GITHUB_IMAGE_PATH),
        github_archive_csv_path=os.getenv("GITHUB_ARCHIVE_CSV_PATH", DEFAULT_GITHUB_ARCHIVE_CSV_PATH),
        github_token=github_token,
        publish_to_github=publish_to_github,
        confidence=float(os.getenv("YOLO_CONFIDENCE", "0.50")),
        iou=float(os.getenv("YOLO_IOU", "0.45")),
        image_size=int(os.getenv("YOLO_IMAGE_SIZE", "1280")),
        user_agent=os.getenv("USER_AGENT", DEFAULT_USER_AGENT),
        loop_interval_seconds=int(os.getenv("LOOP_INTERVAL_SECONDS", str(DEFAULT_LOOP_INTERVAL_SECONDS))),
        archive_output_path=Path(os.getenv("ARCHIVE_OUTPUT_PATH", str(DEFAULT_ARCHIVE_OUTPUT_PATH))).expanduser(),
        timezone_name=os.getenv("TIMEZONE_NAME", DEFAULT_TIMEZONE_NAME),
        active_start_hour=int(os.getenv("ACTIVE_START_HOUR", str(DEFAULT_ACTIVE_START_HOUR))),
        active_end_hour=int(os.getenv("ACTIVE_END_HOUR", str(DEFAULT_ACTIVE_END_HOUR))),
    )


def build_headers(user_agent: str) -> dict[str, str]:
    return {"User-Agent": user_agent}


def build_ipcamlive_snapshot_url(address: str, stream_id: str) -> str:
    return f"{address.rstrip('/')}/streams/{stream_id}/snapshot.jpg"


def extract_ipcamlive_alias(page_text: str, page_url: str) -> Optional[str]:
    alias_match = re.search(r"var\s+alias\s*=\s*['\"]([^'\"]+)['\"]", page_text)
    if alias_match:
        return alias_match.group(1)

    query_match = re.search(r"[?&]alias=([^&]+)", page_url)
    if query_match:
        return query_match.group(1)

    path_match = re.search(r"ipcamlive\.com/([^/?#]+)", page_url)
    if path_match:
        candidate = path_match.group(1).strip()
        if candidate and candidate not in {"player", "ajax"}:
            return candidate

    return None


def parse_ipcamlive_stream_state(response: requests.Response) -> Optional[dict[str, Any]]:
    text = response.text.strip()
    if not text:
        return None

    try:
        parsed = response.json()
    except ValueError:
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            match = re.search(r"\{.*\}", text, re.DOTALL)
            if not match:
                return None
            try:
                parsed = json.loads(match.group(0))
            except json.JSONDecodeError:
                return None

    return parsed if isinstance(parsed, dict) else None


def resolve_ipcamlive_snapshot_url(alias: str, headers: dict[str, str]) -> Optional[str]:
    state_url = f"https://www.ipcamlive.com{IPCAMLIVE_STREAM_STATE_PATH}"
    response = requests.get(state_url, headers=headers, params={"cameraalias": alias}, timeout=10)
    response.raise_for_status()

    data = parse_ipcamlive_stream_state(response)
    if not data:
        return None

    details = data.get("details") or {}
    stream_id = details.get("streamid")
    address = details.get("address")
    if stream_id and address:
        return build_ipcamlive_snapshot_url(address, stream_id)
    return None


def resolve_ipcamlive_player_url(alias: str, headers: dict[str, str]) -> Optional[str]:
    landing_url = f"https://www.ipcamlive.com/{alias}"
    response = requests.get(landing_url, headers=headers, timeout=10)
    response.raise_for_status()

    page_text = response.text
    token_match = re.search(r"var\s+token\s*=\s*['\"]([^'\"]+)['\"]", page_text)
    if not token_match:
        return None

    token = token_match.group(1)
    player_match = re.search(r"player/player\.php\?alias=([^&'\"]+)", page_text)
    player_alias = player_match.group(1) if player_match else alias
    return f"https://www.ipcamlive.com/player/player.php?alias={player_alias}&autoplay=1&&token={requests.utils.quote(token, safe='')}"


def now_utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def archive_timestamp() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")


def local_now(config: Config) -> datetime:
    return datetime.now(ZoneInfo(config.timezone_name))


def is_within_active_window(now: datetime, config: Config) -> bool:
    start = now.replace(hour=config.active_start_hour, minute=0, second=0, microsecond=0)
    end = now.replace(hour=config.active_end_hour, minute=0, second=0, microsecond=0)
    return start <= now < end


def seconds_until_active_window(now: datetime, config: Config) -> float:
    start = now.replace(hour=config.active_start_hour, minute=0, second=0, microsecond=0)
    end = now.replace(hour=config.active_end_hour, minute=0, second=0, microsecond=0)
    if now < start:
        return (start - now).total_seconds()
    if now >= end:
        next_start = (start + timedelta(days=1)).replace(tzinfo=now.tzinfo)
        return (next_start - now).total_seconds()
    return 0.0


def load_env_file(env_path: Path = DEFAULT_ENV_PATH) -> None:
    """Load simple KEY=VALUE pairs from a local .env file if present."""

    if not env_path.exists():
        return

    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue

        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip()

        if not key or key in os.environ:
            continue

        if (value.startswith('"') and value.endswith('"')) or (value.startswith("'") and value.endswith("'")):
            value = value[1:-1]

        os.environ[key] = value


def resolve_image_url(page_html: bytes, page_url: str, fallback_image_url: str, headers: dict[str, str]) -> Optional[str]:
    page_text = page_html.decode("utf-8", errors="ignore")
    soup = BeautifulSoup(page_html, "html.parser")

    if "ipcamlive.com" in page_url:
        alias = extract_ipcamlive_alias(page_text, page_url)
        if alias:
            snapshot_url = resolve_ipcamlive_snapshot_url(alias, headers)
            if snapshot_url:
                return snapshot_url

            player_url = resolve_ipcamlive_player_url(alias, headers)
            if player_url:
                return player_url

            return fallback_image_url

        iframe_match = re.search(r"<iframe[^>]+src=[\"']([^\"']*player/player\.php[^\"']*)[\"']", page_text, re.IGNORECASE)
        if iframe_match:
            iframe_src = iframe_match.group(1)
            iframe_alias = extract_ipcamlive_alias("", iframe_src)
            if iframe_alias:
                snapshot_url = resolve_ipcamlive_snapshot_url(iframe_alias, headers)
                if snapshot_url:
                    return snapshot_url

                player_url = resolve_ipcamlive_player_url(iframe_alias, headers)
                if player_url:
                    return player_url

        return None

    for img in soup.find_all("img"):
        src = img.get("src", "")
        if ".jpg" in src.lower() and ("webcam" in src.lower() or "arch" in src.lower()):
            if src.startswith("/"):
                return "https://www.nps.gov" + src
            return src

    address_match = re.search(r"var\s+address\s*=\s*['\"](https?://[^'\"]+/)['\"]", page_text)
    streamid_match = re.search(r"var\s+streamid\s*=\s*['\"]([^'\"]+)['\"]", page_text)
    if address_match and streamid_match:
        address = address_match.group(1)
        streamid = streamid_match.group(1)
        return build_ipcamlive_snapshot_url(address, streamid)

    return fallback_image_url


def resolve_writable_output_path(output_path: Path) -> Path:
    """Return a path we can write to, falling back to a temp location if needed."""

    try:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        return output_path
    except (PermissionError, OSError):
        fallback_path = Path(tempfile.gettempdir()) / "wct" / output_path.name
        fallback_path.parent.mkdir(parents=True, exist_ok=True)
        return fallback_path


def try_write_text(output_path: Path, content: str) -> Path:
    writable_path = resolve_writable_output_path(output_path)
    try:
        writable_path.write_text(content, encoding="utf-8")
        return writable_path
    except (PermissionError, OSError):
        fallback_path = Path(tempfile.gettempdir()) / "wct" / writable_path.name
        fallback_path.parent.mkdir(parents=True, exist_ok=True)
        fallback_path.write_text(content, encoding="utf-8")
        return fallback_path


def try_append_archive_rows(output_path: Path, rows: list[ArchiveRow]) -> Path:
    writable_path = resolve_writable_output_path(output_path)

    def _append(path: Path) -> None:
        file_exists = path.exists() and path.stat().st_size > 0
        with path.open("a", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=archive_csv_header())
            if not file_exists:
                writer.writeheader()
            for row in rows:
                writer.writerow(archive_row_to_dict(row))

    try:
        _append(writable_path)
        return writable_path
    except (PermissionError, OSError):
        fallback_path = Path(tempfile.gettempdir()) / "wct" / writable_path.name
        fallback_path.parent.mkdir(parents=True, exist_ok=True)
        _append(fallback_path)
        return fallback_path


def fetch_bytes(url: str, headers: dict[str, str], timeout: int = 10) -> requests.Response:
    response = requests.get(url, headers=headers, timeout=timeout)
    response.raise_for_status()
    return response


def download_image(image_url: str, headers: dict[str, str]) -> np.ndarray:
    response = fetch_bytes(image_url, headers=headers)
    image_array = np.asarray(bytearray(response.content), dtype=np.uint8)
    img = cv2.imdecode(image_array, cv2.IMREAD_COLOR)
    if img is None:
        raise ValueError("OpenCV could not decode the downloaded image")
    return img


@lru_cache(maxsize=1)
def load_model(model_path: str) -> YOLO:
    return YOLO(model_path)


def serialize_detections(result: Any) -> list[DetectionBox]:
    if not result.boxes:
        return []

    names = result.names or {}
    detections: list[DetectionBox] = []
    for box in result.boxes:
        class_id = int(box.cls[0]) if getattr(box, "cls", None) is not None else -1
        confidence = float(box.conf[0]) if getattr(box, "conf", None) is not None else 0.0
        coords = [float(value) for value in box.xyxy[0].tolist()]
        detections.append(
            DetectionBox(
                class_id=class_id,
                class_name=str(names.get(class_id, class_id)),
                confidence=confidence,
                xyxy=coords,
            )
        )
    return detections


def run_detection(config: Config) -> RunResult:
    return run_detection_with_image(config)[0]


def run_detection_with_image(config: Config) -> tuple[RunResult, Optional[np.ndarray]]:
    headers = build_headers(config.user_agent)
    timestamp = now_utc_iso()

    try:
        page_response = fetch_bytes(config.webcam_page_url, headers=headers)
        image_url = resolve_image_url(page_response.content, config.webcam_page_url, config.fallback_image_url, headers)

        if not image_url and "ipcamlive.com" in config.webcam_page_url:
            alias = extract_ipcamlive_alias(page_response.text, config.webcam_page_url)
            if alias:
                image_url = resolve_ipcamlive_snapshot_url(alias, headers) or resolve_ipcamlive_player_url(alias, headers)

        if not image_url:
            image_url = config.fallback_image_url

        if "/player/player.php" in image_url:
            # Some IPCamLive pages only expose an iframe player, so chase one more hop.
            player_response = fetch_bytes(image_url, headers=headers)
            iframe_match = re.search(r"<iframe[^>]+src=[\"']([^\"']+)[\"']", player_response.text, re.IGNORECASE)
            if iframe_match:
                image_url = iframe_match.group(1)
            else:
                image_url = None

        if not image_url:
            image_url = config.fallback_image_url

        img = download_image(image_url, headers=headers)

        model = load_model(config.model_path)
        results = model.predict(
            img,
            classes=VEHICLE_CLASSES,
            conf=config.confidence,
            iou=config.iou,
            imgsz=config.image_size,
            verbose=False,
        )

        detections = serialize_detections(results[0]) if results else []
        tracker_summary = _VEHICLE_TRACKER.update(detections, datetime.now(timezone.utc).timestamp())
        annotated_image = annotate_image(img, results)
        return (
            RunResult(
                status="ok",
                timestamp_utc=timestamp,
                webcam_page_url=config.webcam_page_url,
                image_url=image_url,
                model_path=config.model_path,
                vehicle_count=len(detections),
                tracked_vehicle_count=tracker_summary.active_vehicle_count,
                average_vehicle_dwell_seconds=tracker_summary.average_vehicle_dwell_seconds,
                detections=detections,
                message=f"Detected {len(detections)} vehicles; tracking {tracker_summary.active_vehicle_count} active vehicles",
            ),
            annotated_image,
        )
    except Exception as exc:
        return (
            RunResult(
                status="error",
                timestamp_utc=timestamp,
                webcam_page_url=config.webcam_page_url,
                model_path=config.model_path,
                error=str(exc),
            ),
            None,
        )


def annotate_image(img: np.ndarray, results: Any) -> np.ndarray:
    if not results:
        return img
    return results[0].plot()


def result_to_dict(result: RunResult) -> dict[str, Any]:
    payload = asdict(result)
    payload["detections"] = [asdict(detection) for detection in result.detections]
    payload["output_schema"] = "wct_vehicle_count_v2"
    return payload


def result_to_json_text(result: RunResult) -> str:
    return json.dumps(result_to_dict(result), indent=2) + "\n"


def result_to_feed_text(result: RunResult) -> str:
    lines = [
        "Willow Creek Trail Parking Lot webcam vehicle feed",
        f"status: {result.status}",
        f"timestamp_utc: {result.timestamp_utc}",
        f"webcam_page_url: {result.webcam_page_url}",
        f"image_url: {result.image_url or ''}",
        f"model_path: {result.model_path}",
        f"vehicle_count: {result.vehicle_count}",
        f"tracked_vehicle_count: {result.tracked_vehicle_count}",
        f"average_vehicle_dwell_seconds: {result.average_vehicle_dwell_seconds if result.average_vehicle_dwell_seconds is not None else ''}",
    ]

    if result.message:
        lines.append(f"message: {result.message}")

    if result.error:
        lines.append(f"error: {result.error}")

    if result.detections:
        lines.append("detections:")
        for detection in result.detections:
            lines.append(
                f"- {detection.class_name}"
                f" (class_id={detection.class_id}, confidence={detection.confidence:.3f})"
                f" xyxy={detection.xyxy}"
            )
    else:
        lines.append("detections: none")

    lines.append("output_schema: wct_vehicle_count_v2")
    return "\n".join(lines) + "\n"


def result_to_archive_rows(result: RunResult) -> list[ArchiveRow]:
    rows: list[ArchiveRow] = []
    vehicle_count = len(result.detections)
    for index, detection in enumerate(result.detections, start=1):
        x1, y1, x2, y2 = detection.xyxy
        rows.append(
            ArchiveRow(
                timestamp_utc=result.timestamp_utc,
                vehicle_count=vehicle_count,
                vehicle_index=index,
                vehicle_type=detection.class_name,
                confidence=detection.confidence,
                bbox_x1=x1,
                bbox_y1=y1,
                bbox_x2=x2,
                bbox_y2=y2,
            )
        )
    if not rows:
        rows.append(
            ArchiveRow(
                timestamp_utc=result.timestamp_utc,
                vehicle_count=0,
                vehicle_index=0,
                vehicle_type="none",
                confidence=0.0,
                bbox_x1=0.0,
                bbox_y1=0.0,
                bbox_x2=0.0,
                bbox_y2=0.0,
            )
        )
    return rows


def result_to_archive_csv_rows(result: RunResult) -> list[dict[str, Any]]:
    return [archive_row_to_dict(row) for row in result_to_archive_rows(result)]


def archive_csv_header() -> list[str]:
    return [
        "timestamp_utc",
        "vehicle_count",
        "vehicle_index",
        "vehicle_type",
        "confidence",
        "bbox_x1",
        "bbox_y1",
        "bbox_x2",
        "bbox_y2",
    ]


def archive_row_to_dict(row: ArchiveRow) -> dict[str, Any]:
    return asdict(row)


def archive_rows_to_csv_text(rows: list[ArchiveRow]) -> str:
    from io import StringIO

    buffer = StringIO()
    writer = csv.DictWriter(buffer, fieldnames=archive_csv_header())
    writer.writeheader()
    for row in rows:
        writer.writerow(archive_row_to_dict(row))
    return buffer.getvalue()


def csv_text_to_rows(csv_text: str) -> list[dict[str, str]]:
    from io import StringIO

    if not csv_text.strip():
        return []
    buffer = StringIO(csv_text)
    return list(csv.DictReader(buffer))


def rows_to_csv_text(rows: list[dict[str, Any]]) -> str:
    from io import StringIO

    buffer = StringIO()
    writer = csv.DictWriter(buffer, fieldnames=archive_csv_header())
    writer.writeheader()
    for row in rows:
        writer.writerow(row)
    return buffer.getvalue()


def write_result(result: RunResult, output_path: Path) -> Path:
    return try_write_text(output_path, result_to_json_text(result))


def write_feed(result: RunResult, output_path: Path) -> Path:
    return try_write_text(output_path, result_to_feed_text(result))


def write_annotated_image(img: np.ndarray, output_path: Path) -> Path:
    writable_path = resolve_writable_output_path(output_path)
    if cv2.imwrite(str(writable_path), img):
        return writable_path

    fallback_path = Path(tempfile.gettempdir()) / "wct" / writable_path.name
    fallback_path.parent.mkdir(parents=True, exist_ok=True)
    if not cv2.imwrite(str(fallback_path), img):
        raise ValueError(f"Failed to write annotated image to {output_path}")
    return fallback_path


def append_archive_csv(result: RunResult, output_path: Path) -> Path:
    return try_append_archive_rows(output_path, result_to_archive_rows(result))


def github_contents_api_url(repository: str, file_path: str) -> str:
    return f"https://api.github.com/repos/{repository}/contents/{file_path.lstrip('/')}"


def get_github_headers(token: str) -> dict[str, str]:
    return {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    }


def publish_text_to_github(content_text: str, repository: str, branch: str, file_path: str, token: str, message: str) -> tuple[bool, str]:
    headers = get_github_headers(token)
    url = github_contents_api_url(repository, file_path)

    current_sha: Optional[str] = None
    try:
        response = requests.get(url, headers=headers, params={"ref": branch}, timeout=10)
        if response.status_code == 200:
            payload = response.json()
            current_sha = payload.get("sha")

            existing_content = payload.get("content")
            encoding = payload.get("encoding")
            if existing_content and encoding == "base64":
                decoded = base64.b64decode(existing_content).decode("utf-8")
                if decoded == content_text:
                    return True, f"GitHub file already up to date: {file_path}"
        elif response.status_code != 404:
            response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to inspect GitHub file {file_path}: {exc}"

    body: dict[str, Any] = {
        "message": message,
        "content": base64.b64encode(content_text.encode("utf-8")).decode("ascii"),
        "branch": branch,
    }
    if current_sha:
        body["sha"] = current_sha

    try:
        response = requests.put(url, headers=headers, json=body, timeout=20)
        response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to publish GitHub file {file_path}: {exc}"

    return True, f"Published {repository}/{file_path} on {branch}"


def publish_image_to_github(image: np.ndarray, repository: str, branch: str, file_path: str, token: str, message: str) -> tuple[bool, str]:
    suffix = Path(file_path).suffix.lower()
    ext = ".png" if suffix == ".png" else ".jpg"
    ok, encoded = cv2.imencode(ext, image)
    if not ok:
        return False, f"Failed to encode annotated image for {file_path}"

    image_bytes = encoded.tobytes()
    content_text = base64.b64encode(image_bytes).decode("ascii")

    headers = get_github_headers(token)
    url = github_contents_api_url(repository, file_path)

    current_sha: Optional[str] = None
    try:
        response = requests.get(url, headers=headers, params={"ref": branch}, timeout=10)
        if response.status_code == 200:
            payload = response.json()
            current_sha = payload.get("sha")
            existing_content = payload.get("content")
            encoding = payload.get("encoding")
            if existing_content and encoding == "base64":
                existing_bytes = base64.b64decode(existing_content)
                if existing_bytes == image_bytes:
                    return True, f"GitHub file already up to date: {file_path}"
        elif response.status_code != 404:
            response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to inspect GitHub file {file_path}: {exc}"

    body: dict[str, Any] = {
        "message": message,
        "content": content_text,
        "branch": branch,
    }
    if current_sha:
        body["sha"] = current_sha

    try:
        response = requests.put(url, headers=headers, json=body, timeout=20)
        response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to publish GitHub file {file_path}: {exc}"

    return True, f"Published {repository}/{file_path} on {branch}"


def publish_feed_to_github(feed_text: str, config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    return publish_text_to_github(
        feed_text,
        config.github_repository,
        config.github_branch,
        config.github_feed_path,
        config.github_token,
        "Update WCT webcam feed",
    )


def publish_json_to_github(json_text: str, config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    return publish_text_to_github(
        json_text,
        config.github_repository,
        config.github_branch,
        config.github_json_path,
        config.github_token,
        "Update WCT vehicle count JSON",
    )


def publish_annotated_image_to_github(image: np.ndarray, config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    return publish_image_to_github(
        image,
        config.github_repository,
        config.github_branch,
        config.github_image_path,
        config.github_token,
        "Update WCT annotated image",
    )


def publish_archive_csv_to_github(csv_text: str, config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub archive publish: no GITHUB_TOKEN or GH_TOKEN set"
    headers = get_github_headers(config.github_token)
    url = github_contents_api_url(config.github_repository, config.github_archive_csv_path)

    existing_text = ""
    current_sha: Optional[str] = None
    try:
        response = requests.get(url, headers=headers, params={"ref": config.github_branch}, timeout=10)
        if response.status_code == 200:
            payload = response.json()
            current_sha = payload.get("sha")
            existing_content = payload.get("content")
            encoding = payload.get("encoding")
            if existing_content and encoding == "base64":
                existing_text = base64.b64decode(existing_content).decode("utf-8")
        elif response.status_code != 404:
            response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to inspect GitHub archive CSV {config.github_archive_csv_path}: {exc}"

    existing_rows = csv_text_to_rows(existing_text)
    incoming_rows = csv_text_to_rows(csv_text)
    combined_rows = existing_rows + incoming_rows
    combined_text = rows_to_csv_text(combined_rows)

    body: dict[str, Any] = {
        "message": "Append WCT archive CSV",
        "content": base64.b64encode(combined_text.encode("utf-8")).decode("ascii"),
        "branch": config.github_branch,
    }
    if current_sha:
        body["sha"] = current_sha

    try:
        response = requests.put(url, headers=headers, json=body, timeout=20)
        response.raise_for_status()
    except requests.RequestException as exc:
        return False, f"Failed to publish GitHub archive CSV {config.github_archive_csv_path}: {exc}"

    return True, f"Published {config.github_repository}/{config.github_archive_csv_path} on {config.github_branch}"


def archive_csv_rows_to_text(rows: list[ArchiveRow]) -> str:
    return archive_rows_to_csv_text(rows)


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Count vehicles in the Willow Creek Trail Parking Lot webcam image")
    parser.add_argument("--output", type=Path, default=None, help="Override output JSON path")
    parser.add_argument("--feed-output", type=Path, default=None, help="Override local feed text output path")
    parser.add_argument("--archive-output-path", type=Path, default=None, help="Override local archive CSV output path")
    parser.add_argument("--model", default=None, help="Override YOLO model path")
    parser.add_argument("--confidence", type=float, default=None, help="Override YOLO confidence threshold")
    parser.add_argument("--iou", type=float, default=None, help="Override YOLO IOU threshold")
    parser.add_argument("--imgsz", type=int, default=None, help="Override YOLO image size")
    parser.add_argument("--interval", type=int, default=None, help="Seconds to wait between detection runs")
    parser.add_argument("--timezone", default=None, help="Override the active window timezone")
    parser.add_argument("--active-start-hour", type=int, default=None, help="Override active window start hour")
    parser.add_argument("--active-end-hour", type=int, default=None, help="Override active window end hour")
    parser.add_argument("--once", action="store_true", help="Run one detection cycle and exit")
    args, _unknown = parser.parse_known_args(argv)
    return args


def build_config(args: argparse.Namespace) -> Config:
    config = load_config()
    return Config(
        webcam_page_url=config.webcam_page_url,
        fallback_image_url=config.fallback_image_url,
        model_path=args.model or config.model_path,
        output_path=args.output or config.output_path,
        feed_output_path=args.feed_output or config.feed_output_path,
        github_repository=config.github_repository,
        github_branch=config.github_branch,
        github_json_path=config.github_json_path,
        github_feed_path=config.github_feed_path,
        github_image_path=config.github_image_path,
        github_archive_csv_path=config.github_archive_csv_path,
        github_token=config.github_token,
        publish_to_github=config.publish_to_github,
        confidence=args.confidence if args.confidence is not None else config.confidence,
        iou=args.iou if args.iou is not None else config.iou,
        image_size=args.imgsz if args.imgsz is not None else config.image_size,
        user_agent=config.user_agent,
        loop_interval_seconds=args.interval if args.interval is not None else config.loop_interval_seconds,
        archive_output_path=args.archive_output_path or config.archive_output_path,
        timezone_name=args.timezone or config.timezone_name,
        active_start_hour=args.active_start_hour if args.active_start_hour is not None else config.active_start_hour,
        active_end_hour=args.active_end_hour if args.active_end_hour is not None else config.active_end_hour,
    )


def run_once(config: Config) -> int:
    result, annotated_image = run_detection_with_image(config)
    json_text = result_to_json_text(result)
    feed_text = result_to_feed_text(result)

    publish_messages: list[str] = []
    publish_ok = True
    if config.publish_to_github:
        json_published, json_message = publish_json_to_github(json_text, config)
        feed_published, feed_message = publish_feed_to_github(feed_text, config)
        image_published = True
        image_message = None
        if annotated_image is not None:
            image_published, image_message = publish_annotated_image_to_github(annotated_image, config)

        archive_csv_text = archive_rows_to_csv_text(result_to_archive_rows(result))
        archive_csv_published, archive_csv_message = publish_archive_csv_to_github(archive_csv_text, config)
        publish_ok = (
            json_published
            and feed_published
            and image_published
            and archive_csv_published
        )
        publish_messages.extend([json_message, feed_message])
        if image_message:
            publish_messages.append(image_message)
        publish_messages.append(archive_csv_message)
    else:
        output_path = write_result(result, config.output_path)
        feed_output_path = write_feed(result, config.feed_output_path)
        annotated_image_output_path = None
        if annotated_image is not None:
            annotated_image_output_path = write_annotated_image(annotated_image, config.annotated_image_output_path)

        archive_path = append_archive_csv(result, config.archive_output_path)

        print(f"Wrote result to {output_path}")
        print(f"Wrote feed to {feed_output_path}")
        if annotated_image_output_path:
            print(f"Wrote annotated image to {annotated_image_output_path}")
        print(f"Appended archive CSV row(s) to {archive_path}")

    print(json_text.rstrip())
    for publish_message in publish_messages:
        print(publish_message)
    return 0 if result.status == "ok" and publish_ok else 1


def main() -> int:
    args = parse_args()
    config = build_config(args)
    if args.once:
        return run_once(config)

    while True:
        current_time = local_now(config)
        if not is_within_active_window(current_time, config):
            sleep_seconds = max(1, int(seconds_until_active_window(current_time, config)))
            print(f"Outside active window; sleeping {sleep_seconds} seconds")
            time.sleep(sleep_seconds)
            continue
        exit_code = run_once(config)
        if exit_code != 0:
            print(f"Run completed with exit code {exit_code}; waiting {config.loop_interval_seconds} seconds before retrying")
        time.sleep(max(1, config.loop_interval_seconds))


if __name__ == "__main__":
    raise SystemExit(main())
