from __future__ import annotations

import argparse
import base64
import csv
import json
import os
import re
import time
import traceback
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


try:
    SCRIPT_DIR = Path(__file__).resolve().parent
except NameError:
    # Databricks notebook cells do not define __file__.
    SCRIPT_DIR = Path.cwd().resolve()


DEFAULT_IPCAMLIVE_LANDING_PAGE_URL = "https://www.ipcamlive.com/willowcreektrail"
DEFAULT_WEBCAM_PAGE_URL = DEFAULT_IPCAMLIVE_LANDING_PAGE_URL
DEFAULT_FALLBACK_IMAGE_URLS: list[str] = []
DEFAULT_FALLBACK_IMAGE_URL = ""
IPCAMLIVE_STREAM_STATE_PATH = "/ajax/getcamerastreamstate.php"
DEFAULT_MODEL_PATH = "yolov8m.pt"
DEFAULT_BASE_OUTPUT_DIR = SCRIPT_DIR.parent / "IP3" / "WCT"
DEFAULT_ARCHIVE_OUTPUT_PATH = DEFAULT_BASE_OUTPUT_DIR / "archivefeed.csv"
DEFAULT_FEED_OUTPUT_PATH = SCRIPT_DIR / "wct_latest_feed.txt"
DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH = SCRIPT_DIR / "wct_latest_annotated.jpg"
DEFAULT_GITHUB_REPOSITORY = "VolpeUSDOT/Public-Lands-Computer-Vision"
DEFAULT_GITHUB_BRANCH = "main"
DEFAULT_GITHUB_JSON_PATH = "IP3/WCT/wct_vehicle_count_latest.json"
DEFAULT_GITHUB_FEED_PATH = "IP3/WCT/wct_latest_feed.txt"
DEFAULT_GITHUB_IMAGE_PATH = "IP3/WCT/wct_latest_annotated.jpg"
DEFAULT_GITHUB_ARCHIVE_CSV_PATH = "IP3/WCT/archivefeed.csv"
DEFAULT_GITHUB_STATE_PATH = "IP3/WCT/wct_vehicle_tracker_state.json"
DEFAULT_GITHUB_SPOT_STATE_PATH = "IP3/WCT/wct_parking_spot_state.json"
DEFAULT_DATABRICKS_PARKING_SPOTS_PATH = Path("/Workspace/Users/daniel.lang@dot.gov/Public-Lands-Computer-Vision/IP3/WCT/parking_spots_wct.json")
DEFAULT_LOCAL_PARKING_SPOTS_PATH = SCRIPT_DIR / "parking_spots_wct.json"
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
DEFAULT_OUTPUT_PATH = DEFAULT_BASE_OUTPUT_DIR / "wct_vehicle_count_latest.json"
DEFAULT_ENV_PATH = SCRIPT_DIR / ".env"
VEHICLE_CLASSES = [2, 3, 5, 7]
DEFAULT_LOOP_INTERVAL_SECONDS = 50
DEFAULT_TIMEZONE_NAME = "America/New_York"
DEFAULT_ACTIVE_START_HOUR = 9
DEFAULT_ACTIVE_END_HOUR = 21
TRACK_MATCH_IOU_THRESHOLD = 0.30
TRACK_MAX_MISSED_SECONDS = 180
RECOVERY_SPOT_IDS = {f"spot_{index}" for index in range(12, 18)}
DEFAULT_RECOVERY_CONFIDENCE = 0.25
DEFAULT_IMAGE_RETRY_ATTEMPTS = 2
DEFAULT_IMAGE_RETRY_DELAY_SECONDS = 15


@dataclass(frozen=True)
class Config:
    webcam_page_url: str = DEFAULT_WEBCAM_PAGE_URL
    fallback_image_url: str = DEFAULT_FALLBACK_IMAGE_URL
    model_path: str = DEFAULT_MODEL_PATH
    parking_spots_path: Path = DEFAULT_DATABRICKS_PARKING_SPOTS_PATH
    output_path: Path = DEFAULT_OUTPUT_PATH
    feed_output_path: Path = DEFAULT_FEED_OUTPUT_PATH
    annotated_image_output_path: Path = DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH
    github_repository: str = DEFAULT_GITHUB_REPOSITORY
    github_branch: str = DEFAULT_GITHUB_BRANCH
    github_json_path: str = DEFAULT_GITHUB_JSON_PATH
    github_feed_path: str = DEFAULT_GITHUB_FEED_PATH
    github_image_path: str = DEFAULT_GITHUB_IMAGE_PATH
    github_archive_csv_path: str = DEFAULT_GITHUB_ARCHIVE_CSV_PATH
    github_state_path: str = DEFAULT_GITHUB_STATE_PATH
    github_spot_state_path: str = DEFAULT_GITHUB_SPOT_STATE_PATH
    github_token: Optional[str] = None
    publish_to_github: bool = False
    confidence: float = 0.50
    recovery_confidence: float = DEFAULT_RECOVERY_CONFIDENCE
    iou: float = 0.45
    image_size: int = 1280
    user_agent: str = DEFAULT_USER_AGENT
    loop_interval_seconds: int = DEFAULT_LOOP_INTERVAL_SECONDS
    archive_output_path: Path = DEFAULT_ARCHIVE_OUTPUT_PATH
    timezone_name: str = DEFAULT_TIMEZONE_NAME
    active_start_hour: int = DEFAULT_ACTIVE_START_HOUR
    active_end_hour: int = DEFAULT_ACTIVE_END_HOUR
    image_retry_attempts: int = DEFAULT_IMAGE_RETRY_ATTEMPTS
    image_retry_delay_seconds: int = DEFAULT_IMAGE_RETRY_DELAY_SECONDS


@dataclass
class DetectionBox:
    class_id: int
    class_name: str
    confidence: float
    xyxy: list[float]
    track_id: Optional[int] = None
    dwell_seconds: Optional[int] = None
    spot_id: Optional[str] = None


@dataclass
class ArchiveRow:
    timestamp_utc: str
    spot_id: str
    spot_label: str
    status: str
    status_since_utc: str
    track_id: Optional[int]
    vehicle_type: Optional[str]
    confidence: Optional[float]
    dwell_seconds: Optional[int]
    bbox_x1: Optional[float]
    bbox_y1: Optional[float]
    bbox_x2: Optional[float]
    bbox_y2: Optional[float]


@dataclass
class RunResult:
    status: str
    timestamp_utc: str
    webcam_page_url: str
    image_url: Optional[str] = None
    model_path: str = DEFAULT_MODEL_PATH
    vehicle_count: int = 0
    detected_vehicle_count: int = 0
    occupied_spot_count: int = 0
    empty_spot_count: int = 0
    tracked_vehicle_count: int = 0
    average_vehicle_dwell_seconds: Optional[float] = None
    detections: list[DetectionBox] = field(default_factory=list)
    spots: list[dict[str, Any]] = field(default_factory=list)
    message: Optional[str] = None
    error: Optional[str] = None


@dataclass(frozen=True)
class ParkingSpotDefinition:
    spot_id: str
    label: str
    polygon: list[list[float]]


@dataclass
class SpotVehicle:
    track_id: int
    vehicle_type: str
    vehicle_bound: list[float]
    confidence: float
    dwell_seconds: int


@dataclass
class ParkingSpotState:
    spot_id: str
    label: str
    status: str
    status_since_utc: str
    vehicle: Optional[SpotVehicle] = None


@dataclass
class ParkingSpotResult:
    spot_id: str
    label: str
    polygon: list[list[float]]
    status: str
    status_since_utc: str
    vehicle: Optional[SpotVehicle] = None


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


def dwell_seconds_from_track(track: VehicleTrack, observed_ts: float) -> int:
    return max(0, int(observed_ts - track.first_seen_ts))


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
            detection.track_id = track.track_id
            detection.dwell_seconds = dwell_seconds_from_track(track, observed_ts)
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
            detection.track_id = self._next_track_id
            detection.dwell_seconds = 0
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
        # Only report dwell time for vehicles that are currently parked in-frame.
        durations = [max(0.0, observed_ts - track.first_seen_ts) for track in self._active_tracks.values()]
        if not durations:
            return None
        return sum(durations) / len(durations)

    def snapshot(self) -> dict[str, Any]:
        return {
            "next_track_id": self._next_track_id,
            "active_tracks": [asdict(track) for track in self._active_tracks.values()],
            "completed_dwell_seconds": self._completed_dwell_seconds,
        }

    @classmethod
    def from_snapshot(cls, payload: dict[str, Any]) -> "VehicleTracker":
        tracker = cls()
        try:
            tracker._next_track_id = int(payload.get("next_track_id", 1))
        except (TypeError, ValueError):
            tracker._next_track_id = 1

        active_tracks = payload.get("active_tracks") or []
        if isinstance(active_tracks, list):
            for raw_track in active_tracks:
                if not isinstance(raw_track, dict):
                    continue
                try:
                    tracker._active_tracks[int(raw_track.get("track_id"))] = VehicleTrack(
                        track_id=int(raw_track.get("track_id")),
                        class_id=int(raw_track.get("class_id", -1)),
                        class_name=str(raw_track.get("class_name", "unknown")),
                        first_seen_ts=float(raw_track.get("first_seen_ts", time.time())),
                        last_seen_ts=float(raw_track.get("last_seen_ts", time.time())),
                        confidence=float(raw_track.get("confidence", 0.0)),
                        xyxy=[float(v) for v in raw_track.get("xyxy", [0, 0, 0, 0])],
                        missed_count=int(raw_track.get("missed_count", 0)),
                    )
                except (TypeError, ValueError, KeyError):
                    continue

        completed = payload.get("completed_dwell_seconds") or []
        if isinstance(completed, list):
            tracker._completed_dwell_seconds = [float(value) for value in completed if isinstance(value, (int, float))]

        return tracker


def tracker_to_json_text(tracker: VehicleTracker) -> str:
    return json.dumps(tracker.snapshot(), indent=2) + "\n"


def tracker_from_json_text(text: str) -> VehicleTracker:
    if not text.strip():
        return VehicleTracker()
    payload = json.loads(text)
    if not isinstance(payload, dict):
        raise ValueError("Tracker state file must contain a JSON object")
    return VehicleTracker.from_snapshot(payload)


_VEHICLE_TRACKER = VehicleTracker()
_TRACKER_STATE_LOADED = False


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
    parking_spots_override = os.getenv("PARKING_SPOTS_PATH")
    parking_spots_candidates = [
        Path(parking_spots_override).expanduser() if parking_spots_override else None,
        DEFAULT_LOCAL_PARKING_SPOTS_PATH,
        DEFAULT_DATABRICKS_PARKING_SPOTS_PATH,
    ]

    def is_readable_file(path: Path) -> bool:
        try:
            return path.is_file() and path.stat().st_size >= 0
        except OSError:
            return False

    parking_spots_path = next((candidate for candidate in parking_spots_candidates if candidate is not None and is_readable_file(candidate)), None)
    if parking_spots_path is None:
        parking_spots_path = Path(parking_spots_override).expanduser() if parking_spots_override else DEFAULT_LOCAL_PARKING_SPOTS_PATH
    github_token = os.getenv("GITHUB_TOKEN") or os.getenv("GH_TOKEN")
    publish_to_github = os.getenv("PUBLISH_TO_GITHUB", "").strip().lower() in {"1", "true", "yes", "on"}
    return Config(
        webcam_page_url=os.getenv("WEBCAM_PAGE_URL", DEFAULT_WEBCAM_PAGE_URL),
        fallback_image_url=os.getenv("FALLBACK_IMAGE_URL", DEFAULT_FALLBACK_IMAGE_URL),
        model_path=os.getenv("MODEL_PATH", DEFAULT_MODEL_PATH),
        parking_spots_path=parking_spots_path,
        output_path=output_path,
        feed_output_path=feed_output_path,
        annotated_image_output_path=annotated_image_output_path,
        github_repository=os.getenv("GITHUB_REPOSITORY", DEFAULT_GITHUB_REPOSITORY),
        github_branch=os.getenv("GITHUB_BRANCH", DEFAULT_GITHUB_BRANCH),
        github_json_path=os.getenv("GITHUB_JSON_PATH", DEFAULT_GITHUB_JSON_PATH),
        github_feed_path=os.getenv("GITHUB_FEED_PATH", DEFAULT_GITHUB_FEED_PATH),
        github_image_path=os.getenv("GITHUB_IMAGE_PATH", DEFAULT_GITHUB_IMAGE_PATH),
        github_archive_csv_path=os.getenv("GITHUB_ARCHIVE_CSV_PATH", DEFAULT_GITHUB_ARCHIVE_CSV_PATH),
        github_spot_state_path=os.getenv("GITHUB_SPOT_STATE_PATH", DEFAULT_GITHUB_SPOT_STATE_PATH),
        github_token=github_token,
        publish_to_github=publish_to_github,
        confidence=float(os.getenv("YOLO_CONFIDENCE", "0.50")),
        recovery_confidence=float(os.getenv("YOLO_RECOVERY_CONFIDENCE", str(DEFAULT_RECOVERY_CONFIDENCE))),
        iou=float(os.getenv("YOLO_IOU", "0.45")),
        image_size=int(os.getenv("YOLO_IMAGE_SIZE", "1280")),
        user_agent=os.getenv("USER_AGENT", DEFAULT_USER_AGENT),
        loop_interval_seconds=int(os.getenv("LOOP_INTERVAL_SECONDS", str(DEFAULT_LOOP_INTERVAL_SECONDS))),
        archive_output_path=Path(os.getenv("ARCHIVE_OUTPUT_PATH", str(DEFAULT_ARCHIVE_OUTPUT_PATH))).expanduser(),
        timezone_name=os.getenv("TIMEZONE_NAME", DEFAULT_TIMEZONE_NAME),
        active_start_hour=int(os.getenv("ACTIVE_START_HOUR", str(DEFAULT_ACTIVE_START_HOUR))),
        active_end_hour=int(os.getenv("ACTIVE_END_HOUR", str(DEFAULT_ACTIVE_END_HOUR))),
        image_retry_attempts=int(os.getenv("IMAGE_RETRY_ATTEMPTS", str(DEFAULT_IMAGE_RETRY_ATTEMPTS))),
        image_retry_delay_seconds=int(os.getenv("IMAGE_RETRY_DELAY_SECONDS", str(DEFAULT_IMAGE_RETRY_DELAY_SECONDS))),
    )


def build_headers(user_agent: str) -> dict[str, str]:
    return {"User-Agent": user_agent}


def get_fallback_image_urls(config: Config) -> list[str]:
    urls = [config.fallback_image_url] if config.fallback_image_url else []
    for url in DEFAULT_FALLBACK_IMAGE_URLS:
        if url and url not in urls:
            urls.append(url)
    return urls


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


def extract_ipcamlive_og_image_url(page_text: str) -> Optional[str]:
    match = re.search(r'<meta\s+property=["\']og:image["\']\s+content=["\']([^"\']+)["\']', page_text, re.IGNORECASE)
    if match:
        return match.group(1)
    return None


def extract_ipcamlive_snapshot_candidates(page_text: str) -> list[str]:
    candidates: list[str] = []

    og_image_url = extract_ipcamlive_og_image_url(page_text)
    if og_image_url and og_image_url not in candidates:
        candidates.append(og_image_url)

    for url_match in re.finditer(r'https?://[^"\'\s>]+snapshot\.jpg', page_text, re.IGNORECASE):
        candidate = url_match.group(0)
        if candidate not in candidates:
            candidates.append(candidate)

    address_match = re.search(r"var\s+address\s*=\s*['\"](https?://[^'\"]+/)['\"]", page_text)
    streamid_match = re.search(r"var\s+streamid\s*=\s*['\"]([^'\"]+)['\"]", page_text)
    if address_match and streamid_match:
        candidate = build_ipcamlive_snapshot_url(address_match.group(1), streamid_match.group(1))
        if candidate not in candidates:
            candidates.append(candidate)

    return candidates


def is_url_reachable(url: str, headers: dict[str, str], timeout: int = 10) -> bool:
    try:
        response = requests.get(url, headers=headers, timeout=timeout)
        return response.status_code == 200 and bool(response.content)
    except requests.RequestException:
        return False


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
        candidate_url = build_ipcamlive_snapshot_url(address, stream_id)
        if is_url_reachable(candidate_url, headers):
            return candidate_url
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

    player_template_match = re.search(r"player/player\.php\?alias=\{0\}[^'\"\s>]*", page_text)
    if player_template_match:
        candidate_path = (
            player_template_match.group(0)
            .replace("{0}", alias)
            .replace("{1}", requests.utils.quote(token, safe=""))
            .replace("{2}", "")
        )
        candidate_url = f"https://www.ipcamlive.com/{candidate_path.lstrip('/')}"
        if is_url_reachable(candidate_url, headers):
            return candidate_url

    player_match = re.search(r"player/player\.php\?alias=[^'\"\s>]+", page_text)
    if player_match:
        candidate_path = player_match.group(0)
        if "{0}" in candidate_path or "{1}" in candidate_path or "{2}" in candidate_path:
            candidate_path = (
                candidate_path.replace("{0}", alias)
                .replace("{1}", requests.utils.quote(token, safe=""))
                .replace("{2}", "")
            )
        elif "alias=" in candidate_path:
            candidate_path = re.sub(r"alias=[^&'\"\s>]+", f"alias={alias}", candidate_path, count=1)

        candidate_url = candidate_path if candidate_path.startswith("http") else f"https://www.ipcamlive.com/{candidate_path.lstrip('/')}"
        if is_url_reachable(candidate_url, headers):
            return candidate_url

    candidate_url = f"https://www.ipcamlive.com/player/player.php?alias={alias}&autoplay=1&&token={requests.utils.quote(token, safe='')}"
    if is_url_reachable(candidate_url, headers):
        return candidate_url
    return None


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


def resolve_image_url(page_html: bytes, page_url: str, fallback_image_urls: list[str], headers: dict[str, str]) -> Optional[str]:
    page_text = page_html.decode("utf-8", errors="ignore")
    soup = BeautifulSoup(page_html, "html.parser")

    if page_url and "ipcamlive.com" in page_url:
        candidates = extract_ipcamlive_snapshot_candidates(page_text)
        alias = extract_ipcamlive_alias(page_text, page_url)
        if alias:
            try:
                snapshot_url = resolve_ipcamlive_snapshot_url(alias, headers)
                if snapshot_url:
                    candidates.append(snapshot_url)
            except requests.RequestException:
                pass

            try:
                player_url = resolve_ipcamlive_player_url(alias, headers)
                if player_url:
                    candidates.append(player_url)
            except requests.RequestException:
                pass

        iframe_match = re.search(r"<iframe[^>]+src=[\"']([^\"']*player/player\.php[^\"']*)[\"']", page_text, re.IGNORECASE)
        if iframe_match:
            iframe_src = iframe_match.group(1)
            iframe_alias = extract_ipcamlive_alias("", iframe_src)
            if iframe_alias:
                try:
                    snapshot_url = resolve_ipcamlive_snapshot_url(iframe_alias, headers)
                    if snapshot_url:
                        candidates.append(snapshot_url)
                except requests.RequestException:
                    pass

                try:
                    player_url = resolve_ipcamlive_player_url(iframe_alias, headers)
                    if player_url:
                        candidates.append(player_url)
                except requests.RequestException:
                    pass

        reachable = first_reachable_url(candidates, headers)
        if reachable:
            return reachable

        if alias:
            try:
                player_url = resolve_ipcamlive_player_url(alias, headers)
                if player_url:
                    return player_url
            except requests.RequestException:
                pass

        if iframe_match:
            iframe_alias = extract_ipcamlive_alias("", iframe_src)
            if iframe_alias:
                try:
                    player_url = resolve_ipcamlive_player_url(iframe_alias, headers)
                    if player_url:
                        return player_url
                except requests.RequestException:
                    pass

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
        candidate = build_ipcamlive_snapshot_url(address, streamid)
        if is_valid_ipcamlive_snapshot_url(candidate):
            return candidate

    for fallback_image_url in fallback_image_urls:
        if is_valid_ipcamlive_snapshot_url(fallback_image_url):
            return fallback_image_url
    return None


def resolve_current_webcam_image_url(page_url: str, fallback_image_urls: list[str], headers: dict[str, str]) -> tuple[Optional[str], Optional[bytes], Optional[str]]:
    """Fetch the webcam page and resolve the freshest image URL available right now."""

    page_response = fetch_bytes(page_url, headers=headers)
    image_url = resolve_image_url(page_response.content, page_url, fallback_image_urls, headers)
    return image_url, page_response.content, page_response.text


def resolve_ipcamlive_image_url_from_state(alias: str, headers: dict[str, str]) -> Optional[str]:
    state_url = f"https://www.ipcamlive.com{IPCAMLIVE_STREAM_STATE_PATH}"
    response = requests.get(state_url, headers=headers, params={"cameraalias": alias}, timeout=10)
    response.raise_for_status()
    data = parse_ipcamlive_stream_state(response)
    if not data:
        return None
    details = data.get("details") or {}
    address = details.get("address")
    stream_id = details.get("streamid")
    if address and stream_id:
        candidate = build_ipcamlive_snapshot_url(address, stream_id)
        return candidate if is_valid_ipcamlive_snapshot_url(candidate) else None
    return None


def resolve_ipcamlive_snapshot_from_page_or_state(page_text: str, alias: str, headers: dict[str, str]) -> Optional[str]:
    candidates = extract_ipcamlive_snapshot_candidates(page_text)
    if alias:
        try:
            state_url = resolve_ipcamlive_image_url_from_state(alias, headers)
            if state_url:
                candidates.append(state_url)
        except requests.RequestException:
            pass

    candidate = first_reachable_url(candidates, headers)
    return candidate


def is_valid_ipcamlive_snapshot_url(url: Optional[str]) -> bool:
    if not url:
        return False
    lower = url.lower()
    if "snapshot.jpg" not in lower:
        return False
    if "/streams//snapshot.jpg" in lower:
        return False
    return True


def first_reachable_url(urls: list[Optional[str]], headers: dict[str, str]) -> Optional[str]:
    for url in urls:
        if is_valid_ipcamlive_snapshot_url(url) and is_url_reachable(url, headers):
            return url
    return None


def extract_ipcamlive_snapshot_from_page(page_text: str) -> Optional[str]:
    candidates = extract_ipcamlive_snapshot_candidates(page_text)
    return candidates[0] if candidates else None


def load_parking_spots(path: Path) -> list[ParkingSpotDefinition]:
    if not path.exists():
        raise FileNotFoundError(f"Parking spots file not found: {path}")

    payload = json.loads(path.read_text(encoding="utf-8"))
    spots_raw = payload.get("spots", [])
    if not isinstance(spots_raw, list):
        raise ValueError("Parking spots JSON must contain a list under 'spots'")

    spots: list[ParkingSpotDefinition] = []
    for index, raw_spot in enumerate(spots_raw, start=1):
        if not isinstance(raw_spot, dict):
            continue
        polygon_raw = raw_spot.get("polygon", [])
        polygon: list[list[float]] = []
        for point in polygon_raw:
            if isinstance(point, (list, tuple)) and len(point) == 2:
                polygon.append([float(point[0]), float(point[1])])
        if len(polygon) < 3:
            continue
        spots.append(
            ParkingSpotDefinition(
                spot_id=str(raw_spot.get("spot_id") or f"spot_{index}"),
                label=str(raw_spot.get("label") or f"Spot {index}"),
                polygon=polygon,
            )
        )

    if not spots:
        raise ValueError(f"No valid parking spot polygons found in {path}")
    return spots


def point_in_polygon(point: tuple[float, float], polygon: list[list[float]]) -> bool:
    x, y = point
    inside = False
    n = len(polygon)
    if n < 3:
        return False

    j = n - 1
    for i in range(n):
        xi, yi = polygon[i]
        xj, yj = polygon[j]
        intersects = ((yi > y) != (yj > y)) and (x < (xj - xi) * (y - yi) / ((yj - yi) or 1e-9) + xi)
        if intersects:
            inside = not inside
        j = i
    return inside


def detection_anchor_point(detection: DetectionBox) -> tuple[float, float]:
    x1, y1, x2, y2 = detection.xyxy
    height = y2 - y1
    # Nudge the anchor slightly upward to stay inside angled spot polygons.
    y = y2 - max(6.0, 0.1 * height)
    return (x1 + x2) / 2.0, y


def polygon_bounds(polygon: list[list[float]]) -> list[float]:
    xs = [point[0] for point in polygon]
    ys = [point[1] for point in polygon]
    return [min(xs), min(ys), max(xs), max(ys)]


def bbox_iou(box_a: list[float], box_b: list[float]) -> float:
    ax1, ay1, ax2, ay2 = box_a
    bx1, by1, bx2, by2 = box_b
    inter_x1 = max(ax1, bx1)
    inter_y1 = max(ay1, by1)
    inter_x2 = min(ax2, bx2)
    inter_y2 = min(ay2, by2)
    inter_w = max(0.0, inter_x2 - inter_x1)
    inter_h = max(0.0, inter_y2 - inter_y1)
    inter_area = inter_w * inter_h
    if inter_area <= 0.0:
        return 0.0
    area_a = max(0.0, ax2 - ax1) * max(0.0, ay2 - ay1)
    area_b = max(0.0, bx2 - bx1) * max(0.0, by2 - by1)
    union = area_a + area_b - inter_area
    if union <= 0.0:
        return 0.0
    return inter_area / union


def spot_state_from_payload(payload: dict[str, Any], spot: ParkingSpotDefinition) -> ParkingSpotState:
    vehicle_raw = payload.get("vehicle")
    vehicle = None
    if isinstance(vehicle_raw, dict):
        try:
            vehicle = SpotVehicle(
                track_id=int(vehicle_raw["track_id"]),
                vehicle_type=str(vehicle_raw["vehicle_type"]),
                vehicle_bound=[float(v) for v in vehicle_raw.get("vehicle_bound", [])],
                confidence=float(vehicle_raw["confidence"]),
                dwell_seconds=int(vehicle_raw["dwell_seconds"]),
            )
        except (KeyError, TypeError, ValueError):
            vehicle = None

    return ParkingSpotState(
        spot_id=str(payload.get("spot_id", spot.spot_id)),
        label=str(payload.get("label", spot.label)),
        status=str(payload.get("status", "Empty")),
        status_since_utc=str(payload.get("status_since_utc", now_utc_iso())),
        vehicle=vehicle,
    )


def parking_spot_state_to_dict(state: ParkingSpotState) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "spot_id": state.spot_id,
        "label": state.label,
        "status": state.status,
        "status_since_utc": state.status_since_utc,
        "vehicle": asdict(state.vehicle) if state.vehicle else None,
    }
    return payload


def load_parking_spot_state_from_github(config: Config, spots: list[ParkingSpotDefinition]) -> dict[str, ParkingSpotState]:
    if not config.github_token:
        return {spot.spot_id: ParkingSpotState(spot_id=spot.spot_id, label=spot.label, status="Empty", status_since_utc=now_utc_iso()) for spot in spots}

    headers = get_github_headers(config.github_token)
    url = github_contents_api_url(config.github_repository, config.github_spot_state_path)
    try:
        response = requests.get(url, headers=headers, params={"ref": config.github_branch}, timeout=10)
        if response.status_code == 404:
            return {spot.spot_id: ParkingSpotState(spot_id=spot.spot_id, label=spot.label, status="Empty", status_since_utc=now_utc_iso()) for spot in spots}
        response.raise_for_status()
        payload = response.json()
        content = payload.get("content")
        encoding = payload.get("encoding")
        if not content or encoding != "base64":
            return {spot.spot_id: ParkingSpotState(spot_id=spot.spot_id, label=spot.label, status="Empty", status_since_utc=now_utc_iso()) for spot in spots}
        raw_text = base64.b64decode(content).decode("utf-8")
        state_payload = json.loads(raw_text)
        spot_entries = state_payload.get("spots", []) if isinstance(state_payload, dict) else []
        by_id: dict[str, ParkingSpotState] = {}
        for spot in spots:
            matching = next((entry for entry in spot_entries if isinstance(entry, dict) and entry.get("spot_id") == spot.spot_id), None)
            if matching:
                by_id[spot.spot_id] = spot_state_from_payload(matching, spot)
            else:
                by_id[spot.spot_id] = ParkingSpotState(spot_id=spot.spot_id, label=spot.label, status="Empty", status_since_utc=now_utc_iso())
        return by_id
    except Exception:
        return {spot.spot_id: ParkingSpotState(spot_id=spot.spot_id, label=spot.label, status="Empty", status_since_utc=now_utc_iso()) for spot in spots}


def publish_parking_spot_state_to_github(states: list[ParkingSpotState], config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped spot state publish: no GITHUB_TOKEN or GH_TOKEN set"

    payload = {
        "output_schema": "wct_parking_spot_state_v1",
        "timestamp_utc": now_utc_iso(),
        "spots": [parking_spot_state_to_dict(state) for state in states],
    }
    return publish_text_to_github(
        json.dumps(payload, indent=2) + "\n",
        config.github_repository,
        config.github_branch,
        config.github_spot_state_path,
        config.github_token,
        "Update WCT parking spot state",
    )


def match_detections_to_spots(
    detections: list[DetectionBox],
    spots: list[ParkingSpotDefinition],
    spot_states: dict[str, ParkingSpotState],
    observed_ts: str,
) -> list[ParkingSpotResult]:
    occupancy_by_spot: dict[str, DetectionBox] = {}
    fallback_matches: list[tuple[float, DetectionBox, ParkingSpotDefinition]] = []
    for detection in detections:
        anchor = detection_anchor_point(detection)
        matched = False
        for spot in spots:
            if point_in_polygon(anchor, spot.polygon):
                occupancy_by_spot[spot.spot_id] = detection
                detection.spot_id = spot.spot_id
                matched = True
                break
        if not matched:
            det_box = detection.xyxy
            for spot in spots:
                iou = bbox_iou(det_box, polygon_bounds(spot.polygon))
                if iou > 0.0:
                    fallback_matches.append((iou, detection, spot))

    fallback_matches.sort(reverse=True, key=lambda item: item[0])
    used_spots: set[str] = set(occupancy_by_spot.keys())
    used_detections: set[int] = {id(det) for det in occupancy_by_spot.values()}
    for iou, detection, spot in fallback_matches:
        if spot.spot_id in used_spots:
            continue
        if id(detection) in used_detections:
            continue
        occupancy_by_spot[spot.spot_id] = detection
        detection.spot_id = spot.spot_id
        used_spots.add(spot.spot_id)
        used_detections.add(id(detection))

    results: list[ParkingSpotResult] = []
    for spot in spots:
        current_state = spot_states.get(spot.spot_id) or ParkingSpotState(
            spot_id=spot.spot_id,
            label=spot.label,
            status="Empty",
            status_since_utc=observed_ts,
        )
        matched = occupancy_by_spot.get(spot.spot_id)
        if matched is None:
            if current_state.status != "Empty":
                current_state = ParkingSpotState(
                    spot_id=spot.spot_id,
                    label=spot.label,
                    status="Empty",
                    status_since_utc=observed_ts,
                    vehicle=None,
                )
            else:
                current_state = ParkingSpotState(
                    spot_id=spot.spot_id,
                    label=spot.label,
                    status="Empty",
                    status_since_utc=current_state.status_since_utc,
                    vehicle=None,
                )
        else:
            vehicle = SpotVehicle(
                track_id=matched.track_id if matched.track_id is not None else -1,
                vehicle_type=matched.class_name,
                vehicle_bound=[float(v) for v in matched.xyxy],
                confidence=matched.confidence,
                dwell_seconds=matched.dwell_seconds if matched.dwell_seconds is not None else 0,
            )
            if current_state.status != "Occupied" or current_state.vehicle is None or current_state.vehicle.track_id != vehicle.track_id:
                current_state = ParkingSpotState(
                    spot_id=spot.spot_id,
                    label=spot.label,
                    status="Occupied",
                    status_since_utc=observed_ts,
                    vehicle=vehicle,
                )
            else:
                current_state = ParkingSpotState(
                    spot_id=spot.spot_id,
                    label=spot.label,
                    status="Occupied",
                    status_since_utc=current_state.status_since_utc,
                    vehicle=vehicle,
                )
        spot_states[spot.spot_id] = current_state
        results.append(
            ParkingSpotResult(
                spot_id=spot.spot_id,
                label=spot.label,
                polygon=spot.polygon,
                status=current_state.status,
                status_since_utc=current_state.status_since_utc,
                vehicle=current_state.vehicle,
            )
        )
    return results


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
    if not response.content:
        raise ValueError(f"Downloaded empty image content from {image_url}")

    content_type = response.headers.get("Content-Type", "").lower()
    if content_type and not content_type.startswith("image/"):
        body_preview = response.text.strip().replace("\n", " ")[:200]
        raise ValueError(
            f"Resolved URL did not return an image (content-type={content_type!r}): {image_url}; body={body_preview!r}"
        )

    if response.content.lstrip().startswith((b"<", b"<!doctype", b"<?xml")):
        body_preview = response.text.strip().replace("\n", " ")[:200]
        raise ValueError(f"Resolved URL returned HTML/XML instead of an image: {image_url}; body={body_preview!r}")

    image_array = np.asarray(bytearray(response.content), dtype=np.uint8)
    img = cv2.imdecode(image_array, cv2.IMREAD_COLOR)
    if img is None:
        raise ValueError(f"OpenCV could not decode the downloaded image from {image_url}")
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


def format_detection_for_feed(detection: DetectionBox) -> str:
    dwell_part = f" dwell {detection.dwell_seconds} sec" if detection.dwell_seconds is not None else ""
    return (
        f"- {detection.class_name}"
        f" (class_id={detection.class_id}, confidence={detection.confidence:.3f})"
        f" xyxy={detection.xyxy}{dwell_part}"
    )


def spot_recovery_confidence(spot_id: str, config: Config) -> float:
    return config.recovery_confidence if spot_id in RECOVERY_SPOT_IDS else config.confidence


def filter_detections_for_spot_recovery(detections: list[DetectionBox], spots: list[ParkingSpotDefinition], config: Config) -> list[DetectionBox]:
    if not detections or not spots:
        return detections

    low_confidence_spots = {spot.spot_id for spot in spots if spot.spot_id in RECOVERY_SPOT_IDS}
    if not low_confidence_spots:
        return detections

    filtered: list[DetectionBox] = []
    for detection in detections:
        anchor = detection_anchor_point(detection)
        matched_spot_id = None
        for spot in spots:
            if point_in_polygon(anchor, spot.polygon):
                matched_spot_id = spot.spot_id
                break

        threshold = spot_recovery_confidence(matched_spot_id or "", config)
        if detection.confidence >= threshold:
            filtered.append(detection)

    return filtered


def is_transient_image_error(exc: Exception) -> bool:
    if isinstance(exc, ValueError):
        message = str(exc)
        return (
            "did not return an image" in message
            or "returned HTML/XML instead of an image" in message
            or "could not decode the downloaded image" in message
            or "Downloaded empty image content" in message
        )
    return False


def resolve_and_download_image(config: Config, headers: dict[str, str], fallback_image_urls: list[str]) -> tuple[np.ndarray, str]:
    last_exc: Exception | None = None
    attempts = max(1, config.image_retry_attempts)

    for attempt in range(1, attempts + 1):
        stage = "resolve webcam page image URL"
        try:
            image_url, page_html, page_text = resolve_current_webcam_image_url(config.webcam_page_url, fallback_image_urls, headers)

            stage = "resolve IPCamLive alias image URL"
            if not image_url and config.webcam_page_url and "ipcamlive.com" in config.webcam_page_url:
                alias = extract_ipcamlive_alias(page_text, config.webcam_page_url)
                if alias:
                    image_url = resolve_ipcamlive_snapshot_from_page_or_state(page_text, alias, headers) or resolve_ipcamlive_player_url(alias, headers)

            stage = "resolve IPCamLive snapshot fallback"
            if not image_url:
                image_url = resolve_ipcamlive_snapshot_from_page_or_state(page_text, "", headers)

            stage = "select static fallback image URL"
            if not image_url:
                image_url = next((url for url in fallback_image_urls if url), None)

            stage = "follow IPCamLive player iframe"
            if image_url and "/player/player.php" in image_url:
                # Some IPCamLive pages only expose an iframe player, so chase one more hop.
                player_response = fetch_bytes(image_url, headers=headers)
                player_snapshot = first_reachable_url(extract_ipcamlive_snapshot_candidates(player_response.text), headers)
                if player_snapshot:
                    image_url = player_snapshot
                else:
                    iframe_match = re.search(r"<iframe[^>]+src=[\"']([^\"']+)[\"']", player_response.text, re.IGNORECASE)
                    if iframe_match:
                        image_url = iframe_match.group(1)

            stage = "verify image URL reachability"
            if image_url and not is_url_reachable(image_url, headers):
                image_url = next((url for url in fallback_image_urls if is_url_reachable(url, headers)), None)

            stage = "download image"
            if not image_url:
                raise ValueError("Could not resolve an image URL from the webcam page")

            try:
                return download_image(image_url, headers=headers), image_url
            except Exception as exc:
                last_exc = exc
                fallback_url = next((url for url in fallback_image_urls if url != image_url and is_url_reachable(url, headers)), None)
                if fallback_url:
                    try:
                        return download_image(fallback_url, headers=headers), fallback_url
                    except Exception as fallback_exc:
                        last_exc = fallback_exc

                if attempt < attempts and is_transient_image_error(exc):
                    time.sleep(max(1, config.image_retry_delay_seconds))
                    continue
                raise
        except Exception as exc:
            last_exc = exc
            if attempt < attempts and is_transient_image_error(exc):
                time.sleep(max(1, config.image_retry_delay_seconds))
                continue
            raise ValueError(f"{exc} | stage={stage}") from exc

    assert last_exc is not None
    raise ValueError(f"{last_exc} | stage=download image") from last_exc


def run_detection(config: Config) -> RunResult:
    return run_detection_with_image(config)[0]


def run_detection_with_image(config: Config) -> tuple[RunResult, Optional[np.ndarray]]:
    headers = build_headers(config.user_agent)
    timestamp = now_utc_iso()
    spots = load_parking_spots(config.parking_spots_path)
    spot_states = load_spot_states(config, spots)
    fallback_image_urls = get_fallback_image_urls(config)

    try:
        img, image_url = resolve_and_download_image(config, headers, fallback_image_urls)

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
        detections = filter_detections_for_spot_recovery(detections, spots, config)
        tracker_summary = _VEHICLE_TRACKER.update(detections, datetime.now(timezone.utc).timestamp())
        spot_results = match_detections_to_spots(detections, spots, spot_states, timestamp)
        occupied_spot_count = sum(1 for spot in spot_results if spot.status == "Occupied")
        empty_spot_count = len(spot_results) - occupied_spot_count
        annotated_image = annotate_image(img, results)
        return (
            RunResult(
                status="ok",
                timestamp_utc=timestamp,
                webcam_page_url=config.webcam_page_url,
                image_url=image_url,
                model_path=config.model_path,
                vehicle_count=len(detections),
                detected_vehicle_count=len(detections),
                occupied_spot_count=occupied_spot_count,
                empty_spot_count=empty_spot_count,
                tracked_vehicle_count=tracker_summary.active_vehicle_count,
                average_vehicle_dwell_seconds=tracker_summary.average_vehicle_dwell_seconds,
                detections=detections,
                spots=[{
                    "spot_id": spot.spot_id,
                    "label": spot.label,
                    "polygon": spot.polygon,
                    "status": spot.status,
                    "status_since_utc": spot.status_since_utc,
                    "vehicle": asdict(spot.vehicle) if spot.vehicle else None,
                } for spot in spot_results],
                message=f"Detected {len(detections)} vehicles; occupied {occupied_spot_count} spots",
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
                error=f"{exc} | stage={stage}\n{traceback.format_exc()}",
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
    payload["spots"] = result.spots
    payload["output_schema"] = "wct_parking_spot_occupancy_v1"
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
        f"occupied_spot_count: {result.occupied_spot_count}",
        f"empty_spot_count: {result.empty_spot_count}",
    ]

    if result.message:
        lines.append(f"message: {result.message}")

    if result.error:
        lines.append(f"error: {result.error}")

    if result.spots:
        lines.append("spots:")
        for spot in result.spots:
            lines.append(f"- {spot['label']} ({spot['spot_id']}): {spot['status']} since {spot['status_since_utc']}")
            vehicle = spot.get("vehicle")
            if vehicle:
                lines.append(
                    f"  vehicle: {vehicle['vehicle_type']} (track_id={vehicle['track_id']}, confidence={vehicle['confidence']:.3f})"
                    f" bound={vehicle['vehicle_bound']} dwell {vehicle['dwell_seconds']} sec"
                )
    else:
        lines.append("spots: none")

    if result.detections:
        lines.append("detections:")
        for detection in result.detections:
            lines.append(format_detection_for_feed(detection))

    lines.append("output_schema: wct_parking_spot_occupancy_v1")
    return "\n".join(lines) + "\n"


def result_to_archive_rows(result: RunResult) -> list[ArchiveRow]:
    rows: list[ArchiveRow] = []
    if result.spots:
        for spot in result.spots:
            vehicle = spot.get("vehicle") if isinstance(spot, dict) else None
            if vehicle:
                rows.append(
                    ArchiveRow(
                        timestamp_utc=result.timestamp_utc,
                        spot_id=str(spot.get("spot_id", "")),
                        spot_label=str(spot.get("label", "")),
                        status=str(spot.get("status", "Occupied")),
                        status_since_utc=str(spot.get("status_since_utc", result.timestamp_utc)),
                        track_id=int(vehicle.get("track_id")) if vehicle.get("track_id") is not None else None,
                        vehicle_type=str(vehicle.get("vehicle_type", "unknown")),
                        confidence=float(vehicle.get("confidence", 0.0)),
                        dwell_seconds=int(vehicle.get("dwell_seconds")) if vehicle.get("dwell_seconds") is not None else None,
                        bbox_x1=float(vehicle.get("vehicle_bound", [0.0, 0.0, 0.0, 0.0])[0]),
                        bbox_y1=float(vehicle.get("vehicle_bound", [0.0, 0.0, 0.0, 0.0])[1]),
                        bbox_x2=float(vehicle.get("vehicle_bound", [0.0, 0.0, 0.0, 0.0])[2]),
                        bbox_y2=float(vehicle.get("vehicle_bound", [0.0, 0.0, 0.0, 0.0])[3]),
                    )
                )
            else:
                rows.append(
                    ArchiveRow(
                        timestamp_utc=result.timestamp_utc,
                        spot_id=str(spot.get("spot_id", "")),
                        spot_label=str(spot.get("label", "")),
                        status=str(spot.get("status", "Empty")),
                        status_since_utc=str(spot.get("status_since_utc", result.timestamp_utc)),
                        track_id=None,
                        vehicle_type=None,
                        confidence=None,
                        dwell_seconds=None,
                        bbox_x1=None,
                        bbox_y1=None,
                        bbox_x2=None,
                        bbox_y2=None,
                    )
                )
    else:
        rows.append(
            ArchiveRow(
                timestamp_utc=result.timestamp_utc,
                spot_id="",
                spot_label="",
                status="Empty",
                status_since_utc=result.timestamp_utc,
                track_id=None,
                vehicle_type=None,
                confidence=None,
                dwell_seconds=None,
                bbox_x1=None,
                bbox_y1=None,
                bbox_x2=None,
                bbox_y2=None,
            )
        )
    return rows


def result_to_archive_csv_rows(result: RunResult) -> list[dict[str, Any]]:
    return [archive_row_to_dict(row) for row in result_to_archive_rows(result)]


def archive_csv_header() -> list[str]:
    return [
        "timestamp_utc",
        "spot_id",
        "spot_label",
        "status",
        "status_since_utc",
        "track_id",
        "vehicle_type",
        "confidence",
        "dwell_seconds",
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
    writer = csv.DictWriter(buffer, fieldnames=archive_csv_header(), extrasaction="ignore")
    writer.writeheader()
    for row in rows:
        writer.writerow({key: getattr(row, key) for key in archive_csv_header()})
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
    fieldnames = archive_csv_header()
    writer = csv.DictWriter(buffer, fieldnames=fieldnames, extrasaction="ignore")
    writer.writeheader()
    for row in rows:
        writer.writerow({key: row.get(key) for key in fieldnames})
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


def load_tracker_state_from_github(config: Config) -> bool:
    global _VEHICLE_TRACKER, _TRACKER_STATE_LOADED

    if _TRACKER_STATE_LOADED:
        return True
    if not config.github_token:
        return False

    headers = get_github_headers(config.github_token)
    url = github_contents_api_url(config.github_repository, config.github_state_path)
    try:
        response = requests.get(url, headers=headers, params={"ref": config.github_branch}, timeout=10)
        if response.status_code == 404:
            _TRACKER_STATE_LOADED = True
            return False
        response.raise_for_status()
        payload = response.json()
        content = payload.get("content")
        encoding = payload.get("encoding")
        if not content or encoding != "base64":
            _TRACKER_STATE_LOADED = True
            return False
        raw_text = base64.b64decode(content).decode("utf-8")
        _VEHICLE_TRACKER = tracker_from_json_text(raw_text)
        _TRACKER_STATE_LOADED = True
        return True
    except requests.RequestException:
        return False
    except (ValueError, json.JSONDecodeError):
        return False


def load_spot_states(config: Config, spots: list[ParkingSpotDefinition]) -> dict[str, ParkingSpotState]:
    return load_parking_spot_state_from_github(config, spots)


def spot_results_to_state_objects(result: RunResult) -> list[ParkingSpotState]:
    states: list[ParkingSpotState] = []
    for spot in result.spots:
        vehicle_raw = spot.get("vehicle") if isinstance(spot, dict) else None
        vehicle = None
        if isinstance(vehicle_raw, dict):
            try:
                vehicle = SpotVehicle(
                    track_id=int(vehicle_raw.get("track_id", -1)),
                    vehicle_type=str(vehicle_raw.get("vehicle_type", "unknown")),
                    vehicle_bound=[float(v) for v in vehicle_raw.get("vehicle_bound", [])],
                    confidence=float(vehicle_raw.get("confidence", 0.0)),
                    dwell_seconds=int(vehicle_raw.get("dwell_seconds", 0)),
                )
            except (TypeError, ValueError):
                vehicle = None
        states.append(
            ParkingSpotState(
                spot_id=str(spot.get("spot_id", "")),
                label=str(spot.get("label", "")),
                status=str(spot.get("status", "Empty")),
                status_since_utc=str(spot.get("status_since_utc", result.timestamp_utc)),
                vehicle=vehicle,
            )
        )
    return states


def publish_tracker_state_to_github(config: Config) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped tracker state publish: no GITHUB_TOKEN or GH_TOKEN set"

    state_text = tracker_to_json_text(_VEHICLE_TRACKER)
    return publish_text_to_github(
        state_text,
        config.github_repository,
        config.github_branch,
        config.github_state_path,
        config.github_token,
        "Update WCT vehicle tracker state",
    )


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Count vehicles in the Willow Creek Trail Parking Lot webcam image")
    parser.add_argument("--output", type=Path, default=None, help="Override output JSON path")
    parser.add_argument("--feed-output", type=Path, default=None, help="Override local feed text output path")
    parser.add_argument("--archive-output-path", type=Path, default=None, help="Override local archive CSV output path")
    parser.add_argument("--model", default=None, help="Override YOLO model path")
    parser.add_argument("--confidence", type=float, default=None, help="Override YOLO confidence threshold")
    parser.add_argument("--recovery-confidence", type=float, default=None, help="Override YOLO confidence threshold used for recovery spots")
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
        parking_spots_path=config.parking_spots_path,
        output_path=args.output or config.output_path,
        feed_output_path=args.feed_output or config.feed_output_path,
        github_repository=config.github_repository,
        github_branch=config.github_branch,
        github_json_path=config.github_json_path,
        github_feed_path=config.github_feed_path,
        github_image_path=config.github_image_path,
        github_archive_csv_path=config.github_archive_csv_path,
        github_state_path=config.github_state_path,
        github_spot_state_path=config.github_spot_state_path,
        github_token=config.github_token,
        publish_to_github=config.publish_to_github,
        confidence=args.confidence if args.confidence is not None else config.confidence,
        recovery_confidence=args.recovery_confidence if args.recovery_confidence is not None else config.recovery_confidence,
        iou=args.iou if args.iou is not None else config.iou,
        image_size=args.imgsz if args.imgsz is not None else config.image_size,
        user_agent=config.user_agent,
        loop_interval_seconds=args.interval if args.interval is not None else config.loop_interval_seconds,
        archive_output_path=args.archive_output_path or config.archive_output_path,
        timezone_name=args.timezone or config.timezone_name,
        active_start_hour=args.active_start_hour if args.active_start_hour is not None else config.active_start_hour,
        active_end_hour=args.active_end_hour if args.active_end_hour is not None else config.active_end_hour,
    )


def first_spot_ids(prefix: str, start: int, end: int) -> set[str]:
    return {f"{prefix}_{index}" for index in range(start, end + 1)}


def get_recovery_spot_labels() -> set[str]:
    return RECOVERY_SPOT_IDS


def spot_state_to_feed_entry(spot: ParkingSpotResult) -> str:
    lines = [f"- {spot.label} ({spot.spot_id}): {spot.status} since {spot.status_since_utc}"]
    if spot.vehicle:
        lines.append(
            f"  vehicle: {spot.vehicle.vehicle_type} (track_id={spot.vehicle.track_id}, confidence={spot.vehicle.confidence:.3f})"
            f" bound={spot.vehicle.vehicle_bound} dwell {spot.vehicle.dwell_seconds} sec"
        )
    return "\n".join(lines)


def run_once(config: Config) -> int:
    load_tracker_state_from_github(config)
    result, annotated_image = run_detection_with_image(config)
    json_text = result_to_json_text(result)
    feed_text = result_to_feed_text(result)

    publish_messages: list[str] = []
    publish_ok = True
    if config.publish_to_github:
        if result.status == "ok":
            json_published, json_message = publish_json_to_github(json_text, config)
            feed_published, feed_message = publish_feed_to_github(feed_text, config)
            image_published = True
            image_message = None
            if annotated_image is not None:
                image_published, image_message = publish_annotated_image_to_github(annotated_image, config)

            archive_csv_text = archive_rows_to_csv_text(result_to_archive_rows(result))
            archive_csv_published, archive_csv_message = publish_archive_csv_to_github(archive_csv_text, config)
            tracker_state_published, tracker_state_message = publish_tracker_state_to_github(config)
            spot_state_published, spot_state_message = publish_parking_spot_state_to_github(spot_results_to_state_objects(result), config)
            publish_ok = (
                json_published
                and feed_published
                and image_published
                and archive_csv_published
                and tracker_state_published
                and spot_state_published
            )
            publish_messages.extend([json_message, feed_message])
            if image_message:
                publish_messages.append(image_message)
            publish_messages.append(archive_csv_message)
            publish_messages.append(tracker_state_message)
            publish_messages.append(spot_state_message)
        else:
            publish_ok = False
            publish_messages.append("Skipped GitHub publish: run status is error")
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
