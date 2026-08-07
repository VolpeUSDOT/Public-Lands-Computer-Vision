from __future__ import annotations

import argparse
import json
import os
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from functools import lru_cache
from pathlib import Path
from typing import Optional
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit

os.environ.pop("OPENSSL_FORCE_FIPS_MODE", None)

import cv2
import numpy as np
import requests
from bs4 import BeautifulSoup
from ultralytics import YOLO


try:
    SCRIPT_DIR = Path(__file__).resolve().parent
except NameError:
    SCRIPT_DIR = Path.cwd().resolve()


DEFAULT_WEBCAM_PAGE_URL = "https://www.nps.gov/media/webcam/view.htm?id=325AE6AF-BAEB-F65D-EF3D638BF683E78E&r=/glac/learn/photosmultimedia/webcams.htm"
DEFAULT_WEBCAM_FALLBACK_PAGE_URL = "https://www.nps.gov/glac/learn/photosmultimedia/webcams.htm"
DEFAULT_FALLBACK_IMAGE_URL = ""
DEFAULT_MODEL_PATH = "yolov8m.pt"
DEFAULT_OUTPUT_PATH = SCRIPT_DIR / "glacier_latest_feed.json"
DEFAULT_FEED_OUTPUT_PATH = SCRIPT_DIR / "glacier_latest_feed.txt"
DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH = SCRIPT_DIR / "glacier_latest_annotated.jpg"
DEFAULT_HISTORY_OUTPUT_PATH = SCRIPT_DIR / "glacier_latest_history.jsonl"
DEFAULT_TRACKING_STATE_OUTPUT_PATH = SCRIPT_DIR / "glacier_latest_tracking_state.json"
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
VEHICLE_CLASSES = [2, 3, 5, 7]
DEFAULT_CAMERA_NAME = "logan_pass"
ALL_CAMERA_NAME = "all"
DEFAULT_LOGAN_PASS_PARKING_SPOTS_TOTAL = 100
WEST_ENTRANCE_COUNTING_LINE_RATIO = 0.58
WEST_ENTRANCE_MOTION_THRESHOLD_PX = 8.0
TRACK_IOU_MATCH_THRESHOLD = 0.30

GLACIER_CAMERAS: dict[str, dict[str, str]] = {
    "logan_pass": {
        "label": "Logan Pass Parking Lot",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=325AE6AF-BAEB-F65D-EF3D638BF683E78E&r=/glac/learn/photosmultimedia/webcams.htm",
        "parking_spots_total": str(DEFAULT_LOGAN_PASS_PARKING_SPOTS_TOTAL),
        "tracking_mode": "dwell",
    },
    "west_entrance": {
        "label": "West Entrance",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=33478DF3-1DD8-B71B-0B8C97DB0A03B0F7",
        "lane_split_ratio": "0.50",
        "tracking_mode": "directional",
    },
    "apgar_village": {
        "label": "Apgar Village",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=81B4692D-1DD8-B71B-0B9AE4B7C186B022",
        "tracking_mode": "detect",
    },
}


@dataclass(frozen=True)
class Config:
    camera_name: str = DEFAULT_CAMERA_NAME
    camera_label: str = "Logan Pass Parking Lot"
    webcam_page_url: str = DEFAULT_WEBCAM_PAGE_URL
    fallback_webcam_page_url: str = DEFAULT_WEBCAM_FALLBACK_PAGE_URL
    fallback_image_url: str = DEFAULT_FALLBACK_IMAGE_URL
    model_path: str = DEFAULT_MODEL_PATH
    output_path: Path = DEFAULT_OUTPUT_PATH
    feed_output_path: Path = DEFAULT_FEED_OUTPUT_PATH
    annotated_image_output_path: Path = DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH
    history_output_path: Path = DEFAULT_HISTORY_OUTPUT_PATH
    tracking_state_output_path: Path = DEFAULT_TRACKING_STATE_OUTPUT_PATH
    confidence: float = 0.15
    iou: float = 0.45
    image_size: int = 1280
    user_agent: str = DEFAULT_USER_AGENT


@dataclass
class DetectionBox:
    class_id: int
    class_name: str
    confidence: float
    xyxy: list[float]
    track_id: Optional[int] = None


@dataclass
class RunResult:
    status: str
    timestamp_utc: str
    camera_name: str
    camera_label: str
    webcam_page_url: str
    image_url: Optional[str] = None
    model_path: str = DEFAULT_MODEL_PATH
    vehicle_count: int = 0
    detected_vehicle_count: int = 0
    current_queue: int = 0
    current_queue_by_lane: Optional[dict[str, int]] = None
    peak_queue_today: Optional[int] = None
    parking_spots_total: Optional[int] = None
    parking_spots_available: Optional[int] = None
    incoming_count: Optional[int] = None
    exiting_count: Optional[int] = None
    exits: Optional[int] = None
    average_dwell_time_minutes: Optional[float] = None
    detections: list[DetectionBox] = field(default_factory=list)
    message: Optional[str] = None
    error: Optional[str] = None


def now_utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def load_env_file(env_path: Path = SCRIPT_DIR / ".env") -> None:
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


def load_config() -> Config:
    load_env_file()
    return Config(
        camera_name=os.getenv("CAMERA_NAME", DEFAULT_CAMERA_NAME),
        camera_label=os.getenv("CAMERA_LABEL", "Logan Pass Parking Lot"),
        webcam_page_url=os.getenv("WEBCAM_PAGE_URL", DEFAULT_WEBCAM_PAGE_URL),
        fallback_webcam_page_url=os.getenv("FALLBACK_WEBCAM_PAGE_URL", DEFAULT_WEBCAM_FALLBACK_PAGE_URL),
        fallback_image_url=os.getenv("FALLBACK_IMAGE_URL", DEFAULT_FALLBACK_IMAGE_URL),
        model_path=os.getenv("MODEL_PATH", DEFAULT_MODEL_PATH),
        output_path=Path(os.getenv("OUTPUT_PATH", str(DEFAULT_OUTPUT_PATH))).expanduser(),
        feed_output_path=Path(os.getenv("FEED_OUTPUT_PATH", str(DEFAULT_FEED_OUTPUT_PATH))).expanduser(),
        annotated_image_output_path=Path(os.getenv("ANNOTATED_IMAGE_OUTPUT_PATH", str(DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH))).expanduser(),
        history_output_path=Path(os.getenv("HISTORY_OUTPUT_PATH", str(DEFAULT_HISTORY_OUTPUT_PATH))).expanduser(),
        tracking_state_output_path=Path(os.getenv("TRACKING_STATE_OUTPUT_PATH", str(DEFAULT_TRACKING_STATE_OUTPUT_PATH))).expanduser(),
        confidence=float(os.getenv("YOLO_CONFIDENCE", "0.15")),
        iou=float(os.getenv("YOLO_IOU", "0.45")),
        image_size=int(os.getenv("YOLO_IMAGE_SIZE", "1280")),
        user_agent=os.getenv("USER_AGENT", DEFAULT_USER_AGENT),
    )


def build_headers(user_agent: str, referer: Optional[str] = None) -> dict[str, str]:
    headers = {
        "User-Agent": user_agent,
        "Cache-Control": "no-cache",
        "Pragma": "no-cache",
    }
    if referer:
        headers["Referer"] = referer
    return headers


def cache_busted_url(url: str) -> str:
    parsed = urlsplit(url)
    query = dict(parse_qsl(parsed.query, keep_blank_values=True))
    query["_"] = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S%f")
    return urlunsplit((parsed.scheme, parsed.netloc, parsed.path, urlencode(query), parsed.fragment))


def fetch_page(url: str, headers: dict[str, str]) -> requests.Response:
    response = requests.get(url, headers=headers, timeout=20)
    response.raise_for_status()
    return response


def resolve_image_url(page_url: str, fallback_page_url: str, fallback_image_url: str, headers: dict[str, str]) -> str:
    best_non_nps_url: Optional[str] = None
    for candidate_url in (page_url, fallback_page_url):
        try:
            response = fetch_page(candidate_url, headers)
            soup = BeautifulSoup(response.text, "html.parser")
            for img in soup.find_all("img"):
                src = (img.get("src") or "").strip()
                if not src:
                    continue
                lowered = src.lower()
                if ".jpg" in lowered or ".jpeg" in lowered or "webcam" in lowered:
                    if "glacier.org" in lowered:
                        if best_non_nps_url is None:
                            best_non_nps_url = src if src.startswith("http") else "https://glacier.org" + src
                        continue
                    if src.startswith("/"):
                        return "https://www.nps.gov" + src
                    return src
        except requests.RequestException:
            continue
    if best_non_nps_url:
        return best_non_nps_url
    if fallback_image_url:
        return fallback_image_url
    raise RuntimeError("Could not resolve a Glacier webcam image URL from the page")


def download_image(image_url: str, headers: dict[str, str], referer: Optional[str] = None) -> np.ndarray:
    request_headers = dict(headers)
    if referer:
        request_headers["Referer"] = referer
    response = requests.get(cache_busted_url(image_url), headers=request_headers, timeout=20)
    response.raise_for_status()
    image_array = np.asarray(bytearray(response.content), dtype=np.uint8)
    img = cv2.imdecode(image_array, cv2.IMREAD_COLOR)
    if img is None:
        raise ValueError("OpenCV could not decode the downloaded image")
    return img


def summarize_queue(detections: list[DetectionBox], image_shape: tuple[int, ...], lane_split_ratio: Optional[float] = None) -> tuple[int, Optional[dict[str, int]]]:
    current_queue = len(detections)
    if lane_split_ratio is None or len(image_shape) < 2:
        return current_queue, None

    width = float(image_shape[1])
    split_x = width * lane_split_ratio
    lane_counts = {"left_lane": 0, "right_lane": 0}
    for detection in detections:
        x1, _, x2, _ = detection.xyxy
        center_x = (x1 + x2) / 2.0
        if center_x < split_x:
            lane_counts["left_lane"] += 1
        else:
            lane_counts["right_lane"] += 1
    return current_queue, lane_counts


def camera_history_path(base_path: Path, camera_name: str, default_camera_name: str = DEFAULT_CAMERA_NAME) -> Path:
    base = camera_output_path(base_path, camera_name, default_camera_name)
    stem = base.stem
    if stem.endswith("_feed"):
        stem = stem[:-5]
    return base.with_name(f"{stem}.jsonl")


def camera_tracking_state_path(base_path: Path, camera_name: str, default_camera_name: str = DEFAULT_CAMERA_NAME) -> Path:
    base = camera_output_path(base_path, camera_name, default_camera_name)
    stem = base.stem
    if stem.endswith("_tracking_state"):
        return base
    if stem.endswith("_latest"):
        stem = stem.replace("_latest", "")
    return base.with_name(f"{stem}_tracking_state.json")


def parse_utc_timestamp(value: str) -> datetime:
    if value.endswith("Z"):
        value = value[:-1] + "+00:00"
    return datetime.fromisoformat(value)


def format_minutes(value: float) -> float:
    return round(value, 2)


def bbox_center(xyxy: list[float]) -> tuple[float, float]:
    x1, y1, x2, y2 = xyxy
    return ((x1 + x2) / 2.0, (y1 + y2) / 2.0)


def west_entrance_motion_vector(current_xyxy: list[float], previous_xyxy: Optional[list[float]]) -> tuple[float, float]:
    current_center_x, current_center_y = bbox_center(current_xyxy)
    if previous_xyxy is None:
        return 0.0, 0.0
    previous_center_x, previous_center_y = bbox_center(previous_xyxy)
    return current_center_x - previous_center_x, current_center_y - previous_center_y


def bbox_iou(a: list[float], b: list[float]) -> float:
    ax1, ay1, ax2, ay2 = a
    bx1, by1, bx2, by2 = b
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


def load_tracking_state(path: Path) -> dict[str, object]:
    default_state: dict[str, object] = {
        "next_stable_id": 1,
        "tracker_to_stable": {},
        "tracks": {},
    }
    if not path.exists():
        return default_state
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return default_state
    if not isinstance(data, dict):
        return default_state
    data.setdefault("next_stable_id", 1)
    data.setdefault("tracker_to_stable", {})
    data.setdefault("tracks", {})
    return data


def save_tracking_state(path: Path, state: dict[str, object]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(state, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return path


def collect_vehicle_detections(results) -> list[DetectionBox]:
    detections: list[DetectionBox] = []
    for result in results:
        names = result.names
        boxes = result.boxes
        if boxes is None:
            continue
        for box in boxes:
            class_id = int(box.cls.item())
            if class_id not in VEHICLE_CLASSES:
                continue
            xyxy = [float(v) for v in box.xyxy[0].tolist()]
            confidence = float(box.conf.item())
            track_id = None
            if getattr(box, "id", None) is not None:
                try:
                    track_id = int(box.id.item())
                except Exception:
                    track_id = None
            detections.append(
                DetectionBox(
                    class_id=class_id,
                    class_name=str(names.get(class_id, class_id)),
                    confidence=confidence,
                    xyxy=xyxy,
                    track_id=track_id,
                )
            )
    return detections


def resolve_stable_track_id(
    state: dict[str, object],
    detection: DetectionBox,
    camera_name: str,
    timestamp_utc: str,
) -> str:
    tracker_to_stable = state.setdefault("tracker_to_stable", {})
    tracks = state.setdefault("tracks", {})
    raw_tracker_id = str(detection.track_id) if detection.track_id is not None else None

    if raw_tracker_id and raw_tracker_id in tracker_to_stable:
        return str(tracker_to_stable[raw_tracker_id])

    best_stable_id: Optional[str] = None
    best_iou = 0.0
    for stable_id, track in tracks.items():
        if not isinstance(track, dict):
            continue
        if track.get("camera_name") != camera_name:
            continue
        if track.get("class_id") != detection.class_id:
            continue
        if track.get("active") is False:
            continue
        previous_xyxy = track.get("last_xyxy")
        if not isinstance(previous_xyxy, list) or len(previous_xyxy) != 4:
            continue
        score = bbox_iou(previous_xyxy, detection.xyxy)
        if score > best_iou:
            best_iou = score
            best_stable_id = str(stable_id)

    if best_stable_id is not None and best_iou >= TRACK_IOU_MATCH_THRESHOLD:
        if raw_tracker_id:
            tracker_to_stable[raw_tracker_id] = best_stable_id
        return best_stable_id

    next_stable_id = int(state.get("next_stable_id", 1))
    stable_id = f"vehicle_{next_stable_id}"
    state["next_stable_id"] = next_stable_id + 1
    if raw_tracker_id:
        tracker_to_stable[raw_tracker_id] = stable_id
    return stable_id


def upsert_track_record(
    state: dict[str, object],
    stable_id: str,
    detection: DetectionBox,
    camera_name: str,
    timestamp_utc: str,
    motion_class: Optional[str] = None,
    motion_dx: Optional[float] = None,
    motion_dy: Optional[float] = None,
) -> None:
    tracks = state.setdefault("tracks", {})
    track = tracks.get(stable_id)
    if not isinstance(track, dict):
        track = {}
        tracks[stable_id] = track
    if "first_seen" not in track:
        track["first_seen"] = timestamp_utc
    track["camera_name"] = camera_name
    track["class_id"] = detection.class_id
    track["class_name"] = detection.class_name
    track["last_seen"] = timestamp_utc
    track["active"] = True
    track["last_xyxy"] = detection.xyxy
    track["last_track_id"] = detection.track_id
    if motion_class is not None:
        track["motion_class"] = motion_class
    if motion_dx is not None:
        track["motion_dx"] = motion_dx
    if motion_dy is not None:
        track["motion_dy"] = motion_dy


def finalize_missing_tracks(state: dict[str, object], camera_name: str, active_stable_ids: set[str], timestamp_utc: str) -> None:
    tracks = state.setdefault("tracks", {})
    for stable_id, track in tracks.items():
        if not isinstance(track, dict):
            continue
        if track.get("camera_name") != camera_name:
            continue
        if stable_id in active_stable_ids:
            continue
        if track.get("active") is not False:
            track["active"] = False
            track["disappeared_at"] = timestamp_utc


def summarize_logan_pass_dwell_time(
    detections: list[DetectionBox],
    state: dict[str, object],
    camera_name: str,
    timestamp_utc: str,
) -> tuple[int, Optional[float], int]:
    active_stable_ids: set[str] = set()
    for detection in detections:
        stable_id = resolve_stable_track_id(state, detection, camera_name, timestamp_utc)
        active_stable_ids.add(stable_id)
        upsert_track_record(state, stable_id, detection, camera_name, timestamp_utc)

    finalize_missing_tracks(state, camera_name, active_stable_ids, timestamp_utc)

    now_dt = parse_utc_timestamp(timestamp_utc)
    active_tracks = []
    tracks = state.get("tracks", {})
    if isinstance(tracks, dict):
        for stable_id, track in tracks.items():
            if not isinstance(track, dict):
                continue
            if track.get("camera_name") != camera_name or track.get("active") is not True:
                continue
            first_seen = track.get("first_seen")
            if not isinstance(first_seen, str):
                continue
            try:
                first_seen_dt = parse_utc_timestamp(first_seen)
            except ValueError:
                continue
            dwell_minutes = max((now_dt - first_seen_dt).total_seconds() / 60.0, 0.0)
            active_tracks.append(dwell_minutes)

    average_dwell_time_minutes = format_minutes(sum(active_tracks) / len(active_tracks)) if active_tracks else None
    return len(active_stable_ids), average_dwell_time_minutes, len(active_tracks)


def classify_west_entrance_motion(
    current_xyxy: list[float],
    previous_xyxy: Optional[list[float]],
    line_x: float,
    motion_dx: Optional[float] = None,
    motion_dy: Optional[float] = None,
) -> str:
    current_center_x, _ = bbox_center(current_xyxy)
    if motion_dx is None or motion_dy is None:
        motion_dx, motion_dy = west_entrance_motion_vector(current_xyxy, previous_xyxy)

    # The entrance road flows leftward into the park and rightward out of it in this frame.
    # The virtual line helps stabilize the direction call when the tracker only moves a few pixels.
    if previous_xyxy is None:
        return "incoming" if current_center_x >= line_x else "exiting"

    previous_center_x, _ = bbox_center(previous_xyxy)

    if previous_center_x >= line_x and current_center_x < line_x and motion_dx < 0:
        return "incoming"
    if previous_center_x <= line_x and current_center_x > line_x and motion_dx > 0:
        return "exiting"
    if abs(motion_dx) >= WEST_ENTRANCE_MOTION_THRESHOLD_PX and abs(motion_dx) >= abs(motion_dy):
        return "incoming" if motion_dx < 0 else "exiting"
    return "incoming" if current_center_x >= line_x else "exiting"


def summarize_west_entrance_counts(
    detections: list[DetectionBox],
    state: dict[str, object],
    camera_name: str,
    timestamp_utc: str,
    image_width: float,
) -> tuple[int, int, dict[str, int]]:
    line_x = image_width * WEST_ENTRANCE_COUNTING_LINE_RATIO
    active_stable_ids: set[str] = set()
    incoming_count = 0
    exiting_count = 0
    lane_counts = {"left_lane": 0, "right_lane": 0}

    tracks = state.setdefault("tracks", {})
    for detection in detections:
        stable_id = resolve_stable_track_id(state, detection, camera_name, timestamp_utc)
        active_stable_ids.add(stable_id)

        previous_xyxy = None
        existing_track = tracks.get(stable_id)
        if isinstance(existing_track, dict):
            previous_xyxy = existing_track.get("last_xyxy") if isinstance(existing_track.get("last_xyxy"), list) else None

        motion_dx, motion_dy = west_entrance_motion_vector(detection.xyxy, previous_xyxy)
        motion_class = classify_west_entrance_motion(
            detection.xyxy,
            previous_xyxy,
            line_x,
            motion_dx=motion_dx,
            motion_dy=motion_dy,
        )
        if motion_class == "incoming":
            incoming_count += 1
            center_x, _ = bbox_center(detection.xyxy)
            if center_x < line_x:
                lane_counts["left_lane"] += 1
            else:
                lane_counts["right_lane"] += 1
        else:
            exiting_count += 1

        upsert_track_record(
            state,
            stable_id,
            detection,
            camera_name,
            timestamp_utc,
            motion_class=motion_class,
            motion_dx=motion_dx,
            motion_dy=motion_dy,
        )
        track = tracks.get(stable_id)
        if isinstance(track, dict):
            track["motion_class"] = motion_class
            track["motion_dx"] = motion_dx
            track["motion_dy"] = motion_dy

    finalize_missing_tracks(state, camera_name, active_stable_ids, timestamp_utc)
    return incoming_count, exiting_count, lane_counts


def read_history_entries(path: Path) -> list[dict[str, object]]:
    if not path.exists():
        return []

    entries: list[dict[str, object]] = []
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line:
            continue
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(entry, dict):
            entries.append(entry)
    return entries


def peak_queue_today(history_path: Path, current_queue: int, timestamp_utc: str) -> int:
    current_date = timestamp_utc.split("T", 1)[0]
    peak = current_queue
    for entry in read_history_entries(history_path):
        if entry.get("date_utc") != current_date:
            continue
        queue_value = entry.get("current_queue")
        if isinstance(queue_value, int):
            peak = max(peak, queue_value)
    return peak


def append_history_entry(path: Path, result: RunResult) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "timestamp_utc": result.timestamp_utc,
        "date_utc": result.timestamp_utc.split("T", 1)[0],
        "camera_name": result.camera_name,
        "camera_label": result.camera_label,
        "status": result.status,
        "current_queue": result.current_queue,
        "peak_queue_today": result.peak_queue_today,
        "current_queue_by_lane": result.current_queue_by_lane,
        "parking_spots_total": result.parking_spots_total,
        "parking_spots_available": result.parking_spots_available,
        "incoming_count": result.incoming_count,
        "exiting_count": result.exiting_count,
        "exits": result.exits,
        "average_dwell_time_minutes": result.average_dwell_time_minutes,
    }
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(payload, ensure_ascii=False) + "\n")
    return path


@lru_cache(maxsize=1)
def load_model(model_path: str) -> YOLO:
    return YOLO(model_path)


def detect_vehicles(img: np.ndarray, config: Config) -> list[DetectionBox]:
    model = load_model(config.model_path)
    results = model.predict(
        source=img,
        conf=config.confidence,
        iou=config.iou,
        imgsz=config.image_size,
        verbose=False,
    )
    return collect_vehicle_detections(results)


def track_vehicles(img: np.ndarray, config: Config, persist: bool = True) -> list[DetectionBox]:
    model = load_model(config.model_path)
    results = model.track(
        source=img,
        conf=config.confidence,
        iou=config.iou,
        imgsz=config.image_size,
        classes=VEHICLE_CLASSES,
        persist=persist,
        verbose=False,
    )
    return collect_vehicle_detections(results)


def annotate_image(img: np.ndarray, detections: list[DetectionBox]) -> np.ndarray:
    annotated = img.copy()
    for index, detection in enumerate(detections, start=1):
        x1, y1, x2, y2 = [int(round(v)) for v in detection.xyxy]
        cv2.rectangle(annotated, (x1, y1), (x2, y2), (0, 255, 255), 2)
        label = f"{index}: {detection.class_name} {detection.confidence:.2f}"
        (text_width, text_height), baseline = cv2.getTextSize(label, cv2.FONT_HERSHEY_SIMPLEX, 0.55, 2)
        text_y = max(18, y1 - 8)
        cv2.rectangle(annotated, (x1, text_y - text_height - baseline), (x1 + text_width + 6, text_y + baseline), (0, 255, 255), -1)
        cv2.putText(annotated, label, (x1 + 3, text_y), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 0, 0), 2, cv2.LINE_AA)

    title = f"Glacier vehicle count: {len(detections)}"
    cv2.rectangle(annotated, (12, 12), (12 + 320, 12 + 42), (0, 0, 0), -1)
    cv2.putText(annotated, title, (22, 41), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (255, 255, 255), 2, cv2.LINE_AA)
    return annotated


def result_to_json_text(result: RunResult) -> str:
    return json.dumps(asdict(result), indent=2) + "\n"


def result_to_feed_text(result: RunResult) -> str:
    lines = [
        "Glacier National Park webcam vehicle feed",
        f"camera_name: {result.camera_name}",
        f"camera_label: {result.camera_label}",
        f"timestamp_utc: {result.timestamp_utc}",
        f"webcam_page_url: {result.webcam_page_url}",
        f"image_url: {result.image_url or ''}",
        f"current_queue: {result.current_queue}",
        f"peak_queue_today: {result.peak_queue_today if result.peak_queue_today is not None else ''}",
        f"current_queue_by_lane: {json.dumps(result.current_queue_by_lane or {}, ensure_ascii=False)}",
        f"parking_spots_total: {result.parking_spots_total if result.parking_spots_total is not None else ''}",
        f"parking_spots_available: {result.parking_spots_available if result.parking_spots_available is not None else ''}",
        f"incoming_count: {result.incoming_count if result.incoming_count is not None else ''}",
        f"exiting_count: {result.exiting_count if result.exiting_count is not None else ''}",
        f"exits: {result.exits if result.exits is not None else ''}",
        f"average_dwell_time_minutes: {result.average_dwell_time_minutes if result.average_dwell_time_minutes is not None else ''}",
        f"vehicle_count: {result.vehicle_count}",
        f"detected_vehicle_count: {result.detected_vehicle_count}",
        f"model_path: {result.model_path}",
        f"status: {result.status}",
    ]
    if result.message:
        lines.append(f"message: {result.message}")
    if result.error:
        lines.append(f"error: {result.error}")
    return "\n".join(lines) + "\n"


def write_text(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def write_json(path: Path, text: str) -> Path:
    return write_text(path, text)


def write_annotated_image(img: np.ndarray, path: Path) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    ok = cv2.imwrite(str(path), img)
    if not ok:
        raise ValueError(f"Failed to write annotated image to {path}")
    return path


def camera_output_path(base_path: Path, camera_name: str, default_camera_name: str = DEFAULT_CAMERA_NAME) -> Path:
    if camera_name == default_camera_name:
        return base_path
    stem = base_path.stem
    suffix = base_path.suffix
    if stem.startswith("glacier_latest_"):
        stem = stem.replace("glacier_latest_", f"glacier_{camera_name}_", 1)
    elif stem.startswith("glacier_"):
        stem = stem.replace("glacier_", f"glacier_{camera_name}_", 1)
    else:
        stem = f"{stem}_{camera_name}"
    return base_path.with_name(f"{stem}{suffix}")


def run_camera(config: Config) -> tuple[int, str]:
    headers = build_headers(config.user_agent)
    timestamp_utc = now_utc_iso()
    try:
        image_url = resolve_image_url(
            config.webcam_page_url,
            config.fallback_webcam_page_url,
            config.fallback_image_url,
            headers,
        )
        img = download_image(image_url, headers, referer=config.webcam_page_url)
        camera = GLACIER_CAMERAS.get(config.camera_name, {})
        tracking_mode = camera.get("tracking_mode", "detect")
        if tracking_mode in {"directional", "dwell"}:
            detections = track_vehicles(img, config, persist=True)
        else:
            detections = detect_vehicles(img, config)

        annotated_image = annotate_image(img, detections)
        lane_split_ratio = camera.get("lane_split_ratio")
        parking_spots_total_raw = camera.get("parking_spots_total")
        parking_spots_total = int(parking_spots_total_raw) if parking_spots_total_raw is not None else None
        tracking_state = load_tracking_state(config.tracking_state_output_path)

        if config.camera_name == "west_entrance":
            incoming_count, exiting_count, current_queue_by_lane = summarize_west_entrance_counts(
                detections,
                tracking_state,
                config.camera_name,
                timestamp_utc,
                float(img.shape[1]),
            )
            current_queue = incoming_count
            exits = exiting_count
            average_dwell_time_minutes = None
        elif config.camera_name == "logan_pass":
            current_queue, average_dwell_time_minutes, _active_vehicle_count = summarize_logan_pass_dwell_time(
                detections,
                tracking_state,
                config.camera_name,
                timestamp_utc,
            )
            incoming_count = current_queue
            exiting_count = 0
            exits = 0
            current_queue_by_lane = None
        else:
            current_queue, current_queue_by_lane = summarize_queue(
                detections,
                img.shape,
                float(lane_split_ratio) if lane_split_ratio is not None else None,
            )
            incoming_count = current_queue
            exiting_count = 0
            exits = 0
            average_dwell_time_minutes = None

        save_tracking_state(config.tracking_state_output_path, tracking_state)

        peak_today = peak_queue_today(config.history_output_path, current_queue, timestamp_utc)
        parking_spots_available = max(parking_spots_total - current_queue, 0) if parking_spots_total is not None else None
        result = RunResult(
            status="ok",
            timestamp_utc=timestamp_utc,
            camera_name=config.camera_name,
            camera_label=config.camera_label,
            webcam_page_url=config.webcam_page_url,
            image_url=image_url,
            model_path=config.model_path,
            vehicle_count=current_queue,
            detected_vehicle_count=len(detections),
            current_queue=current_queue,
            current_queue_by_lane=current_queue_by_lane,
            peak_queue_today=peak_today,
            parking_spots_total=parking_spots_total,
            parking_spots_available=parking_spots_available,
            incoming_count=incoming_count,
            exiting_count=exiting_count,
            exits=exits,
            average_dwell_time_minutes=average_dwell_time_minutes,
            detections=detections,
            message=f"Current queue {current_queue}, peak today {peak_today}",
        )
        json_text = result_to_json_text(result)
        feed_text = result_to_feed_text(result)
        json_path = write_json(config.output_path, json_text)
        feed_path = write_text(config.feed_output_path, feed_text)
        image_path = write_annotated_image(annotated_image, config.annotated_image_output_path)
        append_history_entry(config.history_output_path, result)
        print(json_text.rstrip())
        print(f"Wrote result to {json_path}")
        print(f"Wrote feed to {feed_path}")
        print(f"Wrote annotated image to {image_path}")
        return 0, json_text
    except Exception as exc:
        result = RunResult(
            status="error",
            timestamp_utc=timestamp_utc,
            camera_name=config.camera_name,
            camera_label=config.camera_label,
            webcam_page_url=config.webcam_page_url,
            model_path=config.model_path,
            current_queue=0,
            current_queue_by_lane=None,
            peak_queue_today=0,
            incoming_count=0,
            exiting_count=0,
            exits=0,
            error=str(exc),
        )
        json_text = result_to_json_text(result)
        write_json(config.output_path, json_text)
        write_text(config.feed_output_path, result_to_feed_text(result))
        print(json_text.rstrip())
        return 1, json_text


def build_camera_config(base_config: Config, camera_name: str) -> Config:
    if camera_name not in GLACIER_CAMERAS:
        raise ValueError(f"Unknown Glacier camera: {camera_name}")
    camera = GLACIER_CAMERAS[camera_name]
    return Config(
        camera_name=camera_name,
        camera_label=camera["label"],
        webcam_page_url=camera["webcam_page_url"],
        fallback_webcam_page_url=base_config.fallback_webcam_page_url,
        fallback_image_url=base_config.fallback_image_url,
        model_path=base_config.model_path,
        output_path=camera_output_path(base_config.output_path, camera_name),
        feed_output_path=camera_output_path(base_config.feed_output_path, camera_name),
        annotated_image_output_path=camera_output_path(base_config.annotated_image_output_path, camera_name),
        history_output_path=camera_history_path(base_config.history_output_path, camera_name),
        tracking_state_output_path=camera_tracking_state_path(base_config.tracking_state_output_path, camera_name),
        confidence=base_config.confidence,
        iou=base_config.iou,
        image_size=base_config.image_size,
        user_agent=base_config.user_agent,
    )


def run_once(config: Config, camera_name: str) -> int:
    if camera_name == ALL_CAMERA_NAME:
        exit_code = 0
        for name in GLACIER_CAMERAS:
            camera_config = build_camera_config(config, name)
            camera_exit_code, _ = run_camera(camera_config)
            exit_code = max(exit_code, camera_exit_code)
        return exit_code

    camera_config = build_camera_config(config, camera_name)
    camera_exit_code, _ = run_camera(camera_config)
    return camera_exit_code


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Count vehicles in the Glacier National Park webcam image")
    parser.add_argument("--camera", choices=[*GLACIER_CAMERAS.keys(), ALL_CAMERA_NAME], default=DEFAULT_CAMERA_NAME, help="Camera to process")
    parser.add_argument("--output", type=Path, default=None, help="Override output JSON path")
    parser.add_argument("--feed-output", type=Path, default=None, help="Override feed text output path")
    parser.add_argument("--annotated-image-output", type=Path, default=None, help="Override annotated image output path")
    parser.add_argument("--model", default=None, help="Override YOLO model path")
    parser.add_argument("--confidence", type=float, default=None, help="Override YOLO confidence threshold")
    parser.add_argument("--iou", type=float, default=None, help="Override YOLO IOU threshold")
    parser.add_argument("--imgsz", type=int, default=None, help="Override YOLO image size")
    parser.add_argument("--once", action="store_true", help="Run one detection cycle and exit")
    args, _unknown = parser.parse_known_args(argv)
    return args


def build_config(args: argparse.Namespace) -> Config:
    config = load_config()
    return Config(
        camera_name=args.camera if args.camera in GLACIER_CAMERAS or args.camera == ALL_CAMERA_NAME else config.camera_name,
        camera_label=config.camera_label,
        webcam_page_url=config.webcam_page_url,
        fallback_webcam_page_url=config.fallback_webcam_page_url,
        fallback_image_url=config.fallback_image_url,
        model_path=args.model or config.model_path,
        output_path=args.output or config.output_path,
        feed_output_path=args.feed_output or config.feed_output_path,
        annotated_image_output_path=args.annotated_image_output or config.annotated_image_output_path,
        history_output_path=config.history_output_path,
        tracking_state_output_path=config.tracking_state_output_path,
        confidence=args.confidence if args.confidence is not None else config.confidence,
        iou=args.iou if args.iou is not None else config.iou,
        image_size=args.imgsz if args.imgsz is not None else config.image_size,
        user_agent=config.user_agent,
    )


def main() -> int:
    args = parse_args()
    config = build_config(args)
    return run_once(config, args.camera)


if __name__ == "__main__":
    raise SystemExit(main())
