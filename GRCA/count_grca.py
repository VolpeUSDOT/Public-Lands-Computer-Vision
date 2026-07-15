from __future__ import annotations

import argparse
import base64
import csv
import json
import os
import re
import tempfile
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from functools import lru_cache
from pathlib import Path
from typing import Any, Optional

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


def _default_output_dir() -> Path:
    name = SCRIPT_DIR.name.upper()
    if name == "ARCH":
        return SCRIPT_DIR
    if name == "GRCA":
        return SCRIPT_DIR
    if name == "IP3":
        return SCRIPT_DIR / "ARCH"
    if SCRIPT_DIR.parent.name.upper() == "IP3":
        return SCRIPT_DIR.parent / "ARCH"
    return SCRIPT_DIR.parent / "IP3" / "ARCH"


DEFAULT_WEBCAM_PAGE_URL = (
    "https://www.nps.gov/media/webcam/view.htm?id=9B5FC6BA-9FE6-EC6B-61637825D562D367&r=/grca/learn/photosmultimedia/webcams.htm"
)
DEFAULT_SR64_WEBCAM_PAGE_URL = (
    "https://www.nps.gov/media/webcam/view.htm?id=7D6A3936-E1C5-C480-4FA08472583AA182&r=/grca/learn/photosmultimedia/webcams.htm"
)
DEFAULT_FALLBACK_IMAGE_URL = ""
DEFAULT_UTDOT_WEBCAM_PAGE_URL = "https://prod-ut.ibi511.com/map/Cctv/136741?t=1782490208"
DEFAULT_UTDOT_FALLBACK_IMAGE_URL = DEFAULT_UTDOT_WEBCAM_PAGE_URL
DEFAULT_MODEL_PATH = "yolov8x.pt"
DEFAULT_OUTPUT_DIR = _default_output_dir()
DEFAULT_LANES_PATH = DEFAULT_OUTPUT_DIR / "grca_lanes.json"
DEFAULT_FEED_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "grca_latest_feed.txt"
DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "grca_latest_annotated.jpg"
DEFAULT_SR64_ANNOTATED_IMAGE_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "sr64_annotated.jpg"
DEFAULT_ARCHIVE_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "grca_archivefeed.csv"
DEFAULT_UTDOT_LANES_PATH = DEFAULT_OUTPUT_DIR / "utdot_lanes.json"
DEFAULT_UTDOT_ANNOTATED_IMAGE_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "UTDOT_latest_annotated.jpg"
DEFAULT_GITHUB_REPOSITORY = "VolpeUSDOT/Public-Lands-Computer-Vision"
DEFAULT_GITHUB_BRANCH = "main"
DEFAULT_GITHUB_JSON_PATH = "IP3/ARCH/grca_latest_feed.json"
DEFAULT_GITHUB_FEED_PATH = "IP3/ARCH/grca_latest_feed.txt"
DEFAULT_GITHUB_IMAGE_PATH = "IP3/ARCH/grca_latest_annotated.jpg"
DEFAULT_GITHUB_ARCHIVE_CSV_PATH = "IP3/ARCH/grca_archivefeed.csv"
DEFAULT_UTDOT_GITHUB_IMAGE_PATH = "IP3/ARCH/UTDOT_latest_annotated.jpg"
DEFAULT_ARCHIVE_MAX_ROWS = 4999
DEFAULT_ARCHIVE_ROTATION_DIRNAME = "archive"
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
DEFAULT_OUTPUT_PATH = DEFAULT_OUTPUT_DIR / "grca_latest_feed.json"
DEFAULT_ENV_PATH = SCRIPT_DIR / ".env"
VEHICLE_CLASSES = [2, 3, 5, 7]


@dataclass(frozen=True)
class Config:
    webcam_page_url: str = DEFAULT_WEBCAM_PAGE_URL
    sr64_webcam_page_url: str = DEFAULT_SR64_WEBCAM_PAGE_URL
    fallback_image_url: str = DEFAULT_FALLBACK_IMAGE_URL
    sr64_fallback_image_url: str = DEFAULT_FALLBACK_IMAGE_URL
    utdot_webcam_page_url: str = DEFAULT_UTDOT_WEBCAM_PAGE_URL
    utdot_fallback_image_url: str = DEFAULT_UTDOT_FALLBACK_IMAGE_URL
    model_path: str = DEFAULT_MODEL_PATH
    lanes_path: Path = DEFAULT_LANES_PATH
    utdot_lanes_path: Path = DEFAULT_UTDOT_LANES_PATH
    output_path: Path = DEFAULT_OUTPUT_PATH
    feed_output_path: Path = DEFAULT_FEED_OUTPUT_PATH
    annotated_image_output_path: Path = DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH
    sr64_annotated_image_output_path: Path = DEFAULT_SR64_ANNOTATED_IMAGE_OUTPUT_PATH
    utdot_annotated_image_output_path: Path = DEFAULT_UTDOT_ANNOTATED_IMAGE_OUTPUT_PATH
    archive_output_path: Path = DEFAULT_ARCHIVE_OUTPUT_PATH
    github_repository: str = DEFAULT_GITHUB_REPOSITORY
    github_branch: str = DEFAULT_GITHUB_BRANCH
    github_json_path: str = DEFAULT_GITHUB_JSON_PATH
    github_feed_path: str = DEFAULT_GITHUB_FEED_PATH
    github_image_path: str = DEFAULT_GITHUB_IMAGE_PATH
    github_archive_csv_path: str = DEFAULT_GITHUB_ARCHIVE_CSV_PATH
    utdot_github_image_path: str = DEFAULT_UTDOT_GITHUB_IMAGE_PATH
    github_token: Optional[str] = None
    publish_to_github: bool = False
    confidence: float = 0.50
    iou: float = 0.45
    image_size: int = 1280
    user_agent: str = DEFAULT_USER_AGENT
    archive_max_rows: int = DEFAULT_ARCHIVE_MAX_ROWS


@dataclass
class DetectionBox:
    class_id: int
    class_name: str
    confidence: float
    xyxy: list[float]


@dataclass(frozen=True)
class LaneDefinition:
    lane_id: str
    label: str
    polygon: list[list[float]]


@dataclass
class RunResult:
    status: str
    timestamp_utc: str
    webcam_page_url: str
    image_url: Optional[str] = None
    model_path: str = DEFAULT_MODEL_PATH
    vehicle_count: int = 0
    lane_1_count: int = 0
    lane_2_count: int = 0
    in_line_count: int = 0
    on_entrance_road: int = 0
    detections: list[DetectionBox] = field(default_factory=list)
    message: Optional[str] = None
    error: Optional[str] = None


def load_config() -> Config:
    """Load runtime settings from environment variables."""

    load_env_file()
    output_path = normalize_grca_path(Path(os.getenv("OUTPUT_PATH", str(DEFAULT_OUTPUT_PATH))).expanduser())
    feed_output_path = normalize_grca_path(Path(os.getenv("FEED_OUTPUT_PATH", str(DEFAULT_FEED_OUTPUT_PATH))).expanduser())
    annotated_image_output_path = normalize_grca_path(Path(os.getenv("ANNOTATED_IMAGE_OUTPUT_PATH", str(DEFAULT_ANNOTATED_IMAGE_OUTPUT_PATH))).expanduser())
    utdot_annotated_image_output_path = normalize_grca_path(Path(os.getenv("UTDOT_ANNOTATED_IMAGE_OUTPUT_PATH", str(DEFAULT_UTDOT_ANNOTATED_IMAGE_OUTPUT_PATH))).expanduser())
    archive_output_path = normalize_grca_path(Path(os.getenv("ARCHIVE_OUTPUT_PATH", str(DEFAULT_ARCHIVE_OUTPUT_PATH))).expanduser())
    github_token = os.getenv("GITHUB_TOKEN") or os.getenv("GH_TOKEN")
    publish_to_github = os.getenv("PUBLISH_TO_GITHUB", "").strip().lower() in {"1", "true", "yes", "on"}
    if github_token and not os.getenv("PUBLISH_TO_GITHUB"):
        publish_to_github = True
    return Config(
        webcam_page_url=os.getenv("WEBCAM_PAGE_URL", DEFAULT_WEBCAM_PAGE_URL),
        sr64_webcam_page_url=os.getenv("SR64_WEBCAM_PAGE_URL", DEFAULT_SR64_WEBCAM_PAGE_URL),
        fallback_image_url=os.getenv("FALLBACK_IMAGE_URL", DEFAULT_FALLBACK_IMAGE_URL),
        sr64_fallback_image_url=os.getenv("SR64_FALLBACK_IMAGE_URL", DEFAULT_FALLBACK_IMAGE_URL),
        utdot_webcam_page_url=os.getenv("UTDOT_WEBCAM_PAGE_URL", DEFAULT_UTDOT_WEBCAM_PAGE_URL),
        utdot_fallback_image_url=os.getenv("UTDOT_FALLBACK_IMAGE_URL", DEFAULT_UTDOT_FALLBACK_IMAGE_URL),
        model_path=os.getenv("MODEL_PATH", DEFAULT_MODEL_PATH),
        lanes_path=normalize_grca_path(Path(os.getenv("LANES_PATH", str(DEFAULT_LANES_PATH))).expanduser()),
        utdot_lanes_path=normalize_grca_path(Path(os.getenv("UTDOT_LANES_PATH", str(DEFAULT_UTDOT_LANES_PATH))).expanduser()),
        output_path=output_path,
        feed_output_path=feed_output_path,
        annotated_image_output_path=annotated_image_output_path,
        sr64_annotated_image_output_path=normalize_grca_path(Path(os.getenv("SR64_ANNOTATED_IMAGE_OUTPUT_PATH", str(DEFAULT_SR64_ANNOTATED_IMAGE_OUTPUT_PATH))).expanduser()),
        utdot_annotated_image_output_path=utdot_annotated_image_output_path,
        archive_output_path=archive_output_path,
        github_repository=os.getenv("GITHUB_REPOSITORY", DEFAULT_GITHUB_REPOSITORY),
        github_branch=os.getenv("GITHUB_BRANCH", DEFAULT_GITHUB_BRANCH),
        github_json_path=os.getenv("GITHUB_JSON_PATH", DEFAULT_GITHUB_JSON_PATH),
        github_feed_path=os.getenv("GITHUB_FEED_PATH", DEFAULT_GITHUB_FEED_PATH),
        github_image_path=os.getenv("GITHUB_IMAGE_PATH", DEFAULT_GITHUB_IMAGE_PATH),
        github_archive_csv_path=os.getenv("GITHUB_ARCHIVE_CSV_PATH", DEFAULT_GITHUB_ARCHIVE_CSV_PATH),
        utdot_github_image_path=os.getenv("UTDOT_GITHUB_IMAGE_PATH", DEFAULT_UTDOT_GITHUB_IMAGE_PATH),
        github_token=github_token,
        publish_to_github=publish_to_github,
        confidence=float(os.getenv("YOLO_CONFIDENCE", "0.50")),
        iou=float(os.getenv("YOLO_IOU", "0.45")),
        image_size=int(os.getenv("YOLO_IMAGE_SIZE", "1280")),
        user_agent=os.getenv("USER_AGENT", DEFAULT_USER_AGENT),
        archive_max_rows=int(os.getenv("ARCHIVE_MAX_ROWS", str(DEFAULT_ARCHIVE_MAX_ROWS))),
    )


def build_headers(user_agent: str) -> dict[str, str]:
    return {
        "User-Agent": user_agent,
        "Cache-Control": "no-cache",
        "Pragma": "no-cache",
    }


def now_utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def normalize_grca_path(path: Path) -> Path:
    raw = str(path)
    raw = raw.replace("/IP3/IP3/ARCH", "/IP3/ARCH")
    raw = raw.replace("\\IP3\\IP3\\ARCH", "\\IP3\\ARCH")
    raw = raw.replace("/IP3/IP3/", "/IP3/")
    raw = raw.replace("\\IP3\\IP3\\", "\\IP3\\")
    return Path(raw)


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


def resolve_image_url(page_html: bytes, fallback_image_url: str) -> str:
    def normalize_candidate(url: str) -> str:
        url = url.strip()
        if not url:
            return ""
        if url.startswith("//"):
            return f"https:{url}"
        if url.startswith("/"):
            return f"https://www.nps.gov{url}"
        return url

    def candidate_score(candidate: str, context: str = "") -> int:
        text = f"{candidate} {context}".lower()
        score = 0

        positive_terms = {
            "webcam": 10,
            "camera": 8,
            "cctv": 8,
            "live": 6,
            "image": 4,
            "photosmultimedia": 6,
            "south entrance": 6,
            "entrance station": 6,
        }
        negative_terms = {
            "flag": -12,
            "logo": -12,
            "icon": -10,
            "sprite": -10,
            "search": -8,
            "menu": -8,
            "social": -8,
            "footer": -6,
            "header": -6,
        }

        for term, weight in positive_terms.items():
            if term in text:
                score += weight
        for term, weight in negative_terms.items():
            if term in text:
                score += weight

        if re.search(r"/webcam/|webcam|camera|cctv", candidate, re.IGNORECASE):
            score += 12
        if re.search(r"/flag|flag|logo|icon|sprite", candidate, re.IGNORECASE):
            score -= 12
        if re.search(r"\.(?:jpg|jpeg|png)(?:\?|$)", candidate, re.IGNORECASE):
            score += 2

        return score

    soup = BeautifulSoup(page_html, "html.parser")
    scored_candidates: list[tuple[int, str]] = []
    for tag in soup.find_all(["img", "meta", "source", "a", "link"]):
        context_bits = [
            str(tag.get("alt", "")),
            str(tag.get("title", "")),
            str(tag.get("aria-label", "")),
            str(tag.get("id", "")),
            str(tag.get("class", "")),
        ]
        parent = tag.parent
        if parent is not None:
            context_bits.append(parent.get_text(" ", strip=True)[:400])
        context = " ".join(bit for bit in context_bits if bit)
        for key in ("src", "data-src", "href", "content", "srcset"):
            value = tag.get(key, "")
            if value:
                raw_value = str(value)
                candidate = normalize_candidate(raw_value.split(",", 1)[0].split(" ", 1)[0])
                lowered = candidate.lower()
                if not lowered.endswith((".jpg", ".jpeg", ".png")) and ".jpg?" not in lowered and ".jpeg?" not in lowered and ".png?" not in lowered:
                    continue
                scored_candidates.append((candidate_score(candidate, context), candidate))

    page_text = page_html.decode("utf-8", errors="ignore")
    for raw_candidate in re.findall(r"https?://[^\"'\s>]+?\.(?:jpg|jpeg|png)(?:\?[^\"'\s>]*)?", page_text, re.IGNORECASE):
        candidate = normalize_candidate(raw_candidate)
        scored_candidates.append((candidate_score(candidate, page_text), candidate))
    for raw_candidate in re.findall(r"//[^\"'\s>]+?\.(?:jpg|jpeg|png)(?:\?[^\"'\s>]*)?", page_text, re.IGNORECASE):
        candidate = normalize_candidate(raw_candidate)
        scored_candidates.append((candidate_score(candidate, page_text), candidate))
    for raw_candidate in re.findall(r"/[^\"'\s>]+?\.(?:jpg|jpeg|png)(?:\?[^\"'\s>]*)?", page_text, re.IGNORECASE):
        candidate = normalize_candidate(raw_candidate)
        scored_candidates.append((candidate_score(candidate, page_text), candidate))

    if scored_candidates:
        scored_candidates.sort(key=lambda item: item[0], reverse=True)
        return scored_candidates[0][1]

    return fallback_image_url


def cache_busted_url(url: str) -> str:
    """Add a request-specific cache-buster without changing the base image URL."""

    parsed = urlsplit(url)
    query = dict(parse_qsl(parsed.query, keep_blank_values=True))
    query["_"] = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S%f")
    return urlunsplit((parsed.scheme, parsed.netloc, parsed.path, urlencode(query), parsed.fragment))


def fetch_bytes(url: str, headers: dict[str, str], timeout: int = 10) -> requests.Response:
    response = requests.get(url, headers=headers, timeout=timeout)
    response.raise_for_status()
    return response


def download_image(image_url: str, headers: dict[str, str]) -> np.ndarray:
    response = fetch_bytes(cache_busted_url(image_url), headers=headers)
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


def load_lane_definitions(path: Path) -> list[LaneDefinition]:
    if not path.exists():
        raise FileNotFoundError(f"Lane file not found: {path}")

    payload = json.loads(path.read_text(encoding="utf-8"))
    lanes_raw = payload.get("lanes", [])
    if not isinstance(lanes_raw, list):
        raise ValueError("Lane JSON must contain a list under 'lanes'")

    lanes: list[LaneDefinition] = []
    for index, raw_lane in enumerate(lanes_raw, start=1):
        if not isinstance(raw_lane, dict):
            continue
        polygon_raw = raw_lane.get("polygon", [])
        polygon: list[list[float]] = []
        for point in polygon_raw:
            if isinstance(point, (list, tuple)) and len(point) == 2:
                polygon.append([float(point[0]), float(point[1])])
        if len(polygon) < 3:
            continue
        lanes.append(
            LaneDefinition(
                lane_id=str(raw_lane.get("lane_id") or f"lane_{index}"),
                label=str(raw_lane.get("label") or f"Lane {index}"),
                polygon=polygon,
            )
        )

    if not lanes:
        raise ValueError(f"No valid lane polygons found in {path}")
    return lanes


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
    y = y2 - max(6.0, 0.1 * height)
    return (x1 + x2) / 2.0, y


def lane_detection_counts(detections: list[DetectionBox], lanes: list[LaneDefinition]) -> dict[str, int]:
    counts = {lane.label: 0 for lane in lanes}
    for detection in detections:
        anchor = detection_anchor_point(detection)
        for lane in lanes:
            if point_in_polygon(anchor, lane.polygon):
                counts[lane.label] = counts.get(lane.label, 0) + 1
                break
    return counts


def run_detection(config: Config) -> RunResult:
    return run_detection_with_image(config)[0]


def run_detection_with_image(config: Config) -> tuple[RunResult, Optional[np.ndarray]]:
    return _run_detection_with_image(
        config,
        webcam_page_url=config.webcam_page_url,
        fallback_image_url=config.fallback_image_url,
        lanes_path=config.lanes_path,
        count_lane_label="Lane 1",
    )


def run_utdot_detection_with_image(config: Config) -> tuple[RunResult, Optional[np.ndarray]]:
    return _run_detection_with_image(
        config,
        webcam_page_url=config.utdot_webcam_page_url,
        fallback_image_url=config.utdot_fallback_image_url,
        lanes_path=config.utdot_lanes_path,
        count_lane_label=None,
    )


def _run_detection_with_image(
    config: Config,
    *,
    webcam_page_url: str,
    fallback_image_url: str,
    lanes_path: Path,
    count_lane_label: Optional[str],
) -> tuple[RunResult, Optional[np.ndarray]]:
    headers = build_headers(config.user_agent)
    timestamp = now_utc_iso()

    try:
        image_url = fallback_image_url
        try:
            page_response = fetch_bytes(cache_busted_url(webcam_page_url), headers=headers)
            image_url = resolve_image_url(page_response.content, fallback_image_url)
        except Exception:
            image_url = fallback_image_url

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
        lanes = load_lane_definitions(lanes_path)
        lane_counts = lane_detection_counts(detections, lanes)
        lane_1_count = lane_counts.get("Lane 1", 0)
        lane_2_count = lane_counts.get("Lane 2", 0)
        in_line_count = lane_1_count + lane_2_count
        entrance_lane_label = count_lane_label or (lanes[0].label if lanes else "")
        on_entrance_road = lane_counts.get(entrance_lane_label, 0) if entrance_lane_label else 0
        annotated_image = annotate_image(img, results, RunResult(
            status="ok",
            timestamp_utc=timestamp,
            webcam_page_url=webcam_page_url,
            image_url=image_url,
            model_path=config.model_path,
            vehicle_count=len(detections),
            lane_1_count=lane_1_count,
            lane_2_count=lane_2_count,
            in_line_count=in_line_count,
            on_entrance_road=on_entrance_road,
            detections=detections,
            message=f"Detected {len(detections)} vehicles anywhere in frame; lane 1={lane_1_count}, lane 2={lane_2_count}, in line={in_line_count}, on_entrance_road={on_entrance_road}",
        ))
        return (
            RunResult(
                status="ok",
                timestamp_utc=timestamp,
                webcam_page_url=webcam_page_url,
                image_url=image_url,
                model_path=config.model_path,
                vehicle_count=len(detections),
                lane_1_count=lane_1_count,
                lane_2_count=lane_2_count,
                in_line_count=in_line_count,
                on_entrance_road=on_entrance_road,
                detections=detections,
                message=f"Detected {len(detections)} vehicles anywhere in frame; lane 1={lane_1_count}, lane 2={lane_2_count}, in line={in_line_count}, on_entrance_road={on_entrance_road}",
            ),
            annotated_image,
        )
    except Exception as exc:
        return (
            RunResult(
                status="error",
                timestamp_utc=timestamp,
                webcam_page_url=webcam_page_url,
                model_path=config.model_path,
                error=str(exc),
            ),
            None,
        )


def annotate_image(img: np.ndarray, results: Any, result: RunResult) -> np.ndarray:
    if not results:
        return img
    annotated = results[0].plot()
    overlay_lines = [
        result.timestamp_utc,
        f"total_vehicles_detected={result.vehicle_count}",
        f"vehicles_in_lane_1={result.lane_1_count}",
        f"vehicles_in_lane_2={result.lane_2_count}",
        f"vehicles_in_line={result.in_line_count}",
        f"on_entrance_road={result.on_entrance_road}",
    ]
    y = 36
    for line in overlay_lines:
        cv2.putText(
            annotated,
            line,
            (16, y),
            cv2.FONT_HERSHEY_SIMPLEX,
            1.0,
            (0, 0, 0),
            4,
            cv2.LINE_AA,
        )
        cv2.putText(
            annotated,
            line,
            (16, y),
            cv2.FONT_HERSHEY_SIMPLEX,
            1.0,
            (255, 255, 255),
            2,
            cv2.LINE_AA,
        )
        y += 36
    return annotated


def result_to_dict(result: RunResult, output_schema: str) -> dict[str, Any]:
    payload = asdict(result)
    payload["detections"] = [asdict(detection) for detection in result.detections]
    payload["output_schema"] = output_schema
    return payload


def result_to_json_text(result: RunResult, output_schema: str) -> str:
    return json.dumps(result_to_dict(result, output_schema), indent=2) + "\n"


def result_to_feed_payload(result: RunResult, output_schema: str) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "status": result.status,
        "timestamp_utc": result.timestamp_utc,
        "total_vehicles_detected": result.vehicle_count,
        "vehicles_in_lane_1": result.lane_1_count,
        "vehicles_in_lane_2": result.lane_2_count,
        "vehicles_in_line": result.in_line_count,
        "on_entrance_road": result.on_entrance_road,
        "output_schema": output_schema,
    }
    if result.message is not None:
        payload["message"] = result.message
    if result.error is not None:
        payload["error"] = result.error
    return payload


def result_to_feed_json_text(result: RunResult, output_schema: str) -> str:
    return json.dumps(result_to_feed_payload(result, output_schema), indent=2) + "\n"


def result_to_archive_row(result: RunResult) -> dict[str, Any]:
    return {
        "timestamp_utc": result.timestamp_utc,
        "total_vehicles_detected": result.vehicle_count,
        "vehicles_in_lane_1": result.lane_1_count,
        "vehicles_in_lane_2": result.lane_2_count,
        "vehicles_in_line": result.in_line_count,
        "on_entrance_road": result.on_entrance_road,
    }


def result_to_feed_text(result: RunResult, feed_title: str, output_schema: str) -> str:
    lines = [
        feed_title,
        f"status: {result.status}",
        f"timestamp_utc: {result.timestamp_utc}",
        f"total_vehicles_detected: {result.vehicle_count}",
        f"vehicles_in_lane_1: {result.lane_1_count}",
        f"vehicles_in_lane_2: {result.lane_2_count}",
        f"vehicles_in_line: {result.in_line_count}",
        f"on_entrance_road: {result.on_entrance_road}",
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

    lines.append("lane_breakdown:")
    lines.append(f"- vehicles_in_lane_1: {result.lane_1_count}")
    lines.append(f"- vehicles_in_lane_2: {result.lane_2_count}")
    lines.append(f"- vehicles_in_line: {result.in_line_count}")

    lines.append(f"output_schema: {output_schema}")
    return "\n".join(lines) + "\n"


def result_to_image_commit_message(result: RunResult, camera_name: str) -> str:
    return (
        f"Update {camera_name} webcam image "
        f"{result.timestamp_utc} "
        f"vehicles={result.vehicle_count} "
        f"lane1={result.lane_1_count} "
        f"lane2={result.lane_2_count} "
        f"inline={result.in_line_count} "
        f"on_entrance_road={result.on_entrance_road}"
    )


def write_result(result: RunResult, output_path: Path, output_schema: str = "grca_lane_vehicle_count_v1") -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(result_to_feed_json_text(result, output_schema), encoding="utf-8")
    return output_path


def write_feed(result: RunResult, output_path: Path, feed_title: str = "GRCA webcam vehicle feed", output_schema: str = "grca_lane_vehicle_feed_v1") -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    feed_text = result_to_feed_text(result, feed_title, output_schema)
    output_path.write_text(feed_text, encoding="utf-8")
    return output_path


def write_archive_csv(result: RunResult, output_path: Path, max_rows: int) -> Path:
    written_path, _archived_path = append_archive_csv(result, output_path, max_rows)
    return written_path


def write_annotated_image(img: np.ndarray, output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    if not cv2.imwrite(str(output_path), img):
        raise ValueError(f"Failed to write annotated image to {output_path}")
    return output_path


def archive_csv_header() -> list[str]:
    return [
        "timestamp_utc",
        "total_vehicles_detected",
        "vehicles_in_lane_1",
        "vehicles_in_lane_2",
        "vehicles_in_line",
        "on_entrance_road",
    ]


def archive_row_to_dict(result: RunResult) -> dict[str, Any]:
    return result_to_archive_row(result)


def archive_rows_to_csv_text(results: list[RunResult]) -> str:
    from io import StringIO

    buffer = StringIO()
    writer = csv.DictWriter(buffer, fieldnames=archive_csv_header(), extrasaction="ignore")
    writer.writeheader()
    for result in results:
        writer.writerow(archive_row_to_dict(result))
    return buffer.getvalue()


def append_archive_csv(result: RunResult, output_path: Path, max_rows: int) -> tuple[Path, Optional[Path]]:
    return try_append_archive_rows(output_path, [result], max_rows)


def csv_text_to_rows(csv_text: str) -> list[dict[str, str]]:
    from io import StringIO

    if not csv_text.strip():
        return []
    return list(csv.DictReader(StringIO(csv_text)))


def rows_to_csv_text(rows: list[dict[str, Any]]) -> str:
    from io import StringIO

    buffer = StringIO()
    writer = csv.DictWriter(buffer, fieldnames=archive_csv_header(), extrasaction="ignore")
    writer.writeheader()
    for row in rows:
        writer.writerow({key: row.get(key) for key in archive_csv_header()})
    return buffer.getvalue()


def archive_csv_row_count(path: Path) -> int:
    if not path.exists() or path.stat().st_size <= 0:
        return 0

    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return sum(1 for _ in reader)


def resolve_writable_output_path(output_path: Path) -> Path:
    try:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        return output_path
    except (PermissionError, OSError):
        fallback_path = Path(tempfile.gettempdir()) / "arches" / output_path.name
        fallback_path.parent.mkdir(parents=True, exist_ok=True)
        return fallback_path


def maybe_rotate_archive_csv(output_path: Path, incoming_row_count: int, max_rows: int) -> tuple[Path, Optional[Path]]:
    writable_path = resolve_writable_output_path(output_path)
    if max_rows <= 0 or incoming_row_count <= 0:
        return writable_path, None

    current_row_count = archive_csv_row_count(writable_path)
    if current_row_count == 0 or current_row_count + incoming_row_count <= max_rows:
        return writable_path, None

    archive_dir = writable_path.parent / DEFAULT_ARCHIVE_ROTATION_DIRNAME
    archive_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ")
    archived_path = archive_dir / f"{writable_path.stem}{timestamp}{writable_path.suffix}"
    suffix = 1
    while archived_path.exists():
        archived_path = archive_dir / f"{writable_path.stem}{timestamp}_{suffix}{writable_path.suffix}"
        suffix += 1
    writable_path.replace(archived_path)
    return writable_path, archived_path


def try_append_archive_rows(output_path: Path, results: list[RunResult], max_rows: int) -> tuple[Path, Optional[Path]]:
    writable_path, archived_path = maybe_rotate_archive_csv(output_path, len(results), max_rows)

    def _append(path: Path) -> None:
        file_exists = path.exists() and path.stat().st_size > 0
        with path.open("a", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=archive_csv_header())
            if not file_exists:
                writer.writeheader()
            for result in results:
                writer.writerow(archive_row_to_dict(result))

    try:
        _append(writable_path)
        return writable_path, archived_path
    except (PermissionError, OSError):
        fallback_path = Path(tempfile.gettempdir()) / "arches" / writable_path.name
        fallback_path.parent.mkdir(parents=True, exist_ok=True)
        _append(fallback_path)
        return fallback_path, archived_path


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

    encoded_content = base64.b64encode(content_text.encode("utf-8")).decode("ascii")

    for attempt in range(1, 4):
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
            "content": encoded_content,
            "branch": branch,
        }
        if current_sha:
            body["sha"] = current_sha

        try:
            response = requests.put(url, headers=headers, json=body, timeout=20)
            response.raise_for_status()
            if attempt > 1:
                return True, f"Published {repository}/{file_path} on {branch} after refresh"
            return True, f"Published {repository}/{file_path} on {branch}"
        except requests.RequestException as exc:
            if getattr(exc.response, "status_code", None) == 409 and attempt < 3:
                continue
            if getattr(exc.response, "status_code", None) == 409:
                return False, f"Failed to publish GitHub file {file_path} after 3 conflict retries"
            return False, f"Failed to publish GitHub file {file_path}: {exc}"

    return False, f"Failed to publish GitHub file {file_path} after retries"


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

    for attempt in range(1, 4):
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
            if attempt > 1:
                return True, f"Published {repository}/{file_path} on {branch} after refresh"
            return True, f"Published {repository}/{file_path} on {branch}"
        except requests.RequestException as exc:
            if getattr(exc.response, "status_code", None) == 409 and attempt < 3:
                continue
            if getattr(exc.response, "status_code", None) == 409:
                return False, f"Failed to publish GitHub file {file_path} after 3 conflict retries"
            return False, f"Failed to publish GitHub file {file_path}: {exc}"

    return False, f"Failed to publish GitHub file {file_path} after retries"


def publish_feed_to_github(feed_text: str, config: Config, github_feed_path: str, message: str) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    return publish_text_to_github(
        feed_text,
        config.github_repository,
        config.github_branch,
        github_feed_path,
        config.github_token,
        message,
    )


def publish_json_to_github(json_text: str, config: Config, github_json_path: str, message: str) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    return publish_text_to_github(
        json_text,
        config.github_repository,
        config.github_branch,
        github_json_path,
        config.github_token,
        message,
    )


def publish_annotated_image_to_github(image: np.ndarray, config: Config, github_image_path: str, camera_name: str) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub publish: no GITHUB_TOKEN or GH_TOKEN set"

    commit_result = RunResult(
        status="ok",
        timestamp_utc=now_utc_iso(),
        webcam_page_url=config.webcam_page_url,
    )

    return publish_image_to_github(
        image,
        config.github_repository,
        config.github_branch,
        github_image_path,
        config.github_token,
        result_to_image_commit_message(commit_result, camera_name),
    )


def publish_archive_csv_to_github(csv_text: str, config: Config, github_archive_csv_path: str, message: str) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub archive publish: no GITHUB_TOKEN or GH_TOKEN set"

    headers = get_github_headers(config.github_token)
    url = github_contents_api_url(config.github_repository, github_archive_csv_path)

    current_sha: Optional[str] = None
    existing_text = ""
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
        return False, f"Failed to inspect GitHub archive CSV {github_archive_csv_path}: {exc}"

    existing_rows = csv_text_to_rows(existing_text)
    incoming_rows = csv_text_to_rows(csv_text)

    if existing_rows and len(existing_rows) + len(incoming_rows) > config.archive_max_rows:
        archived_path = Path(github_archive_csv_path)
        archived_name = f"{archived_path.stem}{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')}{archived_path.suffix}"
        archived_repo_path = str(archived_path.parent / DEFAULT_ARCHIVE_ROTATION_DIRNAME / archived_name).replace("\\", "/")

        archived_ok, archived_message = publish_text_to_github(
            existing_text,
            config.github_repository,
            config.github_branch,
            archived_repo_path,
            config.github_token,
            "Archive webcam CSV",
        )
        if not archived_ok:
            return False, archived_message

        combined_text = rows_to_csv_text(incoming_rows)
        body: dict[str, Any] = {
            "message": message,
            "content": base64.b64encode(combined_text.encode("utf-8")).decode("ascii"),
            "branch": config.github_branch,
        }
    else:
        combined_rows = existing_rows + incoming_rows
        combined_text = rows_to_csv_text(combined_rows)
        body = {
            "message": message,
            "content": base64.b64encode(combined_text.encode("utf-8")).decode("ascii"),
            "branch": config.github_branch,
        }

    if current_sha:
        body["sha"] = current_sha

    try:
        response = requests.put(url, headers=headers, json=body, timeout=20)
        response.raise_for_status()
    except requests.RequestException as exc:
        if getattr(exc.response, "status_code", None) == 409:
            try:
                refresh_response = requests.get(url, headers=headers, params={"ref": config.github_branch}, timeout=10)
                if refresh_response.status_code == 200:
                    refresh_payload = refresh_response.json()
                    refreshed_sha = refresh_payload.get("sha")
                    if refreshed_sha:
                        body["sha"] = refreshed_sha
                        retry_response = requests.put(url, headers=headers, json=body, timeout=20)
                        retry_response.raise_for_status()
                        return True, f"Published {config.github_repository}/{github_archive_csv_path} on {config.github_branch} after refresh"
            except requests.RequestException as retry_exc:
                return False, f"Failed to publish GitHub archive CSV {github_archive_csv_path} after refresh: {retry_exc}"
        return False, f"Failed to publish GitHub archive CSV {github_archive_csv_path}: {exc}"

    return True, f"Published {config.github_repository}/{github_archive_csv_path} on {config.github_branch}"


def publish_archive_csv_row_to_github(result: RunResult, config: Config, github_archive_csv_path: str, message: str) -> tuple[bool, str]:
    if not config.github_token:
        return False, "Skipped GitHub archive publish: no GITHUB_TOKEN or GH_TOKEN set"

    headers = get_github_headers(config.github_token)
    url = github_contents_api_url(config.github_repository, github_archive_csv_path)

    incoming_row = archive_row_to_dict(result)
    for attempt in range(1, 4):
        current_sha: Optional[str] = None
        existing_text = ""
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
            return False, f"Failed to inspect GitHub archive CSV {github_archive_csv_path}: {exc}"

        existing_rows = csv_text_to_rows(existing_text)
        combined_rows = existing_rows + [incoming_row]
        if config.archive_max_rows > 0 and len(combined_rows) > config.archive_max_rows:
            combined_rows = combined_rows[-config.archive_max_rows :]

        body: dict[str, Any] = {
            "message": message,
            "content": base64.b64encode(rows_to_csv_text(combined_rows).encode("utf-8")).decode("ascii"),
            "branch": config.github_branch,
        }
        if current_sha:
            body["sha"] = current_sha

        try:
            response = requests.put(url, headers=headers, json=body, timeout=20)
            response.raise_for_status()
            return True, f"Published {config.github_repository}/{github_archive_csv_path} on {config.github_branch}"
        except requests.RequestException as exc:
            if getattr(exc.response, "status_code", None) == 409 and attempt < 3:
                continue
            if getattr(exc.response, "status_code", None) == 409:
                return False, f"Failed to publish GitHub archive CSV {github_archive_csv_path} after 3 conflict retries"
            return False, f"Failed to publish GitHub archive CSV {github_archive_csv_path}: {exc}"

    return False, f"Failed to publish GitHub archive CSV {github_archive_csv_path} after retries"


def run_camera_pipeline(
    config: Config,
    *,
    webcam_page_url: str,
    fallback_image_url: str,
    lanes_path: Path,
    output_path: Path,
    feed_output_path: Path,
    annotated_image_output_path: Path,
    archive_output_path: Path,
    github_json_path: str,
    github_feed_path: str,
    github_image_path: str,
    github_archive_csv_path: str,
    feed_title: str,
    json_schema: str,
    feed_schema: str,
    camera_name: str,
    count_lane_label: Optional[str],
) -> dict[str, Any]:
    camera_config = Config(
        webcam_page_url=webcam_page_url,
        fallback_image_url=fallback_image_url,
        utdot_webcam_page_url=config.utdot_webcam_page_url,
        utdot_fallback_image_url=config.utdot_fallback_image_url,
        model_path=config.model_path,
        lanes_path=lanes_path,
        utdot_lanes_path=config.utdot_lanes_path,
        output_path=output_path,
        feed_output_path=feed_output_path,
        annotated_image_output_path=annotated_image_output_path,
        utdot_annotated_image_output_path=config.utdot_annotated_image_output_path,
        archive_output_path=archive_output_path,
        github_repository=config.github_repository,
        github_branch=config.github_branch,
        github_json_path=github_json_path,
        github_feed_path=github_feed_path,
        github_image_path=github_image_path,
        github_archive_csv_path=github_archive_csv_path,
        utdot_github_image_path=config.utdot_github_image_path,
        github_token=config.github_token,
        publish_to_github=config.publish_to_github,
        confidence=config.confidence,
        iou=config.iou,
        image_size=config.image_size,
        user_agent=config.user_agent,
        archive_max_rows=config.archive_max_rows,
    )

    result, annotated_image = _run_detection_with_image(
        camera_config,
        webcam_page_url=webcam_page_url,
        fallback_image_url=fallback_image_url,
        lanes_path=lanes_path,
        count_lane_label=count_lane_label,
    )
    result_json = result_to_feed_json_text(result, json_schema)
    feed_text = result_to_feed_text(result, feed_title, feed_schema)
    write_result(result, output_path, json_schema)
    write_feed(result, feed_output_path, feed_title, feed_schema)
    archive_output_path_written, archived_path = append_archive_csv(result, archive_output_path, camera_config.archive_max_rows)
    annotated_image_output_path_written = None
    if annotated_image is not None:
        annotated_image_output_path_written = write_annotated_image(annotated_image, annotated_image_output_path)

    publish_messages: list[str] = []
    publish_ok = True
    if camera_config.publish_to_github:
        json_published, json_message = publish_json_to_github(result_json, camera_config, github_json_path, f"Update {camera_name} vehicle count JSON")
        feed_published, feed_message = publish_feed_to_github(feed_text, camera_config, github_feed_path, f"Update {camera_name} webcam feed")
        archive_published, archive_message = publish_archive_csv_row_to_github(result, camera_config, github_archive_csv_path, f"Update {camera_name} archive CSV")
        image_published = True
        image_message = None
        if annotated_image is not None:
            image_published, image_message = publish_annotated_image_to_github(annotated_image, camera_config, github_image_path, camera_name)
        if not archive_published:
            print(archive_message)
        if not image_published:
            print(image_message)
        publish_ok = json_published and feed_published and archive_published and image_published
        publish_messages.extend([json_message, feed_message])
        if archive_message:
            publish_messages.append(archive_message)
        if image_message:
            publish_messages.append(image_message)

    print(result_json.rstrip())
    print(f"Wrote result to {output_path}")
    print(f"Wrote feed to {feed_output_path}")
    print(f"Updated archive to {archive_output_path_written}")
    print(f"Archive rows: {archive_csv_row_count(archive_output_path_written)}")
    if archived_path:
        print(f"Archived previous CSV to {archived_path}")
    if annotated_image_output_path_written:
        print(f"Wrote annotated image to {annotated_image_output_path_written}")
    for publish_message in publish_messages:
        print(publish_message)

    return {
        "result": result,
        "publish_ok": publish_ok,
    }


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Count vehicles in the configured webcam image")
    parser.add_argument("--output", type=Path, default=None, help="Override output JSON path")
    parser.add_argument("--feed-output", type=Path, default=None, help="Override local feed text output path")
    parser.add_argument("--model", default=None, help="Override YOLO model path")
    parser.add_argument("--confidence", type=float, default=None, help="Override YOLO confidence threshold")
    parser.add_argument("--iou", type=float, default=None, help="Override YOLO IOU threshold")
    parser.add_argument("--imgsz", type=int, default=None, help="Override YOLO image size")
    args, _unknown = parser.parse_known_args(argv)
    return args


def build_config(args: argparse.Namespace) -> Config:
    config = load_config()
    return Config(
        webcam_page_url=config.webcam_page_url,
        sr64_webcam_page_url=config.sr64_webcam_page_url,
        fallback_image_url=config.fallback_image_url,
        sr64_fallback_image_url=config.sr64_fallback_image_url,
        utdot_webcam_page_url=config.utdot_webcam_page_url,
        utdot_fallback_image_url=config.utdot_fallback_image_url,
        model_path=args.model or config.model_path,
        lanes_path=config.lanes_path,
        utdot_lanes_path=config.utdot_lanes_path,
        output_path=args.output or config.output_path,
        feed_output_path=args.feed_output or config.feed_output_path,
        annotated_image_output_path=config.annotated_image_output_path,
        sr64_annotated_image_output_path=config.sr64_annotated_image_output_path,
        utdot_annotated_image_output_path=config.utdot_annotated_image_output_path,
        archive_output_path=config.archive_output_path,
        github_repository=config.github_repository,
        github_branch=config.github_branch,
        github_json_path=config.github_json_path,
        github_feed_path=config.github_feed_path,
        github_image_path=config.github_image_path,
        github_archive_csv_path=config.github_archive_csv_path,
        utdot_github_image_path=config.utdot_github_image_path,
        github_token=config.github_token,
        publish_to_github=config.publish_to_github,
        confidence=args.confidence if args.confidence is not None else config.confidence,
        iou=args.iou if args.iou is not None else config.iou,
        image_size=args.imgsz if args.imgsz is not None else config.image_size,
        user_agent=config.user_agent,
        archive_max_rows=config.archive_max_rows,
    )


def main() -> int:
    args = parse_args()
    config = build_config(args)
    grca_result, grca_annotated_image = _run_detection_with_image(
        config,
        webcam_page_url=config.webcam_page_url,
        fallback_image_url=config.fallback_image_url,
        lanes_path=config.lanes_path,
        count_lane_label="Lane 1",
    )
    utdot_result, utdot_annotated_image = run_utdot_detection_with_image(config)
    sr64_result, sr64_annotated_image = _run_detection_with_image(
        config,
        webcam_page_url=config.sr64_webcam_page_url,
        fallback_image_url=config.sr64_fallback_image_url,
        lanes_path=config.lanes_path,
        count_lane_label="Lane 1",
    )

    if grca_result.status == "ok" and utdot_result.status == "ok" and sr64_result.status == "ok":
        grca_result.on_entrance_road = utdot_result.on_entrance_road
        grca_result.message = (
            f"Detected {grca_result.vehicle_count} vehicles anywhere in frame; "
            f"lane 1={grca_result.lane_1_count}, lane 2={grca_result.lane_2_count}, "
            f"in line={grca_result.in_line_count}, on_entrance_road={grca_result.on_entrance_road}"
        )

    output_path = write_result(grca_result, config.output_path, "grca_lane_vehicle_count_v1")
    json_text = result_to_feed_json_text(grca_result, "grca_lane_vehicle_count_v1")
    feed_output_path = write_feed(grca_result, config.feed_output_path, "GRCA webcam vehicle feed", "grca_lane_vehicle_feed_v1")
    feed_text = result_to_feed_text(grca_result, "GRCA webcam vehicle feed", "grca_lane_vehicle_feed_v1")
    archive_output_path, archived_path = append_archive_csv(grca_result, config.archive_output_path, config.archive_max_rows)
    annotated_image_output_path = None
    if grca_annotated_image is not None:
        annotated_image_output_path = write_annotated_image(grca_annotated_image, config.annotated_image_output_path)

    utdot_image_output_path = None
    if utdot_annotated_image is not None:
        utdot_image_output_path = write_annotated_image(utdot_annotated_image, config.utdot_annotated_image_output_path)

    sr64_image_output_path = None
    if sr64_annotated_image is not None:
        sr64_image_output_path = write_annotated_image(sr64_annotated_image, config.sr64_annotated_image_output_path)

    publish_messages: list[str] = []
    publish_ok = True
    if config.publish_to_github:
        json_published, json_message = publish_json_to_github(json_text, config, config.github_json_path, "Update webcam vehicle count JSON")
        feed_published, feed_message = publish_feed_to_github(feed_text, config, config.github_feed_path, "Update webcam feed")
        archive_published, archive_message = publish_archive_csv_row_to_github(grca_result, config, config.github_archive_csv_path, "Update GRCA archive CSV")
        image_published = True
        image_message = None
        utdot_image_published = True
        utdot_image_message = None
        if annotated_image_output_path is not None:
            image_published, image_message = publish_annotated_image_to_github(grca_annotated_image, config, config.github_image_path, "webcam")
        if utdot_image_output_path is not None:
            utdot_image_published, utdot_image_message = publish_annotated_image_to_github(utdot_annotated_image, config, config.utdot_github_image_path, "UTDOT")
        if not archive_published:
            print(archive_message)
        if not image_published:
            print(image_message)
        if not utdot_image_published:
            print(utdot_image_message)
        publish_ok = json_published and feed_published and archive_published and image_published and utdot_image_published
        publish_messages.extend([json_message, feed_message])
        if archive_message:
            publish_messages.append(archive_message)
        if image_message:
            publish_messages.append(image_message)
        if utdot_image_message:
            publish_messages.append(utdot_image_message)

    print(json_text.rstrip())
    print(f"Wrote result to {output_path}")
    print(f"Wrote feed to {feed_output_path}")
    print(f"Updated archive to {archive_output_path}")
    print(f"Archive rows: {archive_csv_row_count(archive_output_path)}")
    if archived_path:
        print(f"Archived previous CSV to {archived_path}")
    if annotated_image_output_path:
        print(f"Wrote annotated image to {annotated_image_output_path}")
    if utdot_image_output_path:
        print(f"Wrote UTDOT annotated image to {utdot_image_output_path}")
    if sr64_image_output_path:
        print(f"Wrote SR64 annotated image to {sr64_image_output_path}")
    for publish_message in publish_messages:
        print(publish_message)

    return 0 if grca_result.status == "ok" and utdot_result.status == "ok" and sr64_result.status == "ok" and publish_ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
