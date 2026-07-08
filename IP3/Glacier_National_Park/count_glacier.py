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
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
VEHICLE_CLASSES = [2, 3, 5, 7]
DEFAULT_CAMERA_NAME = "logan_pass"
ALL_CAMERA_NAME = "all"

GLACIER_CAMERAS: dict[str, dict[str, str]] = {
    "logan_pass": {
        "label": "Logan Pass Parking Lot",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=325AE6AF-BAEB-F65D-EF3D638BF683E78E&r=/glac/learn/photosmultimedia/webcams.htm",
    },
    "west_entrance": {
        "label": "West Entrance",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=33478DF3-1DD8-B71B-0B8C97DB0A03B0F7",
    },
    "apgar_village": {
        "label": "Apgar Village",
        "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=81B4692D-1DD8-B71B-0B9AE4B7C186B022",
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
    confidence: float = 0.5
    iou: float = 0.45
    image_size: int = 1280
    user_agent: str = DEFAULT_USER_AGENT


@dataclass
class DetectionBox:
    class_id: int
    class_name: str
    confidence: float
    xyxy: list[float]


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
        confidence=float(os.getenv("YOLO_CONFIDENCE", "0.5")),
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
            detections.append(
                DetectionBox(
                    class_id=class_id,
                    class_name=str(names.get(class_id, class_id)),
                    confidence=confidence,
                    xyxy=xyxy,
                )
            )
    return detections


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
        detections = detect_vehicles(img, config)
        annotated_image = annotate_image(img, detections)
        result = RunResult(
            status="ok",
            timestamp_utc=timestamp_utc,
            camera_name=config.camera_name,
            camera_label=config.camera_label,
            webcam_page_url=config.webcam_page_url,
            image_url=image_url,
            model_path=config.model_path,
            vehicle_count=len(detections),
            detected_vehicle_count=len(detections),
            detections=detections,
            message=f"Detected {len(detections)} vehicles anywhere in frame",
        )
        json_text = result_to_json_text(result)
        feed_text = result_to_feed_text(result)
        json_path = write_json(config.output_path, json_text)
        feed_path = write_text(config.feed_output_path, feed_text)
        image_path = write_annotated_image(annotated_image, config.annotated_image_output_path)
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
