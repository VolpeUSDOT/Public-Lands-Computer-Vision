from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Optional

import cv2
import numpy as np

from count_arches import (
    DEFAULT_FALLBACK_IMAGE_URL,
    DEFAULT_USER_AGENT,
    DEFAULT_WEBCAM_PAGE_URL,
    DEFAULT_UTDOT_FALLBACK_IMAGE_URL,
    DEFAULT_UTDOT_WEBCAM_PAGE_URL,
    build_headers,
    download_image,
    fetch_bytes,
    resolve_image_url,
)


DEFAULT_OUTPUT_PATH = Path(__file__).resolve().parent / "utdot_lanes.json"
DEFAULT_UTDOT_OUTPUT_PATH = Path(__file__).resolve().parent / "utdot_lanes.json"
DEFAULT_WINDOW_NAME = "UTDOT Lane Labeler"
DEFAULT_MAX_DISPLAY_WIDTH = 3200
DEFAULT_MAX_DISPLAY_HEIGHT = 1800


@dataclass(frozen=True)
class LaneDefinition:
    lane_id: str
    label: str
    polygon: list[list[int]]


@dataclass
class LaneFile:
    output_schema: str = "arch_lane_polygons_v1"
    image_url: str = ""
    image_width: int = 0
    image_height: int = 0
    lanes: list[LaneDefinition] = field(default_factory=list)


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Interactive helper for defining UTDOT lane polygons")
    parser.add_argument("--output", type=Path, default=DEFAULT_UTDOT_OUTPUT_PATH, help="Where to save the lane JSON")
    parser.add_argument("--webcam-page-url", default=DEFAULT_UTDOT_WEBCAM_PAGE_URL, help="Web page used to resolve the current snapshot")
    parser.add_argument("--fallback-image-url", default=DEFAULT_UTDOT_FALLBACK_IMAGE_URL, help="Fallback snapshot URL")
    parser.add_argument("--user-agent", default=DEFAULT_USER_AGENT, help="User-Agent for webcam requests")
    parser.add_argument("--image-path", type=Path, default=None, help="Use a local image instead of downloading the live snapshot")
    parser.add_argument("--append", action="store_true", help="Append to an existing JSON file instead of starting over")
    parser.add_argument("--max-display-width", type=int, default=DEFAULT_MAX_DISPLAY_WIDTH, help="Maximum display width in pixels")
    parser.add_argument("--max-display-height", type=int, default=DEFAULT_MAX_DISPLAY_HEIGHT, help="Maximum display height in pixels")
    return parser.parse_args(argv)


def load_image(args: argparse.Namespace) -> tuple[np.ndarray, str]:
    if args.image_path:
        image = cv2.imread(str(args.image_path))
        if image is None:
            raise ValueError(f"Failed to load image from {args.image_path}")
        return image, str(args.image_path.resolve())

    headers = build_headers(args.user_agent)
    try:
        page_response = fetch_bytes(args.webcam_page_url, headers=headers)
        image_url = resolve_image_url(page_response.content, args.fallback_image_url)
    except Exception:
        image_url = args.fallback_image_url

    image = download_image(image_url, headers=headers)
    return image, image_url


def fit_image_for_display(image: np.ndarray, max_width: int, max_height: int) -> tuple[np.ndarray, float]:
    height, width = image.shape[:2]
    scale = min(max_width / float(width), max_height / float(height))
    if abs(scale - 1.0) < 1e-6:
        return image.copy(), 1.0

    interpolation = cv2.INTER_AREA if scale < 1.0 else cv2.INTER_CUBIC
    resized = cv2.resize(image, (max(1, int(round(width * scale))), max(1, int(round(height * scale)))), interpolation=interpolation)
    return resized, scale


def point_to_display(point: tuple[int, int], scale: float) -> tuple[int, int]:
    return int(round(point[0] * scale)), int(round(point[1] * scale))


def point_to_image(point: tuple[int, int], scale: float, image_width: int, image_height: int) -> tuple[int, int]:
    if scale <= 0:
        raise ValueError("Display scale must be positive")
    x = min(image_width - 1, max(0, int(round(point[0] / scale))))
    y = min(image_height - 1, max(0, int(round(point[1] / scale))))
    return x, y


def polygon_centroid(points: list[list[int]]) -> tuple[int, int]:
    xs = [point[0] for point in points]
    ys = [point[1] for point in points]
    return int(sum(xs) / len(xs)), int(sum(ys) / len(ys))


def draw_polygon(
    image: np.ndarray,
    polygon: list[list[int]],
    scale: float,
    color: tuple[int, int, int],
    *,
    closed: bool = True,
    label: Optional[str] = None,
) -> None:
    if not polygon:
        return

    points = np.array([point_to_display((point[0], point[1]), scale) for point in polygon], dtype=np.int32).reshape((-1, 1, 2))
    if len(points) >= 2:
        cv2.polylines(image, [points], closed, color, 2, cv2.LINE_AA)

    for point in points:
        x, y = int(point[0][0]), int(point[0][1])
        cv2.circle(image, (x, y), 4, color, -1, cv2.LINE_AA)

    if label:
        cx, cy = polygon_centroid(polygon)
        dx, dy = point_to_display((cx, cy), scale)
        cv2.putText(image, label, (dx + 6, dy - 6), cv2.FONT_HERSHEY_SIMPLEX, 0.6, color, 2, cv2.LINE_AA)


def draw_overlay(canvas: np.ndarray, labeler: "ArchLaneLabeler") -> np.ndarray:
    image = canvas.copy()
    y = 28
    for line in [
        "Left click: add lane vertices",
        "Keys: n/enter=finish lane, u=undo, r=reset current, c=clear all, q=save and quit",
        f"Completed lanes: {len(labeler.lanes)} | Current points: {len(labeler.current_points)}",
    ]:
        cv2.putText(image, line, (20, y), cv2.FONT_HERSHEY_SIMPLEX, 0.65, (0, 255, 255), 2, cv2.LINE_AA)
        y += 28

    for index, lane in enumerate(labeler.lanes, start=1):
        color = labeler.palette[(index - 1) % len(labeler.palette)]
        draw_polygon(image, lane.polygon, labeler.scale, color, label=lane.label)

    if labeler.current_points:
        draw_polygon(image, [list(point) for point in labeler.current_points], labeler.scale, (0, 255, 0), closed=False, label="Current")

    return image


def save_lane_file(path: Path, image_url: str, image: np.ndarray, lanes: list[LaneDefinition]) -> None:
    payload = LaneFile(
        image_url=image_url,
        image_width=int(image.shape[1]),
        image_height=int(image.shape[0]),
        lanes=lanes,
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(asdict(payload), indent=2) + "\n", encoding="utf-8")


def load_existing_lanes(path: Path) -> tuple[str, int, int, list[LaneDefinition]]:
    raw = json.loads(path.read_text(encoding="utf-8"))
    image_url = str(raw.get("image_url", ""))
    image_width = int(raw.get("image_width", 0) or 0)
    image_height = int(raw.get("image_height", 0) or 0)
    lanes: list[LaneDefinition] = []
    for entry in raw.get("lanes", []):
        if not isinstance(entry, dict):
            continue
        polygon = [[int(point[0]), int(point[1])] for point in entry.get("polygon", []) if isinstance(point, (list, tuple)) and len(point) == 2]
        if len(polygon) >= 3:
            lanes.append(
                LaneDefinition(
                    lane_id=str(entry.get("lane_id", f"lane_{len(lanes) + 1}")),
                    label=str(entry.get("label", f"Lane {len(lanes) + 1}")),
                    polygon=polygon,
                )
            )
    return image_url, image_width, image_height, lanes


class ArchLaneLabeler:
    def __init__(self, image: np.ndarray, scale: float, image_url: str, lanes: list[LaneDefinition], output_path: Path) -> None:
        self.image = image
        self.scale = scale
        self.image_url = image_url
        self.lanes = lanes
        self.output_path = output_path
        self.current_points: list[tuple[int, int]] = []
        self.window_name = DEFAULT_WINDOW_NAME
        self.palette = [
            (0, 255, 255),
            (255, 128, 0),
            (0, 200, 0),
            (255, 0, 255),
            (0, 128, 255),
            (255, 255, 0),
            (0, 0, 255),
        ]

    def on_mouse(self, event: int, x: int, y: int, _flags: int, _userdata: object) -> None:
        if event != cv2.EVENT_LBUTTONDOWN:
            return
        self.current_points.append(point_to_image((x, y), self.scale, self.image.shape[1], self.image.shape[0]))

    def finish_current_lane(self) -> None:
        if len(self.current_points) < 3:
            print("A lane needs at least 3 points")
            return

        default_index = len(self.lanes) + 1
        default_label = f"Lane {default_index}"
        print(f"Completed lane with {len(self.current_points)} points for {default_label}")
        try:
            entered = input(f"Label for {default_label} [{default_label}]: ").strip()
        except EOFError:
            entered = ""
        label = entered or default_label
        self.lanes.append(
            LaneDefinition(
                lane_id=f"lane_{default_index}",
                label=label,
                polygon=[[int(x), int(y)] for x, y in self.current_points],
            )
        )
        self.current_points.clear()
        save_lane_file(self.output_path, self.image_url, self.image, self.lanes)
        print(f"Saved {len(self.lanes)} lanes to {self.output_path}")

    def undo(self) -> None:
        if self.current_points:
            self.current_points.pop()
            return
        if self.lanes:
            removed = self.lanes.pop()
            print(f"Removed {removed.label}")
            save_lane_file(self.output_path, self.image_url, self.image, self.lanes)

    def reset_current(self) -> None:
        self.current_points.clear()

    def clear_all(self) -> None:
        self.current_points.clear()
        self.lanes.clear()
        save_lane_file(self.output_path, self.image_url, self.image, self.lanes)
        print("Cleared all lanes")

    def render(self) -> np.ndarray:
        return draw_overlay(self.image, self)


def main(argv: Optional[list[str]] = None) -> int:
    args = parse_args(argv)
    image, image_url = load_image(args)

    lanes: list[LaneDefinition] = []
    if args.append and args.output.exists():
        existing_image_url, _existing_width, _existing_height, lanes = load_existing_lanes(args.output)
        if existing_image_url:
            image_url = existing_image_url

    display_image, scale = fit_image_for_display(image, args.max_display_width, args.max_display_height)
    labeler = ArchLaneLabeler(display_image, scale, image_url, lanes, args.output)

    cv2.namedWindow(labeler.window_name, cv2.WINDOW_NORMAL)
    cv2.setMouseCallback(labeler.window_name, labeler.on_mouse)

    print("Click points around each lane boundary in order around the polygon.")
    print(f"Saving to {args.output}")

    while True:
        frame = labeler.render()
        cv2.imshow(labeler.window_name, frame)
        key = cv2.waitKey(20) & 0xFF
        if key == ord("q"):
            break
        if key in (ord("n"), 13, 10):
            labeler.finish_current_lane()
        elif key == ord("u"):
            labeler.undo()
        elif key == ord("r"):
            labeler.reset_current()
        elif key == ord("c"):
            labeler.clear_all()

    save_lane_file(args.output, image_url, image, labeler.lanes)
    cv2.destroyAllWindows()
    print(f"Wrote {len(labeler.lanes)} lanes to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
