# Arches Webcam Vehicle Count

This script fetches the NPS Arches webcam page, resolves the current webcam image, runs YOLO vehicle detection, and writes the latest result to a JSON artifact that can be committed to GitHub.

## What it does

- Downloads the NPS webcam webpage
- Finds the current Arches webcam JPG
- Runs YOLOv8 vehicle detection
- Counts detected vehicles
- Writes a structured JSON result file

## Script

`Count_Arches.py`

## Output

By default, the script writes a JSON file called `arch_vehicle_count_latest.json`

The JSON includes:

- `status`
- `timestamp_utc`
- `webcam_page_url`
- `image_url`
- `model_path`
- `vehicle_count`
- `detections`
- `message`
- `error`
- `output_schema`

The script also writes a plain-text feed file at `arches_latest_feed.txt` by default, and can publish that file to GitHub when a token is provided.

## Requirements

Python 3.10+ is recommended.

### Python packages

Install the required packages:

```bash
pip install ultralytics opencv-python requests beautifulsoup4 pillow numpy
```

Notes:

- `ultralytics` will pull in PyTorch dependencies as needed, but on some systems you may need to install PyTorch separately.
- `pillow` is not required by the current script, but it is commonly useful if you later add image preview or notebook display support.

## Recommended environment setup

Create and activate a virtual environment first if you are running locally.

### Windows PowerShell

```powershell
python -m venv .venv
.venv\Scripts\Activate.ps1
pip install ultralytics opencv-python requests beautifulsoup4 pillow numpy
```

### macOS/Linux

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install ultralytics opencv-python requests beautifulsoup4 pillow numpy
```

## Model file

The script defaults to:

```text
yolov8x.pt
```

This model will be downloaded automatically by Ultralytics the first time it is used, if it is not already present.

If you want to use a different model, pass `--model` or set the `MODEL_PATH` environment variable.

## Configuration

The script can be configured with environment variables or CLI flags.

### Environment variables

| Variable | Default | Purpose |
|---|---|---|
| `WEBCAM_PAGE_URL` | NPS Arches webcam page URL | The webpage to scan for the image |
| `FALLBACK_IMAGE_URL` | `https://www.nps.gov/webcams-arch/arch_traffic.jpg` | Used if the page does not expose a direct image link |
| `MODEL_PATH` | `yolov8x.pt` | YOLO model path |
| `OUTPUT_PATH` | `arch_vehicle_count_latest.json` | Where to write the result JSON |
| `YOLO_CONFIDENCE` | `0.50` | Confidence threshold |
| `YOLO_IOU` | `0.45` | IOU threshold |
| `YOLO_IMAGE_SIZE` | `1280` | Inference image size |
| `USER_AGENT` | Chrome-like browser string | Request header used for fetching the webpage |
| `FEED_OUTPUT_PATH` | `arches_latest_feed.txt` | Where to write the plain-text feed |
| `GITHUB_REPOSITORY` | `VolpeUSDOT/Public-Lands-Computer-Vision` | GitHub repository to update |
| `GITHUB_BRANCH` | `main` | Branch to update in GitHub |
| `GITHUB_FEED_PATH` | `YOLO/arches_latest_feed.txt` | File path in the repository |
| `GITHUB_TOKEN` / `GH_TOKEN` | unset | Token used to publish the feed file to GitHub |
| `PUBLISH_TO_GITHUB` | unset | Set to `1`, `true`, `yes`, or `on` to force publishing |

The script also reads a local `.env` file in the `YOLO` folder if one exists. Values in the real environment still win over `.env` entries.

### Example environment setup

#### `.env` file

Create `YOLO\.env` with entries like:

```dotenv
GITHUB_TOKEN=ghp_your_token_here
PUBLISH_TO_GITHUB=1
GITHUB_REPOSITORY=VolpeUSDOT/Public-Lands-Computer-Vision
GITHUB_BRANCH=main
GITHUB_FEED_PATH=YOLO/arches_latest_feed.txt
```

`count_arches.py` loads this file automatically at startup.

#### Windows PowerShell

```powershell
$env:OUTPUT_PATH = "results\arch_vehicle_count_latest.json"
$env:YOLO_CONFIDENCE = "0.50"
$env:YOLO_IOU = "0.45"
$env:YOLO_IMAGE_SIZE = "1280"
$env:GITHUB_TOKEN = "<your-token>"
$env:PUBLISH_TO_GITHUB = "1"
```

#### macOS/Linux

```bash
export OUTPUT_PATH="results/arch_vehicle_count_latest.json"
export YOLO_CONFIDENCE="0.50"
export YOLO_IOU="0.45"
export YOLO_IMAGE_SIZE="1280"
export GITHUB_TOKEN="<your-token>"
export PUBLISH_TO_GITHUB="1"
```

## Running the script

### Basic run

From the folder containing `Count_Arches.py`:

```bash
python "Count_Arches.py"
```

### Override the output path

```bash
python "Count_Arches.py" --output arch_vehicle_count_latest.json
```

### Override the model

```bash
python "Count_Arches.py" --model yolov8n.pt
```

### Override detection parameters

```bash
python "Count_Arches.py" --confidence 0.4 --iou 0.5 --imgsz 1280
```

## Expected behavior

On success, the script will:

1. Fetch the webcam page
2. Resolve the image URL
3. Download the image
4. Run YOLO detection
5. Print the JSON result to stdout
6. Write the JSON result to the configured output file

If detection fails or the image cannot be decoded, the script still writes a JSON result with:

- `status: "error"`
- an `error` message describing the failure

## Result schema

Example output:

```json
{
  "status": "ok",
  "timestamp_utc": "2026-06-09T18:00:00+00:00",
  "webcam_page_url": "https://www.nps.gov/media/webcam/view.htm?id=81B46BDB-1DD8-B71B-0B250E8F4CC1E74A",
  "image_url": "https://www.nps.gov/webcams-arch/arch_traffic.jpg",
  "model_path": "yolov8x.pt",
  "vehicle_count": 3,
  "detections": [
    {
      "class_id": 2,
      "class_name": "car",
      "confidence": 0.91,
      "xyxy": [123.4, 456.7, 234.5, 567.8]
    }
  ],
  "message": "Detected 3 vehicles",
  "error": null,
  "output_schema": "arch_vehicle_count_v1"
}
```

## GitHub workflow

The script is designed to write a file into the repository so you can:

- commit the latest result to GitHub
- publish `arches_latest_feed.txt` automatically through the GitHub Contents API
- inspect historical runs if you choose to keep dated artifacts
- later switch the same output path to a Databricks or cloud-storage location

### Token requirements

Automatic publishing requires a GitHub token with permission to write repository contents. A fine-grained personal access token or GitHub Actions token with contents write access is sufficient.

### Suggested GitHub pattern

- Keep the latest result in `arch_vehicle_count_latest.json`
- Optionally write dated snapshots in the same directory, for example:
  - `results/arch_vehicle_count_2026-06-09.json`
