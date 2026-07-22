import cv2
import cv2
from pathlib import Path

# This tells Python to find the exact folder where get_pixels.py lives
SCRIPT_DIR = Path(__file__).resolve().parent

# This builds the perfect path to your image in that same folder
IMAGE_PATH = SCRIPT_DIR / "glacier_latest_annotated.jpg" 

# Load the image using the absolute path
img = cv2.imread(str(IMAGE_PATH))

# Simple safety check to tell you exactly what went wrong if it still fails
if img is None:
    raise FileNotFoundError(f"Could not find or open the image file at: {IMAGE_PATH}. Check if the file is actually named logan.jpg or glacier_latest_annotated.jpg in this folder!")

def click_event(event, x, y, flags, params):
    if event == cv2.EVENT_LBUTTONDOWN:
        print(f"[{x}, {y}]")
        cv2.circle(img, (x, y), 3, (0, 0, 255), -1)
        cv2.imshow("Map Image", img)

cv2.imshow("Map Image", img)
cv2.setMouseCallback("Map Image", click_event)
cv2.waitKey(0)
cv2.destroyAllWindows()