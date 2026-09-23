const GLACIER_REPO = "VolpeUSDOT/Public-Lands-Computer-Vision";
const GLACIER_BRANCH = "main";

const GLACIER_CAMERAS = {
  logan_pass: {
    key: "logan_pass",
    name: "Logan Pass",
    title: "Logan Pass Parking Lot",
    page: "logan_pass.html",
    jsonPath: "glacier_latest_feed.json",
    feedPath: "glacier_latest_feed.txt",
    historyPath: "glacier_latest_history.jsonl",
    imagePath: "glacier_latest_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=325AE6AF-BAEB-F65D-EF3D638BF683E78E&r=/glac/learn/photosmultimedia/webcams.htm",
    parkingSpotsTotal: 100,
  },
  west_entrance: {
    key: "west_entrance",
    name: "West Entrance",
    title: "West Entrance",
    page: "west_entrance.html",
    jsonPath: "glacier_west_entrance_feed.json",
    feedPath: "glacier_west_entrance_feed.txt",
    historyPath: "glacier_west_entrance_history.jsonl",
    imagePath: "glacier_west_entrance_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=33478DF3-1DD8-B71B-0B8C97DB0A03B0F7",
    laneSplitRatio: 0.5,
  },
  apgar_village: {
    key: "apgar_village",
    name: "Apgar Village",
    title: "Apgar Village",
    page: "apgar_village.html",
    jsonPath: "glacier_apgar_village_feed.json",
    feedPath: "glacier_apgar_village_feed.txt",
    historyPath: "glacier_apgar_village_history.jsonl",
    imagePath: "glacier_apgar_village_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=81B4692D-1DD8-B71B-0B9AE4B7C186B022",
  },
};

const CAMERA_ORDER = ["logan_pass", "west_entrance", "apgar_village"];

function formatUtc(value) {
  if (!value) return "--";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return `${new Intl.DateTimeFormat(undefined, {
    dateStyle: "medium",
    timeStyle: "short",
    timeZone: "UTC",
  }).format(date)} UTC`;
}

function formatCount(value) {
  return Number.isFinite(value) ? String(value) : "--";
}

function parseFeed(text) {
  const map = new Map();
  for (const line of text.split(/\r?\n/)) {
    const idx = line.indexOf(":");
    if (idx > 0) {
      map.set(line.slice(0, idx).trim(), line.slice(idx + 1).trim());
    }
  }
  return map;
}

function parseHistory(text) {
  const rows = [];
  for (const line of text.split(/\r?\n/)) {
    if (!line.trim()) continue;
    try {
      rows.push(JSON.parse(line));
    } catch {
      continue;
    }
  }
  return rows;
}

async function fetchText(path) {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) throw new Error(`Unable to load ${path}: ${response.status}`);
  return response.text();
}

async function fetchJson(path) {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) throw new Error(`Unable to load ${path}: ${response.status}`);
  return response.json();
}

function derivedQueueByLane(camera, json, imageWidth) {
  if (json.current_queue_by_lane) return json.current_queue_by_lane;
  if (!camera.laneSplitRatio || !Array.isArray(json.detections) || !Number.isFinite(imageWidth)) return null;

  const counts = { left_lane: 0, right_lane: 0 };
  for (const detection of json.detections) {
    const box = detection.xyxy || [];
    if (box.length < 4) continue;
    const centerX = (Number(box[0]) + Number(box[2])) / 2;
    if (centerX < imageWidth * camera.laneSplitRatio) counts.left_lane += 1;
    else counts.right_lane += 1;
  }
  return counts;
}

function currentQueue(json) {
  return Number.isFinite(json.current_queue) ? json.current_queue : (json.vehicle_count ?? 0);
}

function peakQueue(json, historyRows) {
  if (Number.isFinite(json.peak_queue_today)) return json.peak_queue_today;
  const today = (json.timestamp_utc || "").split("T", 1)[0];
  let peak = currentQueue(json);
  for (const row of historyRows) {
    if (row.date_utc !== today) continue;
    if (Number.isFinite(row.current_queue)) peak = Math.max(peak, row.current_queue);
  }
  return peak;
}

function availableParking(json, camera) {
  if (Number.isFinite(json.parking_spots_available)) return json.parking_spots_available;
  if (!Number.isFinite(camera.parkingSpotsTotal)) return null;
  return Math.max(camera.parkingSpotsTotal - currentQueue(json), 0);
}

function laneLabel(key, camera) {
  if (camera.key === "west_entrance") {
    return key === "left_lane" ? "Left lane" : "Right lane";
  }
  return key === "left_lane" ? "Lane A" : "Lane B";
}

function renderFeed(feedMap, json, camera, historyRows) {
  const feedStatus = document.querySelector("[data-feed-status]");
  const feedTimestamp = document.querySelector("[data-feed-timestamp]");
  const feedMessage = document.querySelector("[data-feed-message]");
  const feedImageUrl = document.querySelector("[data-feed-image-url]");
  const feedCurrentQueue = document.querySelector("[data-feed-current-queue]");
  const feedPeakQueue = document.querySelector("[data-feed-peak-queue]");
  const feedLaneQueue = document.querySelector("[data-feed-lane-queue]");
  const feedParking = document.querySelector("[data-feed-parking]");

  if (feedStatus) feedStatus.textContent = feedMap.get("status") || json.status || "--";
  if (feedTimestamp) feedTimestamp.textContent = formatUtc(feedMap.get("timestamp_utc") || json.timestamp_utc);
  if (feedMessage) feedMessage.textContent = feedMap.get("message") || json.message || "--";
  if (feedImageUrl) feedImageUrl.textContent = feedMap.get("image_url") || json.image_url || "--";
  if (feedCurrentQueue) feedCurrentQueue.textContent = String(currentQueue(json));
  if (feedPeakQueue) feedPeakQueue.textContent = String(peakQueue(json, historyRows));

  const laneCounts = derivedQueueByLane(camera, json);
  if (feedLaneQueue) {
    if (!laneCounts) {
      feedLaneQueue.textContent = "Not split";
    } else {
      feedLaneQueue.textContent = Object.entries(laneCounts)
        .map(([key, value]) => `${laneLabel(key, camera)} ${value}`)
        .join(" | ");
    }
  }

  const parking = availableParking(json, camera);
  if (feedParking) {
    feedParking.textContent = Number.isFinite(parking) ? String(parking) : "N/A";
  }
}

function renderCameraPage(cameraKey, json, feedText, historyRows) {
  const camera = GLACIER_CAMERAS[cameraKey];
  const feedMap = parseFeed(feedText);
  const queue = currentQueue(json);
  const peak = peakQueue(json, historyRows);
  const parking = availableParking(json, camera);

  const titleEl = document.querySelector("[data-camera-title]");
  const subtitleEl = document.querySelector("[data-camera-subtitle]");
  const imageEl = document.querySelector("[data-camera-image]");
  const labelEl = document.querySelector("[data-camera-label]");
  const timestampEl = document.querySelector("[data-camera-timestamp]");
  const sourceEl = document.querySelector("[data-camera-source]");
  const syncEl = document.querySelector("[data-sync-text]");
  const currentEl = document.querySelector("[data-current-queue]");
  const peakEl = document.querySelector("[data-peak-queue]");
  const parkingEl = document.querySelector("[data-parking-spots]");
  const laneListEl = document.querySelector("[data-lane-list]");
  const errorEl = document.querySelector("[data-error]");

  if (titleEl) titleEl.textContent = camera.title;
  if (subtitleEl) {
    subtitleEl.textContent =
      camera.key === "logan_pass"
        ? "Current queue plus parking availability for the Logan Pass lot."
        : "Current queue plus today's peak from the published history file.";
  }
  if (imageEl) {
    imageEl.src = camera.imagePath;
    imageEl.alt = `${camera.title} annotated webcam image`;
  }
  if (labelEl) labelEl.textContent = json.camera_label || camera.title;
  if (timestampEl) timestampEl.textContent = formatUtc(json.timestamp_utc);
  if (sourceEl) {
    sourceEl.href = camera.webcamPageUrl;
    sourceEl.textContent = "Open source webcam page";
  }
  if (syncEl) syncEl.textContent = `Updated ${formatUtc(json.timestamp_utc)}`;
  if (currentEl) currentEl.textContent = String(queue);
  if (peakEl) peakEl.textContent = String(peak);
  if (parkingEl) parkingEl.textContent = Number.isFinite(parking) ? String(parking) : "N/A";

  if (laneListEl) {
    laneListEl.innerHTML = "";
    const laneCounts = derivedQueueByLane(camera, json, imageEl?.naturalWidth);
    if (!laneCounts) {
      const row = document.createElement("li");
      row.className = "lane-row";
      row.innerHTML = `<span class="lane-name">Lane split</span><span class="lane-value">Not needed</span>`;
      laneListEl.appendChild(row);
    } else {
      for (const [key, value] of Object.entries(laneCounts)) {
        const row = document.createElement("li");
        row.className = "lane-row";
        row.innerHTML = `<span class="lane-name">${laneLabel(key, camera)}</span><span class="lane-value">${value}</span>`;
        laneListEl.appendChild(row);
      }
    }
  }

  renderFeed(feedMap, json, camera, historyRows);

  if (errorEl) errorEl.style.display = "none";

  document.querySelectorAll("[data-nav-camera]").forEach((link) => {
    link.setAttribute("aria-current", link.dataset.navCamera === camera.key ? "page" : "false");
  });
}

async function loadCameraData(cameraKey) {
  const camera = GLACIER_CAMERAS[cameraKey];
  const [json, feedText, historyText] = await Promise.all([
    fetchJson(camera.jsonPath),
    fetchText(camera.feedPath),
    fetchText(camera.historyPath).catch(() => ""),
  ]);
  return { json, feedText, historyRows: parseHistory(historyText) };
}

async function renderOverview() {
  const cards = await Promise.all(
    CAMERA_ORDER.map(async (cameraKey) => {
      const camera = GLACIER_CAMERAS[cameraKey];
      const { json, historyRows } = await loadCameraData(cameraKey);
      return { camera, json, historyRows };
    })
  );

  for (const { camera, json, historyRows } of cards) {
    const card = document.querySelector(`[data-summary-card="${camera.key}"]`);
    if (!card) continue;
    const queueEl = card.querySelector("[data-summary-current]");
    const peakEl = card.querySelector("[data-summary-peak]");
    const timeEl = card.querySelector("[data-summary-time]");
    const imageEl = card.querySelector("[data-summary-image]");
    const linkEl = card.querySelector("[data-summary-link]");

    if (queueEl) queueEl.textContent = String(currentQueue(json));
    if (peakEl) peakEl.textContent = String(peakQueue(json, historyRows));
    if (timeEl) timeEl.textContent = formatUtc(json.timestamp_utc);
    if (imageEl) imageEl.src = camera.imagePath;
    if (linkEl) linkEl.href = camera.page;
  }
}

function wireHomeLinks() {
  document.querySelectorAll("[data-home-link]").forEach((link) => {
    link.href = "index.html";
  });
}

async function initGlacierSite() {
  const view = document.body.dataset.view;
  wireHomeLinks();

  if (view === "overview") {
    renderOverview().catch((error) => {
      console.error(error);
    });
    return;
  }

  const cameraKey = document.body.dataset.camera;
  if (!cameraKey || !GLACIER_CAMERAS[cameraKey]) return;

  try {
    const { json, feedText, historyRows } = await loadCameraData(cameraKey);
    renderCameraPage(cameraKey, json, feedText, historyRows);
  } catch (error) {
    console.error(error);
    const errorEl = document.querySelector("[data-error]");
    if (errorEl) {
      errorEl.style.display = "block";
      errorEl.textContent = "The Glacier feed could not be loaded from the published GitHub pages files.";
    }
  }
}

document.addEventListener("DOMContentLoaded", initGlacierSite);
