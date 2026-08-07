const GLACIER_CAMERAS = {
  logan_pass: {
    key: "logan_pass",
    title: "Logan Pass Parking Lot",
    page: "logan_pass.html",
    jsonPath: "glacier_latest_feed.json",
    feedPath: "glacier_latest_feed.txt",
    imagePath: "glacier_latest_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=325AE6AF-BAEB-F65D-EF3D638BF683E78E&r=/glac/learn/photosmultimedia/webcams.htm",
    parkingSpotsTotal: 100,
  },
  west_entrance: {
    key: "west_entrance",
    title: "West Entrance",
    page: "west_entrance.html",
    jsonPath: "glacier_west_entrance_feed.json",
    feedPath: "glacier_west_entrance_feed.txt",
    imagePath: "glacier_west_entrance_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=33478DF3-1DD8-B71B-0B8C97DB0A03B0F7",
  },
  apgar_village: {
    key: "apgar_village",
    title: "Apgar Village",
    page: "apgar_village.html",
    jsonPath: "glacier_apgar_village_feed.json",
    feedPath: "glacier_apgar_village_feed.txt",
    imagePath: "glacier_apgar_village_annotated.jpg",
    webcamPageUrl: "https://www.nps.gov/media/webcam/view.htm?id=81B4692D-1DD8-B71B-0B9AE4B7C186B022",
  },
};

const CAMERA_ORDER = ["logan_pass", "west_entrance", "apgar_village"];
const REFRESH_INTERVAL_MS = 300000;

// Fallback values keep local file:// previews usable when fetch is blocked.
const FALLBACK_CAMERA_DATA = {
  logan_pass: {
    status: "local preview",
    timestamp_utc: "",
    vehicle_count: 0,
    detected_vehicle_count: 0,
    current_queue: 0,
    peak_queue_today: 0,
    current_queue_by_lane: null,
    parking_spots_total: 100,
    parking_spots_available: 100,
    incoming_count: 0,
    exiting_count: 0,
    average_dwell_time_minutes: null,
    message: "Local preview mode",
  },
  west_entrance: {
    status: "local preview",
    timestamp_utc: "",
    vehicle_count: 0,
    detected_vehicle_count: 0,
    current_queue: 0,
    peak_queue_today: 0,
    current_queue_by_lane: { left_lane: 0, right_lane: 0 },
    incoming_count: 0,
    exiting_count: 0,
    average_dwell_time_minutes: null,
    message: "Local preview mode",
  },
  apgar_village: {
    status: "local preview",
    timestamp_utc: "",
    vehicle_count: 0,
    detected_vehicle_count: 0,
    current_queue: 0,
    peak_queue_today: 0,
    current_queue_by_lane: null,
    incoming_count: 0,
    exiting_count: 0,
    average_dwell_time_minutes: null,
    message: "Local preview mode",
  },
};

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

function formatLocalTimestamp(value) {
  if (!value) return "";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
    hour: "numeric",
    minute: "2-digit",
    hour12: true,
  }).format(date);
}

function formatMinutes(value) {
  if (value === null || value === undefined || value === "") return "Calculating...";
  const num = Number(value);
  if (!Number.isFinite(num)) return "Calculating...";
  return `${num.toFixed(num % 1 === 0 ? 0 : 1)} min`;
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

function getCameraKey() {
  return document.body.dataset.camera || null;
}

function getCameraConfig(cameraKey) {
  return cameraKey ? GLACIER_CAMERAS[cameraKey] : null;
}

function getImageElement() {
  return document.getElementById("webcam-image") || document.querySelector("[data-camera-image]");
}

function getCountElements() {
  return {
    incoming: document.getElementById("incoming-count"),
    exiting: document.getElementById("exiting-count"),
    dwell: document.getElementById("dwell-time"),
    totalParking: document.getElementById("total-parking-spots"),
  };
}

function currentQueue(json) {
  return Number.isFinite(json.current_queue) ? json.current_queue : (json.vehicle_count ?? json.detected_vehicle_count ?? 0);
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

function laneSummary(json) {
  return json.current_queue_by_lane || null;
}

async function fetchJson(path) {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) throw new Error(`Unable to load ${path}: ${response.status}`);
  return response.json();
}

async function fetchText(path) {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) throw new Error(`Unable to load ${path}: ${response.status}`);
  return response.text();
}

async function loadCameraSnapshot(cameraKey) {
  const camera = getCameraConfig(cameraKey);
  if (!camera) throw new Error(`Unknown camera: ${cameraKey}`);

  if (window.location.protocol === "file:") {
    return {
      json: FALLBACK_CAMERA_DATA[cameraKey],
      feedText: "",
      historyRows: [],
      isFallback: true,
    };
  }

  try {
    const [json, feedText, historyText] = await Promise.all([
      fetchJson(camera.jsonPath),
      fetchText(camera.feedPath),
      fetchText(camera.feedPath.replace("_feed.txt", "_history.jsonl")).catch(() => ""),
    ]);
    return {
      json,
      feedText,
      historyRows: parseHistory(historyText),
      isFallback: false,
    };
  } catch (error) {
    console.info(`Fallback used for ${cameraKey}:`, error);
    return {
      json: FALLBACK_CAMERA_DATA[cameraKey],
      feedText: "",
      historyRows: [],
      isFallback: true,
    };
  }
}

function updateImage(camera) {
  const imageEl = getImageElement();
  if (!imageEl) return;
  // Bust cache so GitHub-hosted webcam images refresh on every poll.
  imageEl.src = `${camera.imagePath}?v=${Date.now()}`;
  imageEl.alt = `${camera.title} annotated webcam image`;
}

function updateCameraHeader(camera, json) {
  const titleEl = document.querySelector("[data-camera-title]");
  const subtitleEl = document.querySelector("[data-camera-subtitle]");
  const labelEl = document.querySelector("[data-camera-label]");
  const timestampEl = document.querySelector("[data-camera-timestamp]");
  const sourceEl = document.querySelector("[data-camera-source]");
  const syncEl = document.querySelector("[data-sync-text]");

  if (titleEl) titleEl.textContent = camera.title;
  if (subtitleEl) {
    subtitleEl.textContent =
      camera.key === "logan_pass"
        ? "Live vehicle counts and dwell time for the Logan Pass lot."
        : camera.key === "west_entrance"
          ? "Live incoming and exiting vehicle counts with the latest annotated image."
          : "Latest annotated view and vehicle count.";
  }
  if (labelEl) labelEl.textContent = json.camera_label || camera.title;
  if (timestampEl) timestampEl.textContent = formatUtc(json.timestamp_utc);
  if (sourceEl) {
    sourceEl.href = camera.webcamPageUrl;
    sourceEl.textContent = "Open source webcam page";
  }
  if (syncEl) syncEl.textContent = json.timestamp_utc ? `Updated ${formatUtc(json.timestamp_utc)}` : "Local preview";
}

function updateWestMetrics(json) {
  const { incoming, exiting } = getCountElements();
  const incomingCount = json.incoming_count ?? json.current_queue ?? json.vehicle_count ?? json.detected_vehicle_count ?? 0;
  const exitingCount = json.exits ?? json.exiting_count ?? 0;
  if (incoming) incoming.textContent = String(incomingCount);
  if (exiting) exiting.textContent = String(exitingCount);
}

function updateLoganMetrics(json) {
  const { incoming, dwell, totalParking } = getCountElements();
  const queueCount = json.current_queue ?? json.vehicle_count ?? json.detected_vehicle_count ?? 0;
  if (incoming) incoming.textContent = String(queueCount);
  if (dwell) dwell.textContent = formatMinutes(json.average_dwell_time_minutes);
  if (totalParking) totalParking.textContent = String(json.parking_spots_total ?? "--");
}

function updateGenericMetrics(json, camera, historyRows) {
  const feedCurrentQueue = document.querySelector("[data-feed-current-queue]");
  const feedPeakQueue = document.querySelector("[data-feed-peak-queue]");
  const feedLaneQueue = document.querySelector("[data-feed-lane-queue]");
  const feedParking = document.querySelector("[data-feed-parking]");
  const feedStatus = document.querySelector("[data-feed-status]");
  const feedTimestamp = document.querySelector("[data-feed-timestamp]");
  const feedMessage = document.querySelector("[data-feed-message]");
  const feedImageUrl = document.querySelector("[data-feed-image-url]");
  const primaryQueue = currentQueue(json);

  if (feedStatus) feedStatus.textContent = json.status || "ok";
  if (feedTimestamp) feedTimestamp.textContent = json.timestamp_utc ? formatUtc(json.timestamp_utc) : "--";
  if (feedMessage) feedMessage.textContent = json.message || "--";
  if (feedImageUrl) feedImageUrl.textContent = json.image_url || "--";

  if (camera.key === "west_entrance") {
    if (feedCurrentQueue) feedCurrentQueue.textContent = String(json.incoming_count ?? primaryQueue);
    if (feedPeakQueue) feedPeakQueue.textContent = String(json.exits ?? json.exiting_count ?? 0);
    const lanes = laneSummary(json);
    if (feedLaneQueue) {
      feedLaneQueue.textContent = lanes
        ? Object.entries(lanes).map(([k, v]) => `${k}: ${v}`).join(" | ")
        : "Not split";
    }
    if (feedParking) feedParking.textContent = "";
  } else if (camera.key === "logan_pass") {
    if (feedCurrentQueue) feedCurrentQueue.textContent = String(primaryQueue);
    if (feedPeakQueue) feedPeakQueue.textContent = String(peakQueue(json, historyRows));
    if (feedLaneQueue) feedLaneQueue.textContent = "Not needed";
    if (feedParking) {
      const parking = availableParking(json, camera);
      feedParking.textContent = Number.isFinite(parking) ? String(parking) : "N/A";
    }
  } else {
    if (feedCurrentQueue) feedCurrentQueue.textContent = String(primaryQueue);
    if (feedPeakQueue) feedPeakQueue.textContent = String(peakQueue(json, historyRows));
    if (feedLaneQueue) feedLaneQueue.textContent = "Not needed";
    if (feedParking) feedParking.textContent = "";
  }
}

function updatePageData(cameraKey, json, feedText, historyRows, isFallback) {
  const camera = getCameraConfig(cameraKey);
  if (!camera) return;

  updateCameraHeader(camera, json);
  updateImage(camera);
  updateGenericMetrics(json, camera, historyRows);

  if (cameraKey === "west_entrance") {
    updateWestMetrics(json);
  } else if (cameraKey === "logan_pass") {
    updateLoganMetrics(json);
  }

  const errorEl = document.querySelector("[data-error]");
  if (errorEl) {
    if (isFallback) {
      errorEl.style.display = "block";
      errorEl.textContent = "Local preview mode: live JSON fetch is unavailable, so fallback data is shown.";
    } else {
      errorEl.style.display = "none";
      errorEl.textContent = "";
    }
  }

  document.querySelectorAll("[data-nav-camera]").forEach((link) => {
    link.setAttribute("aria-current", link.dataset.navCamera === cameraKey ? "page" : "false");
  });

  return { json, feedText, historyRows };
}

async function refreshCameraPage(cameraKey) {
  const snapshot = await loadCameraSnapshot(cameraKey);
  updatePageData(cameraKey, snapshot.json, snapshot.feedText, snapshot.historyRows, snapshot.isFallback);
}

async function renderOverview() {
  const entries = await Promise.all(
    CAMERA_ORDER.map(async (cameraKey) => {
      const camera = getCameraConfig(cameraKey);
      const snapshot = await loadCameraSnapshot(cameraKey);
      return { camera, ...snapshot };
    })
  );

  for (const entry of entries) {
    if (!entry.camera) continue;
    const card = document.querySelector(`[data-summary-card="${entry.camera.key}"]`);
    if (!card) continue;
    const queueEl = card.querySelector("[data-summary-current]");
    const peakEl = card.querySelector("[data-summary-peak]");
    const timeEl = card.querySelector("[data-summary-time]");
    const imageEl = card.querySelector("[data-summary-image]");
    const linkEl = card.querySelector("[data-summary-link]");
    if (queueEl) queueEl.textContent = String(currentQueue(entry.json));
    if (peakEl) peakEl.textContent = String(peakQueue(entry.json, entry.historyRows));
    if (timeEl) timeEl.textContent = formatUtc(entry.json.timestamp_utc);
    if (imageEl) imageEl.src = `${entry.camera.imagePath}?v=${Date.now()}`;
    if (linkEl) linkEl.href = entry.camera.page;
  }
}

function wireHomeLinks() {
  document.querySelectorAll("[data-home-link]").forEach((link) => {
    link.href = "index.html";
  });
}

async function initGlacierSite() {
  wireHomeLinks();

  const view = document.body.dataset.view;
  if (view === "overview") {
    renderOverview().catch((error) => console.error(error));
    return;
  }

  const cameraKey = getCameraKey();
  if (!cameraKey || !getCameraConfig(cameraKey)) return;

  await refreshCameraPage(cameraKey);
  // Re-poll the feed and image every 5 minutes without a full page reload.
  setInterval(() => {
    refreshCameraPage(cameraKey).catch((error) => console.error(error));
  }, REFRESH_INTERVAL_MS);
}

document.addEventListener("DOMContentLoaded", initGlacierSite);
