#### merging nps data for congestion management database

# loading the libraries I often work with
library(readxl)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras) # addHeatmap

# establishing my base directory rather than actually changing my directory, I just find this easier
p_file_path <- "/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/EagleLake/"

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Load all processed Park Loop Rd Files --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# load all files in the folder, excluding the log
file_paths <- list.files(p_file_path, pattern = "cv_output")
ls_files <- lapply(file_paths, function(x) {
  in_file <- fread(paste0(p_file_path, x))
})
names(ls_files) <- gsub("cv_output|.ts.csv", "", file_paths)
stacked_eglake <- rbindlist(ls_files, idcol = "source_file", fill = T)

# load the gps pings
in_gps_pings <- fread("/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/gps_cleaned_2_4_Eagle_Lake.csv")

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Peg a vehicle as in right side parking  --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

veh_classes <- c("truck", "car", "bus")
stacked_eglake[between(cx, 1250, 2200) & between(cy, 500, 1050) & class %in% veh_classes, veh_on_right := 1] # capturing the right side
stacked_eglake[is.na(veh_on_right), veh_on_right := 0]

# there are so many little videos, and Ali's moving and the window is small, so
# i'm going to just call a unique vehicle by sourcefile + id without any pizzaz
stacked_eglake[, unique_vehicle := paste0(source_file, "_", id)]

# pull start time from file name, assume it's twelve hours off
stacked_eglake[, temp_timestamp := gsub("^20250828|20250829|15_F$", "", source_file)]
# subtract 12
stacked_eglake[, temp_timestamp := as.numeric(temp_timestamp)]
# format as time
stacked_eglake[, temp_timestamp := stringr::str_pad(as.character(temp_timestamp), 6, "left", "0")]
stacked_eglake[, true_timestamp := as.POSIXct(paste0("2025-08-26 ", substr(temp_timestamp, 1, 2), ":", substr(temp_timestamp, 3, 4), 
                                                       ":", substr(temp_timestamp, 5, 6), " EST"), tz = "EST") ]
# increment time for the afternoon stuff
stacked_eglake[temp_timestamp > 120000, true_timestamp := true_timestamp - 12*60*60]
stacked_eglake[temp_timestamp < 120000, true_timestamp := true_timestamp + 12*60*60]

# increment to the point in the video
stacked_eglake[ , true_timestamp := true_timestamp + timestamp]

# total count vehicles park on right in this section of video
stacked_eglake[veh_on_right == 1, uniqueN(unique_vehicle)]
# with decent confidence, most of them that seems good
stacked_eglake[veh_on_right == 1 & confidence_numeric > 0.5, uniqueN(unique_vehicle)]
stacked_eglake[confidence_numeric > 0.5, .(uniqueN(unique_vehicle)), class]

# lets consider the location to be the middle time it's in the box
r_side_vehicles <- stacked_eglake[veh_on_right == 1 & confidence_numeric > 0.5]
r_side_vehicles[, recog_sequence := 1:.N, unique_vehicle]
r_side_vehicles[, middle_of_sequence := floor(median(recog_sequence)), unique_vehicle]
r_side_vehicles <- r_side_vehicles[recog_sequence == middle_of_sequence]

# is vehicle moving westbound or eastbound? flag that, important for outcomes
in_gps_pings[, long_diff := longitude - shift(longitude, n = 1, type = "lag")]
in_gps_pings[long_diff > 0, tempdirection := 1]
in_gps_pings[long_diff < 0, tempdirection := -1]
# for the zeros, populate whatever is before is
setnafill(in_gps_pings, type="locf", cols=c("tempdirection"))
in_gps_pings[tempdirection == -1, direction := "EB"]
in_gps_pings[tempdirection == 1, direction := "WB"]
in_gps_pings[, tempdirection := NULL]

# merge on coordinates
in_gps_pings[, filename := gsub(".ts", "", filename)]
setnames(in_gps_pings, c("filename", "timestamp_sec"), c("source_file", "timestamp"))
vehicles_w_coords <- merge.data.table(r_side_vehicles, in_gps_pings, c("source_file", "timestamp"), all.x = T)

# about 14% here in eagle lake merged on naturally, that's cool. others need a bit more rounding to make it, do a rolling merge
setkeyv(r_side_vehicles, c("source_file", "timestamp"))
setkeyv(in_gps_pings, c("source_file", "timestamp"))
vehicles_w_coordsRoll <- in_gps_pings[r_side_vehicles, roll = "nearest" ]

# filter to coordinates in our target area
vehicles_w_coordsRoll <- vehicles_w_coordsRoll[longitude < -68.2376844 & longitude > -68.256201 & 
                                               latitude < 44.3780] # ensure not in the parking lot

# Group identical coordinates and count occurrences
coords_agg <- vehicles_w_coordsRoll[, .(count = .N), by = .(latitude, longitude)]

# Create a leaflet map centered on mean location
center_lat <- mean(coords_agg$lat, na.rm = TRUE)
center_long <- mean(coords_agg$long, na.rm = TRUE)

# drop records of vehicles outside of park loop road
coords_agg <- coords_agg[!is.na(latitude)]

m <- leaflet(coords_agg) %>%
  addTiles() %>%
  setView(lng = center_long, lat = center_lat, zoom = 14) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude, intensity = ~count,
    blur = 3, radius = 8, max = max(coords_agg$count, na.rm = TRUE)
  )  %>%
  addCircleMarkers(radius = count)

# Print map (in RStudio Viewer or browser)
m

# okay this definitely needs manual editting, I'll do it on the export

# export csv to load into arcgis
write.csv(vehicles_w_coordsRoll,
          "/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/EagleLake/vehicles_w_gps.csv",
          row.names = F)

