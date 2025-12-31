#### merging nps data for congestion management database

# loading the libraries I often work with
library(readxl)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras) # addHeatmap

# establishing my base directory rather than actually changing my directory, I just find this easier
p_file_path <- "/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/ParkLoopRd//"

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Load all processed Park Loop Rd Files --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# load all files in the folder, excluding the log
file_paths <- list.files(p_file_path, pattern = "cv_output")
ls_files <- lapply(file_paths, function(x) {
  in_file <- fread(paste0(p_file_path, x))
})
names(ls_files) <- gsub("cv_output|.ts.csv", "", file_paths)
stacked_parkloop <- rbindlist(ls_files, idcol = "source_file", fill = T)


# load the gps pings
in_gps_pings <- fread("/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/4- Data Collection/Video Data/GPS Data/ParkLoop_dashcam_gps_cleaned.csv")

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Peg a vehicle as in right side parking  --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

veh_classes <- c("truck", "car", "bus")
stacked_parkloop[between(cx, 1550, 1950) & between(cy, 950, 1250) & class %in% veh_classes, veh_on_right := 1]
stacked_parkloop[is.na(veh_on_right), veh_on_right := 0]

# there are so many little videos, and Ali's moving and the window is small, so
# i'm going to just call a unique vehicle by sourcefile + id without any pizzaz
stacked_parkloop[, unique_vehicle := paste0(source_file, "_", id)]

# pull start time from file name, assume it's twelve hours off
stacked_parkloop[, temp_timestamp := gsub("^20250826|20250827|15_F$", "", source_file)]
# subtract 12
stacked_parkloop[, temp_timestamp := as.numeric(temp_timestamp)]
# format as time
stacked_parkloop[, temp_timestamp := stringr::str_pad(as.character(temp_timestamp), 6, "left", "0")]
stacked_parkloop[, true_timestamp := as.POSIXct(paste0("2025-08-26 ", substr(temp_timestamp, 1, 2), ":", substr(temp_timestamp, 3, 4), 
                                                  ":", substr(temp_timestamp, 5, 6), " EST"), tz = "EST") ]
# increment time for the afternoon stuff
stacked_parkloop[temp_timestamp > 120000, true_timestamp := true_timestamp - 12*60*60]
stacked_parkloop[temp_timestamp < 120000, true_timestamp := true_timestamp + 12*60*60]

# increment to the point in the video
stacked_parkloop[ , true_timestamp := true_timestamp + timestamp]

# total count vehicles park on right in this section of video
stacked_parkloop[veh_on_right == 1, uniqueN(unique_vehicle)]
# with decent confidence, most of them that seems good
stacked_parkloop[veh_on_right == 1 & confidence_numeric > 0.5, uniqueN(unique_vehicle)]
stacked_parkloop[confidence_numeric > 0.5, .(uniqueN(unique_vehicle)), class]

# lets consider the location to be the middle time it's in the box
r_side_vehicles <- stacked_parkloop[veh_on_right == 1 & confidence_numeric > 0.5]
r_side_vehicles[, recog_sequence := 1:.N, unique_vehicle]
r_side_vehicles[, middle_of_sequence := floor(median(recog_sequence)), unique_vehicle]
r_side_vehicles <- r_side_vehicles[recog_sequence == middle_of_sequence]
            
# merge on coordinates
in_gps_pings[, filename := gsub(".ts", "", filename)]
setnames(in_gps_pings, c("filename", "timestamp_sec"), c("source_file", "timestamp"))
vehicles_w_coords <- merge.data.table(r_side_vehicles, in_gps_pings, c("source_file", "timestamp"), all.x = T)

# about 60% merged on naturally, that's cool. others need a bit more rounding to make it, do a rolling merge
setkeyv(r_side_vehicles, c("source_file", "timestamp"))
setkeyv(in_gps_pings, c("source_file", "timestamp"))
vehicles_w_coordsRoll <- in_gps_pings[r_side_vehicles, roll = "nearest" ]

# Group identical coordinates and count occurrences
coords_agg <- vehicles_w_coordsRoll[, .(count = .N), by = .(latitude, longitude)]

# Create a leaflet map centered on mean location
center_lat <- mean(coords_agg$lat, na.rm = TRUE)
center_long <- mean(coords_agg$long, na.rm = TRUE)

m <- leaflet(coords_agg) %>%
  addTiles() %>%
  setView(lng = center_long, lat = center_lat, zoom = 14) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude, intensity = ~count,
    blur = 15, radius = 12, max = max(coords_agg$count, na.rm = TRUE)
  )  %>%
  addCircleMarkers(radius = count)

# Print map (in RStudio Viewer or browser)
m

# export csv to load into arcgis
write.csv(vehicles_w_coordsRoll,
          "/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/ParkLoopRd/vehicles_w_gps_morning.csv",
          row.names = F)

