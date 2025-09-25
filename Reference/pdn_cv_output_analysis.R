# analysis of PDN border crossing output
# load libraries
library(data.table)
library(ggplot2)

# import computer vision output, toggle between different files for different samples
# YOUR FILE PATH HERE
in_dt <- fread("./code/Border-Bottleneck-Management/support_scripts/captured_coords/data_download_export_07132025_0745.csv")

# remove the nonnumeric characters from the confidence
in_dt[, confidence := as.numeric(gsub("tensor\\(", "", gsub("\\)", "", confidence)))]

# preview data
plot(in_dt$id, in_dt$confidence)
plot(in_dt$timestamp, in_dt$confidence)

# exploratory visual
ggplot(in_dt, aes(x = timestamp, y = 1000-cy, color = id)) +
  geom_point()

#classify lane
in_dt[, lane := ifelse(cx < 400, "out_of_bounds", 
                       ifelse(cx < 940, "left_two", 
                              ifelse(cx < 1100, "middle",
                                     ifelse(cx < 1450, "right", "out_of_bounds"))))]

# classify movement forward
in_dt[, forward := ifelse(cy > 960, "early", 
                          ifelse(cy > 590, "in_zone", "past_25m_segment"))]

# what's the current run length I pulled, in seconds
print(paste0(in_dt[, max(timestamp)/1000], " seconds"))

# let's take a peek at which lanes all the points are collected from
ggplot(in_dt, aes(x = cx, y = 1000-cy, color = forward)) +
  geom_point()

# let's visualize the first id location of all vehicles in this pull
in_dt[, first_id := min(timestamp), .(id)] # dropped class for sample rate
first_ids_dt <- in_dt[timestamp == first_id]
# ids are first recognized at almost any point in the frame
ggplot(first_ids_dt, aes(x = cx, y = 1080-cy, color = forward)) +
  labs(x = "X Pixel Location", y = "Y Pixel Location", color = "Position relative to benchmark") +
  geom_point() +
  theme_light() +
  theme(legend.position = "bottom")

# get earliest timestamp in zone
in_dt[, earliest_in_subsection := min(timestamp), .(id, forward)] # dropped class for sample rate
highlights_dt <- in_dt[timestamp == earliest_in_subsection]

# if the vehicle changes lanes, want to note that
highlights_dt[, all_lanes := paste0(unique(lane), collapse = " to "), id]

# also keep a count of how many times that object was identified as a sort of sanity check
wide_vehicle_timestamps <- dcast.data.table(highlights_dt, id + all_lanes + first_id ~ forward, value.var = "earliest_in_subsection") #dropped class for sample rate

# vehicles successfully tracked: started out in "early", wasn't out of bounds, made it eventually to "past 25 meter segment"
# then just scale this time to the length of the bridge which is about 400 meters
wide_vehicle_timestamps <- wide_vehicle_timestamps[!grepl("out_of_bounds", all_lanes)]
wide_vehicle_timestamps[!is.na(early) &! grepl("out_of_bounds", all_lanes), sec_to_span_25m := (past_25m_segment - in_zone) / 1000 ]
wide_vehicle_timestamps[, time_to_cross := sec_to_span_25m* 400/25] #400 meter long crossing, 25 meter measurement zone

# extract original lane, attribute crossing time to that and not to the lane they switched to
wide_vehicle_timestamps[, og_lane := gsub(" to .*", "", all_lanes)]

# lets flag hour long time periods, subtracting minutes into the hour I started and going from there 
hour_start <- (60 - 42)*60*1000
wide_vehicle_timestamps[, label_hour := ifelse(first_id < hour_start, "5am", 
                                               ifelse(first_id < (hour_start + (60*60*1000)), "6am", 
                                               ifelse(first_id < (hour_start + 2*(60*60*1000)), "7am", 
                                               ifelse(first_id < (hour_start + 3*(60*60*1000)), "8am", 
                                               "9pm"))))]

# our sample rate in the end, and related metrics
wide_vehicle_timestamps[!is.na(time_to_cross), .N]/wide_vehicle_timestamps[!(is.na(early) | is.na(in_zone)), .N]
wide_vehicle_timestamps[!is.na(time_to_cross), .N, og_lane]
wide_vehicle_timestamps[!(is.na(early) | is.na(in_zone)), .N, og_lane]
wide_vehicle_timestamps[, mean(time_to_cross, na.rm = T)/60, .(og_lane, label_hour)]
wide_vehicle_timestamps[, median(time_to_cross, na.rm = T)/60, og_lane]
wide_vehicle_timestamps[, min(time_to_cross, na.rm = T)/60, og_lane]
wide_vehicle_timestamps[, max(time_to_cross, na.rm = T)/60, og_lane]

### SAMPLE VALIDATION VS BLUETOOTH/WIFI DEVICE DETECTION, JULY 11-14

# load dataset from TTI
#YOUR FILE PATH HERE
in_tti_dt <- as.data.table(readxl::read_excel("./code/Border-Bottleneck-Management/PDN-July-11-Combined-v1.xls"))

# flag overlapping with my time period
# i'm assuming the from/to readerid are distinguishing between "BT3B" Bluetooth and "BT3W" Wifi
in_tti_dt[, flag_period := "general"]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-11 10:18:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-11 12:22:00", tz = "GMT"),  flag_period := "jul11morning"]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-11 11:00:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-11 12:00:00", tz = "GMT"),  flag_period := "jul11morning11"]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-13 05:42:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-13 09:09:00", tz = "GMT") , flag_period := "jul13morning" ]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-13 06:00:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-13 07:00:00", tz = "GMT") , flag_period := "jul13morning6" ]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-13 07:0:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-13 08:0:00", tz = "GMT") , flag_period := "jul13morning7" ]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-13 08:0:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-13 09:00:00", tz = "GMT") , flag_period := "jul13morning8" ]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-14 06:55:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-14 08:57:00", tz = "GMT"), flag_period := "jul14morning" ]
in_tti_dt[`Upstream Timestamp` >= as.POSIXct("2025-07-14 07:00:00", tz = "GMT") & 
          `Downstream Timestamp` <= as.POSIXct("2025-07-14 08:00:00", tz = "GMT"), flag_period := "jul14morning7" ]

in_tti_dt[, .(count = .N, crossing_time = mean(`Travel Time (Seconds)`)/60, 
              med_time = median(`Travel Time (Seconds)`)/60,
              min_time = min(`Travel Time (Seconds)`)/60,
              max_time = max(`Travel Time (Seconds)`)/60), flag_period]

### SOME EXPLORATORY VISUALIZATION OF OUR SAMPLE

# visualize first id spot: ids are first recognized at almost any point in the frame
ggplot(first_ids_dt[id %in% wide_vehicle_timestamps[!is.na(time_to_cross)]$id], aes(x = cx, y = 1080-cy, color = forward)) +
  labs(x = "X Pixel Location", y = "Y Pixel Location", color = "Position relative to benchmark") +
  geom_point()

# let's visualize the functional sample -- just the earlier times
ggplot(in_dt[timestamp < 2200000 & id %in% wide_vehicle_timestamps[!is.na(time_to_cross)]$id], aes(x = timestamp, y = 1080-cy, color = lane)) +
  geom_point()
# to see how the lane classification is working
ggplot(in_dt[id %in% wide_vehicle_timestamps[!is.na(time_to_cross)]$id], aes(x = cx, y = 1080-cy, color = lane)) +
  geom_point()

# colored by individual ids
ggplot(in_dt[id %in% wide_vehicle_timestamps[!is.na(time_to_cross) & past_25m_segment < 1200000]$id], aes(x = cx, y = 1080-cy, color = as.factor(id))) +
  geom_point() +
  theme(
    legend.position = "none"
  ) +
  labs(x = "X Pixel Location", y = "Y Pixel Location", color = "Position relative to benchmark") +
  geom_point() +
  theme_light() +
  theme(legend.position = "none")

# and non functional sample
ggplot(in_dt[timestamp < 100000 & lane != "out_of_bounds" &! id %in% wide_vehicle_timestamps[!is.na(time_to_cross)]$id], aes(x = cx, y = 1080-cy, color = as.factor(id))) +
  geom_point() +
  theme(
    legend.position = "none"
  )



