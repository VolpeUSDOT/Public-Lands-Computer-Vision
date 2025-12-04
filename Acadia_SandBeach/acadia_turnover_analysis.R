# metrics with acadia parking data

# loading the libraries I often work with
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(lubridate)
library(viridis)

# establishing my base directory rather than actually changing my directory, I just find this easier
p_file_path <- "~/acadia_parking_data_clean.csv"

# base date because time processing just needs a date there
p_base_date <- as.Date("2025-09-29")

#in data
in_clean <- fread(p_file_path)

#initial table
in_clean[vehicleID != "-", .(count_veh = .N, count_spots = uniqueN(spotID)), lot][, .(veh_per_spot = count_veh/count_spots), .(lot, count_veh, count_spots)]

# copy to remove records of empty parking spots for the following analysis
filled_spots_only <- copy(in_clean[vehicleID != "-"])

# order by time out, number order by parking spots
filled_spots_only[, spot_index := 1:.N, .(lot, spotID)]

# if no spotType is specified, let's call it standard
filled_spots_only[is.na(spotType), spotType := "standard"]

# If any assumedTimeOut is NA and you want to show until end-of-day window:
# i had to hack the time zone, I couldn't figure out the time zone conversion from UTC to EDT properly
filled_spots_only[ , `:=` (present_at_start = 0, present_at_end = 0)] 
filled_spots_only[is.na(assumedTimeIn), present_at_start := 1] 
filled_spots_only[is.na(assumedTimeIn), assumedTimeIn := as.POSIXct(paste(p_base_date, "05:00"))] 
filled_spots_only[is.na(assumedTimeOut), present_at_end := 1]
filled_spots_only[is.na(assumedTimeOut), assumedTimeOut := as.POSIXct(paste(p_base_date, "12:00"))]

# drop extra columns, they're redundant/a bit confusing at this point
filled_spots_only[, c("inPreciseTime", "outPreciseTime", "time_in", "time_out", "checked") := NULL]

# estimated length of stay]
filled_spots_only[, length_of_stay := difftime(assumedTimeOut, assumedTimeIn, units = "mins")]

#summary table 1: average stay per spot type per lot. do it both with and without edge case cars
avg_stay_length <- filled_spots_only[, .(firstquartile_stay_minutes = quantile(length_of_stay, c(0.25)),
                                         median_stay_minutes = quantile(length_of_stay, c(0.5)),
                                         avg_stay_minutes = mean(length_of_stay),
                                         lastquartile_stay_minutes = quantile(length_of_stay, c(0.75)), count_vehicles = .N),
                                     .(lot, spotType, present_at_start, present_at_end)]
avg_stay_length[, hour_parked := "all"]

# If any assumedTimeOut is NA and you want to show until end-of-day window:
# i had to hack the time zone, I couldn't figure out the time zone conversion from UTC to EDT properly
filled_spots_only[, hour_parked := hour(assumedTimeIn)]
filled_spots_only[, hour_departed := hour(assumedTimeOut)]
#write.csv(filled_spots_only, "~/parking_lots_all_filled_spots.csv" , row.names = F)

  # THIS WHOLE INDENTED SECTION REALLY OUGHT TO BE IN A DIFFERENT SCRIPT OR FORMATTED BETTER AT LEAST
  # Required packages
  library(scales)       # for nice time labels on x-axis (optional)
  dt <- copy(filled_spots_only)
  # Parse times: ignore date, use a dummy date (1970-01-01)
  # Guarantee POSIXct class with same date so comparisons work
  parse_time_only <- function(x) {
    # some rows may already have quotes or multiple formats; try ymd_hms first
    parsed <- ymd_hms(x, quiet = TRUE)
    # if NA, try parsing as POSIXct directly
    ifelse(is.na(parsed), as.POSIXct(x, tz = "UTC"), parsed)
  }
  # Use lubridate::ymd_hms but keep only the time on a fixed date:
  dt[, time_in := {
    t <- ymd_hms(assumedTimeIn, quiet = TRUE)
    # if parse failed for some, fallback to as.POSIXct
    t[is.na(t)] <- as.POSIXct(assumedTimeIn[is.na(t)], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    # normalize to dummy date
    as.POSIXct(sprintf("1970-01-01 %02d:%02d:%02d", hour(t), minute(t), second(t)), tz = "UTC")
  }]
  dt[, time_out := {
    t <- ymd_hms(assumedTimeOut, quiet = TRUE)
    t[is.na(t)] <- as.POSIXct(assumedTimeOut[is.na(t)], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    as.POSIXct(sprintf("1970-01-01 %02d:%02d:%02d", hour(t), minute(t), second(t)), tz = "UTC")
  }]
  
  # In case some records have time_out earlier than time_in (unlikely given dataset) 
  # or have exact equality, we can choose to treat [in, out) and drop zero-length stays:
  dt <- dt[time_out > time_in]
  
  # If there are rows with times outside our 09:00-16:00 window, they still can overlap the intervals;
  # we'll build intervals and the join will take care of filtering.
  
  # Build 15-minute interval start times between 09:00 (inclusive) and 16:00 (exclusive end slot start):
  start_datetime <- as.POSIXct("1970-01-01 09:00:00", tz = "UTC")
  end_datetime   <- as.POSIXct("1970-01-01 16:00:00", tz = "UTC")  # last interval starts at 15:45
  slot_starts <- seq(from = start_datetime, to = end_datetime, by = "15 min")
  # If you want the last interval to start at 15:45 and include up to 16:00, ensure end is 15:45.
  slot_starts <- slot_starts[slot_starts <= (end_datetime)] 
  
  slots <- data.table(
    slot_start = slot_starts,
    slot_end   = slot_starts + minutes(15)
  )
  # Give each slot an index for plotting and left-join convenience
  slots[, slot_index := .I]
  
  # Prepare dt for non-equi join: keep lot, spotID, time_in, time_out
  setkey(dt, time_in, time_out)
  
  # Compute total distinct spots per lot (denominator)
  lot_sizes <- unique(dt[, .(lot, spotID)])[, .(total_spots = .N), by = lot]
  # manually correct lot sizes
  lot_sizes[lot == "el lake", total_spots := 8]
  lot_sizes[lot == "el road", total_spots := 28]
  lot_sizes[lot == "sbl", total_spots := 101]
  lot_sizes[lot == "sbu", total_spots := 22]
  lot_sizes[lot == "jphsouth", total_spots := 71]
  lot_sizes[lot == "jphnorth", total_spots := 185]
  
  # Do a non-equi join (find records where intervals overlap)
  # Overlap condition for [in, out) vs [slot_start, slot_end):
  # overlap if time_in < slot_end AND time_out > slot_start
  # We'll join by no keys but using a cross join-like approach via data.table binary search:
  # Efficient approach: set keys on time_in and then find candidate records with time_in < slot_end,
  # then filter the other condition. We'll use foverlaps-style pattern by creating interval columns on both.
  # Build dt intervals table compatible with foverlaps:
  dt_intervals <- copy(dt)
  dt_intervals[, rec_start := time_in]
  dt_intervals[, rec_end   := time_out]
  
  # For foverlaps, interval tables must have keys with start <= end
  setkey(dt_intervals, rec_start, rec_end)
  setkey(slots, slot_start, slot_end)
  
  # foverlaps returns rows where intervals intersect (by default it finds overlap)
  overlaps <- foverlaps(slots, dt_intervals, by.x = c("slot_start", "slot_end"),
                        by.y = c("rec_start", "rec_end"), nomatch = 0L)
  # overlaps contains slot info + the matching record rows
  
  # Now count distinct spotIDs per lot per slot
  occupancy_slot <- overlaps[, .(occupied_spots = uniqueN(spotID)), by = .(lot, slot_start, slot_index)]
  # Make sure every lot-slot pair exists, filling zeros where missing
  lots <- unique(dt$lot)
  all_combinations <- CJ(lot = lots, slot_index = slots$slot_index)
  all_combinations <- merge(all_combinations, slots[, .(slot_index, slot_start)], by = "slot_index", all.x = TRUE)
  
  occupancy_full <- merge(all_combinations, occupancy_slot, by = c("lot", "slot_start", "slot_index"), all.x = TRUE)
  occupancy_full[is.na(occupied_spots), occupied_spots := 0L]
  
  # Add lot sizes and compute percent occupancy
  occupancy_full <- merge(occupancy_full, lot_sizes, by = "lot", all.x = TRUE)
  occupancy_full[, percent_occupied := (occupied_spots / total_spots) * 100]
  
  # now i want to add on a column of how many vehicles are entering the lot
  # this comes from another dataset...
  in_entries <- data.table(read_xlsx("/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/4- Data Collection/all_lot_vehicles_entered.xlsx"))
  # Parse times: ignore date, use a dummy date (1970-01-01)
  # Guarantee POSIXct class with same date so comparisons work
  # Use lubridate::ymd_hms but keep only the time on a fixed date:
  in_entries[, slot_start := {
    t <- ymd_hms(time, quiet = TRUE)
    # if parse failed for some, fallback to as.POSIXct
    t[is.na(t)] <- as.POSIXct(time[is.na(t)], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    # normalize to dummy date
    as.POSIXct(sprintf("1970-01-01 %02d:%02d:%02d", hour(t), minute(t), second(t)), tz = "UTC")
  }]
  in_entries[, time := NULL]
  
  # For plotting, create a POSIXct x variable (slot_start is already POSIXct)
  plot_dt <- occupancy_full[order(lot, slot_start)]
  plot_dt <- merge.data.table(plot_dt, in_entries, by = c("lot", "slot_start"), all = T)
  plot_dt[, percent_visited := (ever_entered/total_spots) * 100]
  
  # make lot names into a factor to force plot order
  plot_lot_order <- c("Sand Beach, Upper Lot", "Jordan Pond House, South", "Eagle Lake, Lakeside",           
                      "Sand Beach, Lower Lot", "Jordan Pond House, North", "Eagle Lake, Roadside")
  plot_dt[, lot_name_full := factor(lot, 
                                    levels = c('sbu', 'jphsouth', 'el lake', 'sbl', 'jphnorth', 'el road'),
                                    labels = plot_lot_order)]
  
  # plot how full the lots are
  p <- ggplot(plot_dt, aes(x = slot_start, group = lot_name_full)) +
    geom_hline(aes(yintercept = 100), size = 0.7, linetype = "dotted") +
    geom_line(aes(y = percent_occupied), color = "#5e8fff", linewidth = 1) +
    geom_point(aes(y = percent_occupied), color = "#5e8fff", size = 1.2) +
    geom_line(data = plot_dt[!is.na(percent_visited)], aes(y = percent_visited), color = "#0052ba", linewidth = 1) +
    geom_point(data = plot_dt[!is.na(percent_visited)], aes(y = percent_visited), color = "#0052ba", size = 1.2) +
    facet_wrap(~ lot_name_full, ncol = 3, scales = "fixed") +     # scales="fixed" ensures axes are consistent across facets
    scale_y_continuous(
      labels = function(x) paste0(x, "%")                 # prints 0%, 20%, ..., 100%
    ) +
    scale_x_datetime(
      breaks = seq(as.POSIXct("1970-01-01 09:00:00", tz = "UTC"),
                   as.POSIXct("1970-01-01 16:00:00", tz = "UTC"),
                   by = "1 hour"),
      labels = function(x) as.integer(format(x, "%H")),   # prints 9, 10, 11, 12, 13, ...
      expand = expansion(add = c(0, 0))
    ) +
    labs(
      title = "Parking Lot Percent Occupancy (15-minute intervals)",
      x = "Time of day",
      y = "Percent Occupied"
    ) +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = 12)) +
    facet_wrap(~lot_name_full, scales = "free_y") +
    coord_cartesian(clip = "off")
  
  plot_dt_long <- melt.data.table(plot_dt, id.vars = c("lot_name_full", "slot_start", "total_spots"), 
                                  measure.vars = c("occupied_spots", "current_cars_in"), variable.name = "Vehicle Group", value.name = "cnt_vehicles" )
  plot_dt_long[, `Vehicle Group` := ifelse(`Vehicle Group` == "occupied_spots", "Vehicles Parked", "Vehicles In Lot")]
  # let's redo this plot with counts
  # and subtracting out the vehicles that have exited
  # plot how full the lots are
  p <- ggplot(plot_dt_long, aes(x = slot_start, group = `Vehicle Group`, color = `Vehicle Group`)) +
    geom_hline(aes(yintercept = total_spots), size = 0.7, linetype = "dotted") +
    geom_line(aes(y = cnt_vehicles), linewidth = 1) +
    geom_point(aes(y = cnt_vehicles), size = 1.2) +
    facet_wrap(~ lot_name_full, ncol = 3, scales = "fixed") +     # scales="fixed" ensures axes are consistent across facets
    scale_y_continuous(limits = c(0, NA))+
    scale_x_datetime(
      breaks = seq(as.POSIXct("1970-01-01 09:00:00", tz = "UTC"),
                   as.POSIXct("1970-01-01 16:00:00", tz = "UTC"),
                   by = "1 hour"),
      labels = function(x) as.integer(format(x, "%H")),   # prints 9, 10, 11, 12, 13, ...
      expand = expansion(add = c(0, 0))
    ) +
    labs(
      title = "Parking Lot Occupancy (15-minute intervals)",
      x = "Time of day",
      y = "Count Vehicles"
    ) +
    scale_color_manual(values = c("#0052ba", "#5e8fff"), name = "Vehicle Position") +
    theme_minimal() +
    theme(legend.position = "top", text = element_text(size = 12)) +
    facet_wrap(~lot_name_full, scales = "free_y") +
    coord_cartesian(clip = "off")
  print(p)
  
  ggsave(filename = "acad_lot_vehicle_activity.png", 
         plot = p, 
         path = paste0("/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/", "visuals/"),
         width = 10, height = 7)
  ggsave(filename = "acad_lot_driveway_activity.png", 
         plot = p_driveways, 
         path = paste0("/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/", "visuals/"),
         width = 10, height = 7)

  # let's make one more plot of driveway activity, as a doged bar chart
  dt_driveway_long <- melt.data.table(plot_dt, id.vars = c("lot_name_full", "slot_start", "total_spots"), 
                                      measure.vars = c("enter", "exit"), variable.name = "driveway_activity", value.name = "cnt_vehicles" )
  p_driveways <- ggplot(dt_driveway_long, aes(x = slot_start)) +
    geom_col(aes(y = cnt_vehicles, fill = driveway_activity), position = position_dodge()) +
    scale_x_datetime(
      breaks = seq(as.POSIXct("1970-01-01 09:00:00", tz = "UTC"),
                   as.POSIXct("1970-01-01 16:00:00", tz = "UTC"),
                   by = "1 hour"),
      labels = function(x) as.integer(format(x, "%H")),   # prints 9, 10, 11, 12, 13, ...
      expand = expansion(add = c(0, 0))
    ) +
    # # value labels just below the top of each column (inside bar)
    # geom_text(aes(y = cnt_vehicles, label = cnt_vehicles),
    #           position = position_dodge(),
    #           vjust = 1.1,                # >1 nudges text slightly downward from the top
    #           size = 3,                   # adjust size as needed
    #           color = "white") +          # change to "black" if bars are light
    # #add one label per facet; inherit.aes = FALSE so we control mappings here
    # geom_label(
    #   data = pct_parked_tbl,
    #   aes(x = Inf, y = Inf, label = paste0(pct_parked, "% Parked")),
    #   inherit.aes = FALSE,
    #   hjust = 1.05,            # nudge slightly inside from right edge
    #   vjust = 0.7,            # nudge above the line a bit
    #   size = 3
    # ) +
    theme_minimal() +
    theme(legend.position = "top", legend.direction = "horizontal", text = element_text(size = 12)) +
    labs(x = "Hour", y = "Count Vehicles") +
    facet_wrap(~lot_name_full, scales = "free_y") +
    scale_fill_manual(values = c("#0052ba", "#5e8fff"), name = "Vehicle Action in the Given Hour") +
    ggtitle("Lot Driveway Activity (15-minute intervals)") +
    coord_cartesian(clip = "off")
  

#summary table 1: average stay per spot type per lot. do it both with and without edge case cars
avg_stay_length_hourly <- filled_spots_only[, .(firstquartile_stay_minutes = quantile(length_of_stay, c(0.25)),
                                         median_stay_minutes = quantile(length_of_stay, c(0.5)),
                                         avg_stay_minutes = mean(length_of_stay),
                                         lastquartile_stay_minutes = quantile(length_of_stay, c(0.75)), count_vehicles = .N),
                                     .(lot, spotType, hour_parked, present_at_start, present_at_end)]
out_avg_stay <- rbind(avg_stay_length, avg_stay_length_hourly)
write.csv(out_avg_stay, "~/parking_lots_hourly_aggregated.csv" , row.names = F)

### DATA VISUALIZATION EXPLORATION

# pull out spot id details so we can facet
vis_data[, parkingRow := substr(spotID, 1, 1)]
vis_data[, parkingOrder := gsub("^.", "", spotID)]

# Ensure spotID is treated as categorical
vis_data$spotID <- factor(vis_data$spotID, 
                          levels = c(paste0("A", 15:1), paste0("B", 1:21),
                                     paste0("C", 20:1), paste0("D", 1:19),
                                     paste0("E", 17:1), paste0("F", 1:18)))
vis_data$spot_index <- factor(vis_data$spot_index)   # make discrete for color scheme

# Plot
p <- ggplot(vis_data) +
  # horizontal line for each parking event
  geom_segment(aes(x = assumedTimeIn, xend = assumedTimeOut,
                   y = spotID, yend = spotID,
                   color = spot_index),
               linewidth = 2, lineend = "round") +
  # dot at the time-in
  geom_point(aes(x = assumedTimeIn, y = spotID, color = spot_index),
             size = 3, shape = 15) +
  facet_wrap(~ parkingRow, nrow = 1, scales = "free") +
  # color scale: discrete
  scale_color_viridis_d(option = "viridis", name = "Parking Order") +
  # x-axis limits and formatting: 09:00 to 16:00
  scale_x_datetime(
    limits = c(as.POSIXct(paste(p_base_date, "05:00")), 
               as.POSIXct(paste(p_base_date, "12:00"))),
    date_labels = "%H:%M",
    date_breaks = "1 hour",
    expand = c(0,0)
  ) +
  labs(
    x = "Time (09:00 - 16:00)",
    y = "Parking Spot (spotID)",
    title = "Parking events by spot: Sand Beach Lower, Tues Aug 26 2025"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

print(p)

# let's take it to the next level: let's do geom col, height of 1, some hidden heights of 0
# with differentiated shading for the different types of spot and 
# color for number of cars parked over the day, with indicative text also
spot_count_vis_data <- copy(in_clean)
spot_count_vis_data <- spot_count_vis_data[, .(countCarsParked = uniqueN(vehicleID) - ("-" %chin% unique(vehicleID))),
                        .(lot, spotID, spotType)]
spot_count_vis_data[is.na(spotType), spotType := "standard"]

# buffer spots: create buffer spots because I want things to line up
# in an easy way that's closest to the reality of the lot
buffer_spots_id <- c(paste0("A", 16:21), paste0("C", 0), paste0("D", 20:21), 
                     paste0("E", -3:0), paste0("F", 19:21))
buffer_spots_dt <- data.table(lot = rep("sbl", length(buffer_spots_id)),
                              spotID = buffer_spots_id,
                              spotType = rep("visbuffer", length(buffer_spots_id)),
                              countCarsParked = rep(0, length(buffer_spots_id)),
                              weight = rep(0, length(buffer_spots_id)))
spot_count_vis_data <- rbind(spot_count_vis_data[, weight := 1], buffer_spots_dt)

# pull out spot id details so we can facet
spot_count_vis_data[, parkingRow := substr(spotID, 1, 1)]
spot_count_vis_data[, parkingOrder := gsub("^.", "", spotID)]

# Ensure spotID is treated as categorical
spot_count_vis_data$spotID <- factor(spot_count_vis_data$spotID, 
                          levels = c(paste0("A", 21:1), paste0("B", 1:21),
                                     paste0("C", 20:0), paste0("D", 1:21),
                                     paste0("E", 17:-3), paste0("F", 1:21)))
spot_count_vis_data[, spotType := as.factor(spotType)]
spot_count_vis_data[, countCarsParked := as.factor(countCarsParked)]
# vis_data$countCarsParked <- factor(vis_data$countCarsParked)   # make discrete for color scheme

## just showing the total number of vehicles to park there throughout the day
# Plot
p_counts <- ggplot(spot_count_vis_data[lot == "sbl"], 
                   aes(x = weight, y = spotID, fill = countCarsParked, pattern = spotType)) +
  # col for each parking spot
  geom_col_pattern(pattern_colour  = 'white',
                   pattern_size = 0.6) +
  # color scale: discrete
  scale_fill_manual(name = "Count Cars Parked", 
                    values = c("0" = "lightgray",
                               "1" = "#440154",
                               "2" = "#46327e", 
                               "3" = "#365c8d", 
                               "4" = "#277f8e", 
                               "5" = "#1fa187", 
                               "6" = "#4ac16d", 
                               "7" = "#a0da39", 
                               "8" = "#fde725")) + 
  scale_pattern_manual(
    name = "Spot Type",
    values = c(
      "standard" = "none",
      "visbuffer" = "none",
      "ADA" = "stripe",
      "ADA Loading" = "stripe",
      "Authorized Vehicle" = "crosshatch",
      "bus area" = "crosshatch",
      "Not Real Spot" = "circle"
    )
  ) +
  #geom_col() +
  facet_wrap(~ parkingRow, nrow = 1, scales = "free") +
  labs(
    y = "Parking Spot (spotID)",
    title = "Parking events by spot: Sand Beach Lower, Tues Aug 26 2025"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

print(p_counts)


# ADDITIONAL VISUAL OF USE
duration_data <- copy(in_clean)

# hold onto flag if the vehicle was still there when we left

# If any assumedTimeOut is NA and you want to show until end-of-day window:
# i had to hack the time zone, I couldn't figure out the time zone conversion from UTC to EDT properly
duration_data[, hour_parked := factor(ifelse(is.na(assumedTimeIn), "Present BOD", hour(assumedTimeIn)), levels = c("Present BOD", 9:15))]
duration_data[is.na(assumedTimeIn), assumedTimeIn := as.POSIXct(paste(p_base_date, "05:00"))] 
# let's also bin the time by the actual hour the car parked
duration_data[, hour_departed := factor(ifelse(is.na(assumedTimeOut), "Present EOD", hour(assumedTimeOut)), levels = c(9:15, "Present EOD"))]
duration_data[is.na(assumedTimeOut), assumedTimeOut := as.POSIXct(paste(p_base_date, "12:00"))]

duration_data[, parking_duration := as.numeric(difftime(assumedTimeOut, assumedTimeIn, units='mins'))]
duration_data[, entry_time_binned := ifelse(spot_index == 1, "start of day", 
                                            ifelse(as.numeric(difftime(as.POSIXct(paste(p_base_date, "07:00")), assumedTimeIn)) > 0 , "before 11 am",
                                                   ifelse(as.numeric(difftime(as.POSIXct(paste(p_base_date, "09:00")), assumedTimeIn)) > 0, "before 1 pm",
                                                           ifelse(as.numeric(difftime(as.POSIXct(paste(p_base_date, "11:00")), assumedTimeIn)) > 0, "before 3 pm", "after 3 pm"))))]
duration_data[, entry_time_binned := factor(entry_time_binned, levels = c("start of day", "before 11 am", "before 1 pm", "before 3 pm", "after 3 pm"))]

# plot with bins and general times
ggplot(duration_data[lot =="sbl" & vehicleID != "-" & spotType %in% c(NA, "Not Real Spot", "ADA")]) +
  geom_histogram(aes(x = parking_duration, fill = entry_time_binned), binwidth = 15, col = "white", linewidth = 1.5) +
  theme_classic() +
  labs(
    x = "Parking Duration (Minutes)",
    y = "Count Vehicles",
    title = "Parking Duration by Time of Day Parked: Sand Beach Lower, Tues Aug 26 2025",
    subtitle = "Including all vehicles parked in a Standard, ADA, or conveniently made up parking spot."
  ) +
  # color scale: discrete
  scale_fill_viridis_d(option = "viridis", name = "Time of Day Initially Parked") +
  scale_x_continuous(n.breaks = 15)
# plot with more specific hour bins, and a bin specifically for those vehicles still parked end of day
ggplot(duration_data[lot =="sbl" & vehicleID != "-" & spotType %in% c(NA, "Not Real Spot", "ADA")]) +
  geom_histogram(aes(x = parking_duration, fill = hour_parked), binwidth = 15, col = "white", linewidth = 1.5) +
  theme_classic() +
  labs(
    x = "Parking Duration (Minutes)",
    y = "Count Vehicles",
    title = "Parking Duration by Time of Day Parked: Sand Beach Lower, Tues Aug 26 2025",
    subtitle = "Including all vehicles parked in a Standard, ADA, or conveniently made up parking spot."
  ) +
  # color scale: discrete
  scale_fill_viridis_d(option = "viridis", name = "Time of Day Initially Parked") +
  scale_x_continuous(n.breaks = 15)

#ok let's make a table
duration_data[, count_spots := uniqueN(spotID), .(lot, spotType)]
table_stay <- duration_data[vehicleID != "-", .(avg = mean(parking_duration), median = median(parking_duration), count_cars = .N), .(lot, spotType, count_spots, hour_parked)]

# stay leave matrix data
stay_leave_matrix <- duration_data[vehicleID != "-" & spotType %in% c(NA, "Not Real Spot", "ADA"), .(count_cars = .N), .(lot, hour_parked, hour_departed)]

# ggplot matrix heatmap
ggplot(stay_leave_matrix[lot == "sbl"], aes(hour_parked, hour_departed)) +
  geom_tile(aes(fill = count_cars)) +
  geom_text(aes(label = round(count_cars, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  labs(
    x = "Hour Parked",
    y = "Hour Departed",
    title = "Count of Vehicles Parked and Departed By Hour Combination: Sand Beach Lower, Tues Aug 26 2025",
    subtitle = "Including all vehicles parked in a Standard, ADA, or conveniently made up parking spot."
  )
