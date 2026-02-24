# FINAL ACADIA LOT VISUALS

# acadia visuals from parking aggregates
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(lubridate)
library(viridis)
library(scales)       # for nice time labels on x-axis (optional)

p_dir_base <- "/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/"

in_acad_table <- as.data.table(read_xlsx(paste0(p_dir_base, "parking_lots_hourly_aggregated.xlsx"), sheet = "standard, not edge case"))

# column formatting
in_acad_table[, hour_parked := factor(hour_parked, levels = c("all", 9:15), ordered = T) ]
in_acad_table[, lot := factor(lot, levels = c("el lake", "el road", "jphnorth", "jphsouth", 
                                              "sbl", "sbu"), ordered = T) ]
in_acad_table[, attraction := ifelse(grepl("jph", lot), "Jordan Pond House", 
                                     ifelse(grepl("sb", lot), "Sand Beach", "Eagle Lake"))]
in_acad_table[lot == "el lake", lot_name_full := "Eagle Lake Boat Launch"]
in_acad_table[lot == "el road", lot_name_full := "Eagle Lake Lot"]
in_acad_table[lot == "jphsouth", lot_name_full := "Jordan Pond South Lot"]
in_acad_table[lot == "jphnorth", lot_name_full := "Jordan Pond North Lot"]
in_acad_table[lot == "sbl", lot_name_full := "Sand Beach Lower Lot"]
in_acad_table[lot == "sbu", lot_name_full := "Sand Beach Upper Lot"]

# median stay length for these different lots, labelled with the sample size and split by the attractions
ggplot(in_acad_table[hour_parked != "all"], aes(x = hour_parked, y = median_stay_minutes, fill = lot)) +
  geom_col(width = 0.8, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count_vehicles_arrival_and_exit_witnessed), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.6) +
  theme_bw() +
  facet_wrap(~attraction) +
  ggtitle("Median Stay by Hour Parked and Lot", subtitle = "Labelled with the number of cars parked during that hour who left the lot before 4 pm.")

# count of vehicles entering the lot by hour
ggplot(in_acad_table[hour_parked != "all" & attraction != "Eagle Lake"], aes(x = hour_parked, y = vehicles_entered_lot, fill = lot)) +
  geom_col(width = 0.8, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(100*vehicles_exit_lot/vehicles_entered_lot), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.6) +
  theme_bw() +
  facet_wrap(~attraction) +
  ggtitle("Count Vehicles Entering the Lot by Hour and Lot", subtitle = "Labelled with the ratio of cars exitting the lot during that hour.")

# long dataset
in_acad_table[, combined_cars_parking := sum(as.numeric(count_vehicles_arrival_and_exit_witnessed), as.numeric(count_vehicles_present_at_eod), as.numeric(count_vehicles_ADA_parked), as.numeric(count_vehicles_illegally_parked), na.rm = T), .(lot, hour_parked)]
in_acad_table[, public_capacity := standard_capacity + ADA_capacity]

# for the purposes of the visual, create an assumed value for counts of vehicles entering in the 2 pm window
in_acad_table[, interpolated_vehicles_entered := ifelse(hour_parked > 12, mean(vehicles_entered_lot, na.rm = T), NA), lot]
in_acad_table[hour_parked == 14 & is.na(vehicles_entered_lot), vehicles_entered_lot := ceiling(interpolated_vehicles_entered)]
in_acad_table[, interpolated_vehicles_entered := NULL]

# also, for the purposes of the visual, if the overall entered metric is less than the parked metric, clear it out. just about every car parked
# then and this is reflection of slight data collection error / misalignment in data collection approach
in_acad_table[hour_parked != "all" & combined_cars_parking > vehicles_entered_lot, vehicles_entered_lot := NA]

# make lot names into a factor to force plot order
plot_lot_order <- c("Sand Beach Upper Lot", "Jordan Pond South Lot", "Eagle Lake Boat Launch",           
                    "Sand Beach Lower Lot", "Jordan Pond North Lot", "Eagle Lake Lot")
totalcap_in_order <- c("22", "71", "8", "101", "185", "28")
avg_stays_in_order <- c("80", "107", "104", "83", "120", "101")
in_acad_table[, lot_name_full := factor(lot_name_full, 
                                        levels = plot_lot_order,
                                        labels = paste0(plot_lot_order, "\nTotal Capacity: ", totalcap_in_order, " Spots\nAverage Vehicle Stay: ", avg_stays_in_order, " minutes"))]

# calculate percent parked
pct_parked_tbl <- in_acad_table[hour_parked != "all" &! is.na(vehicles_entered_lot) &! is.na( combined_cars_parking), 
                                .(entered_v = sum(vehicles_entered_lot), parked_v = sum(combined_cars_parking)) , lot_name_full]
pct_parked_tbl[, pct_parked := round(100*parked_v/entered_v)]


# reshape
in_acad_table[, vehicles_beyond_capacity := vehicles_entered_lot - combined_cars_parking]
combined_metrics_plot_data <- melt.data.table(in_acad_table, id.vars = c("lot_name_full", "standard_capacity", "public_capacity", "hour_parked", "median_stay_minutes"),
                                              measure.vars = c("vehicles_beyond_capacity", "combined_cars_parking"), variable.name = "metric_type", value.name = "count_vehicles")

# rename the metric type values so it comes up nice in the legend
combined_metrics_plot_data[, metric_type := ifelse(metric_type == "vehicles_beyond_capacity", "Did Not Find Parking", "Successfully Parked")]

# # make one-row-per-lot with its capacity for horizontal line limits
# cap_labels <- unique(combined_metrics_plot_data[, .(lot_name_full, public_capacity)])

# next steps to clean this: move legend to bottom, relabel axes, label vehicles successfully parked with dark
# text within the light block. If not too busy, label horizontal line with n = capacity

integer_breaks <- function(n = 5, ...) {
  breaker <- scales::pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}


# MORE metrics with acadia parking data

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

plot_dt_long <- melt.data.table(plot_dt, id.vars = c("lot_name_full", "slot_start", "total_spots"), 
                                measure.vars = c("occupied_spots", "current_cars_in"), variable.name = "Vehicle Group", value.name = "cnt_vehicles" )
plot_dt_long[, `Vehicle Group` := ifelse(`Vehicle Group` == "occupied_spots", "Vehicles Parked", "Vehicles In Lot")]

# make sure consistent labels for iteration
combined_metrics_plot_data[lot_name_full == "Eagle Lake Boat Launch\nTotal Capacity: 8 Spots\nAverage Vehicle Stay: 104 minutes", clear_lot_label := "Eagle Lake, Lakeside"]
combined_metrics_plot_data[lot_name_full == "Eagle Lake Lot\nTotal Capacity: 28 Spots\nAverage Vehicle Stay: 101 minutes", clear_lot_label := "Eagle Lake, Roadside"]
combined_metrics_plot_data[lot_name_full == "Jordan Pond North Lot\nTotal Capacity: 185 Spots\nAverage Vehicle Stay: 120 minutes", clear_lot_label := "Jordan Pond House, North"]
combined_metrics_plot_data[lot_name_full == "Jordan Pond South Lot\nTotal Capacity: 71 Spots\nAverage Vehicle Stay: 107 minutes", clear_lot_label := "Jordan Pond House, South"]
combined_metrics_plot_data[lot_name_full == "Sand Beach Lower Lot\nTotal Capacity: 101 Spots\nAverage Vehicle Stay: 83 minutes", clear_lot_label := "Sand Beach, Lower Lot"]
combined_metrics_plot_data[lot_name_full == "Sand Beach Upper Lot\nTotal Capacity: 22 Spots\nAverage Vehicle Stay: 80 minutes", clear_lot_label := "Sand Beach, Upper Lot"]

# separate plot for each lot
for(individ_lot in as.character(plot_dt_long[, unique(lot_name_full)])) {
  print(individ_lot)
  
  # left plot
  single_lot_veh_parked <- combined_metrics_plot_data[clear_lot_label == individ_lot & hour_parked != "all"]
  p_single_lot_hourly <- ggplot(single_lot_veh_parked, 
                                aes(x = hour_parked, group = metric_type, y = count_vehicles)) +
    geom_col(aes(fill = metric_type), width = 0.85, 
             position = position_dodge(width = 0.85)) +
    # value labels just below the top of each column (inside bar)
    geom_text(aes(label = count_vehicles),
              position = position_dodge(width = 0.85),
              vjust = 1.1,                # >1 nudges text slightly downward from the top
              size = 3.2,                   # adjust size as needed
              fontface = "bold",
              color = "white") +          # change to "black" if bars are light
    theme_minimal() +
    theme(legend.position = "top", legend.direction = "horizontal", 
          text = element_text(size = 12), legend.title = element_blank()) +
    labs(x = "Hour", y = "Count Vehicles") +
    scale_y_continuous(breaks = integer_breaks())+ #limits = c(0, NA)
    scale_fill_manual(values = c("#0052ba", "#5e8fff")) +
    coord_cartesian(clip = "off")
  
  # right plot
  single_lot_veh_parked <- plot_dt_long[`Vehicle Group` == "Vehicles Parked" & as.character(lot_name_full) == individ_lot]
  p_lot_fill_individ <- ggplot(single_lot_veh_parked, aes(x = slot_start, group = `Vehicle Group`, color = `Vehicle Group`)) +
    geom_hline(aes(linetype = "Lot Capacity", yintercept = total_spots), 
               size = 0.7, linetype = "dotted") + 
    annotate("text", fontface = "italic", size = 3.5, 
             as.POSIXct(ifelse(grepl("Eagle Lake", individ_lot), "1970-01-01 11:15:00", "1970-01-01 10:00:00"), tz = "UTC"), 
             unique(ifelse(individ_lot == "Jordan Pond House, North", single_lot_veh_parked$total_spots + 6, single_lot_veh_parked$total_spots)), label = "Lot Capacity", 
             vjust = 1.3) +
    geom_line(aes(y = cnt_vehicles), linewidth = 1) +
    geom_point(aes(y = cnt_vehicles), size = 1.2) +
    scale_linetype_manual(values = 3) + 
    scale_y_continuous(breaks = integer_breaks())+ #limits = c(0, NA)
    scale_x_datetime(
      breaks = seq(as.POSIXct("1970-01-01 09:00:00", tz = "UTC"),
                   as.POSIXct("1970-01-01 16:00:00", tz = "UTC"),
                   by = "1 hour"),
      labels = function(x) as.integer(format(x, "%H")),   # prints 9, 10, 11, 12, 13, ...
      expand = expansion(add = c(0, 0))
    ) +
    labs(
      x = "Hour",
      y = "Count Vehicles Parked"
    ) +
    scale_color_manual(values = c("#5e8fff"), name = "Vehicle Position") +
    theme_minimal() +
    theme(legend.position = "top", text = element_text(size = 12), legend.title = element_blank(),
          plot.margin = margin(t = 0.1, r = 1, b = 0.1, l = 0.1, unit = "cm")) +
    coord_cartesian(clip = "off")
  
  #p <- cowplot::plot_grid(p_single_lot_hourly, p_lot_fill_individ, labels = NULL)
  out_plots <- p_single_lot_hourly + p_lot_fill_individ
  ggsave(filename = paste0(individ_lot, "_patchwork_plots.png"), 
         plot = out_plots, 
         path = paste0("/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/visuals/"),
         width = 7, height = 4)
  #cowplot::save_plot(filename = paste0("/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/visuals/", individ_lot, "_both_plots.png"), p, ncol = 2)
  
}
