# acadia visuals from parking aggregates

library(data.table)
library(ggplot2)
library(dplyr)
library(readxl)

p_dir_base <- "/Users/Nineveh.OConnell/DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/5- Processed Data Outputs/"

in_acad_table <- as.data.table(read_xlsx(paste0(p_dir_base, "parking_lots_hourly_aggregated.xlsx"), sheet = "standard, not edge case"))

# column formatting
in_acad_table[, hour_parked := factor(hour_parked, levels = c("all", 9:15), ordered = T) ]
in_acad_table[, lot := factor(lot, levels = c("el lake", "el road", "jphnorth", "jphsouth", 
                                              "sbl", "sbu"), ordered = T) ]
in_acad_table[, attraction := ifelse(grepl("jph", lot), "Jordan Pond House", 
                                     ifelse(grepl("sb", lot), "Sand Beach", "Eagle Lake"))]
in_acad_table[lot == "el lake", lot_name_full := "Eagle Lake, Lakeside"]
in_acad_table[lot == "el road", lot_name_full := "Eagle Lake, Roadside"]
in_acad_table[lot == "jphsouth", lot_name_full := "Jordan Pond House, South"]
in_acad_table[lot == "jphnorth", lot_name_full := "Jordan Pond House, North"]
in_acad_table[lot == "sbl", lot_name_full := "Sand Beach, Lower Lot"]
in_acad_table[lot == "sbu", lot_name_full := "Sand Beach, Upper Lot"]

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
plot_lot_order <- c("Sand Beach, Upper Lot", "Jordan Pond House, South", "Eagle Lake, Lakeside",           
                    "Sand Beach, Lower Lot", "Jordan Pond House, North", "Eagle Lake, Roadside")
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
combined_metrics_plot_data <- melt.data.table(in_acad_table, id.vars = c("lot_name_full", "standard_capacity", "public_capacity", "hour_parked", "median_stay_minutes"),
                                              measure.vars = c("vehicles_entered_lot", "combined_cars_parking"), variable.name = "metric_type", value.name = "count_vehicles")

# rename the metric type values so it comes up nice in the legend
combined_metrics_plot_data[, metric_type := ifelse(metric_type == "vehicles_entered_lot", "Did Not Find Parking", "Successfully Parked")]


# # make one-row-per-lot with its capacity for horizontal line limits
# cap_labels <- unique(combined_metrics_plot_data[, .(lot_name_full, public_capacity)])

# next steps to clean this: move legend to bottom, relabel axes, label vehicles successfully parked with dark
# text within the light block. If not too busy, label horizontal line with n = capacity
# Include text under each lot name with computed average stay length for vehicles
# for which we saw the arrival and the departure.
# THEN WRITE UP METHOD, INCLUDE IN SLIDE FOR FEEDBACK
acad_availability_plot <- ggplot(combined_metrics_plot_data[hour_parked != "all"], aes(x = hour_parked)) +
  # geom_hline(aes(yintercept = public_capacity), size = 0.9) +
  # geom_hline(aes(yintercept = standard_capacity), linetype='dotted', size = 0.7) + 
  geom_col(aes(y = count_vehicles, fill = metric_type), width = 1.6, position = position_dodge(width = 0)) +
  # value labels just below the top of each column (inside bar)
  geom_text(aes(y = count_vehicles, label = count_vehicles),
            position = position_dodge(width = 1.6),
            vjust = 1.1,                # >1 nudges text slightly downward from the top
            size = 3,                   # adjust size as needed
            color = "white") +          # change to "black" if bars are light
  #add one label per facet; inherit.aes = FALSE so we control mappings here
  geom_label(
    data = pct_parked_tbl,
    aes(x = Inf, y = Inf, label = paste0(pct_parked, "% Parked")),
    inherit.aes = FALSE,
    hjust = 1.05,            # nudge slightly inside from right edge
    vjust = 0.7,            # nudge above the line a bit
    size = 3
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal", text = element_text(size = 12)) +
  labs(x = "Hour", y = "Count Vehicles") +
  facet_wrap(~lot_name_full, scales = "free_y") +
  scale_fill_manual(values = c("#0052ba", "#5e8fff"), name = "Vehicle Action in the Given Hour") +
  ggtitle("Hourly Vehicle Activity, By Hour and Lot", 
          subtitle = "Vehicle activity in each lot by hour from 9 am to 4 pm on the day of observation.\nLabelled with the number of vehicles that parked and the total number of vehicles that drove into the lot each hour.") +
  coord_cartesian(clip = "off")

# average stay length for these different lots, labelled with the sample size and split by the attractions
# SHOULD THIS BE AVERAGE OR MEDIAN? WHAT'S MORE USEFUL TO KNOW?
acad_stay_length_plot <- ggplot(in_acad_table[hour_parked != "all"], aes(x = hour_parked)) +
  geom_hline(aes(yintercept = avg_stay_minutes), in_acad_table[hour_parked == "all"], size = 0.9) +
  geom_col(aes(y = avg_stay_minutes, fill = "#5e8fff")) +
  # value labels just below the top of each column (inside bar)
  geom_label(aes(y = 0, label = count_vehicles_arrival_and_exit_witnessed),
            vjust = -0.3,                # >1 nudges text slightly downward from the top
            size = 4,                   # adjust size as needed
            color = "white",
            fill = "#5e8fff") +          # change to "black" if bars are light
  # add one label per facet; inherit.aes = FALSE so we control mappings here
  geom_label(
    data = in_acad_table[hour_parked == "all"],
    aes(x = Inf, y = avg_stay_minutes, label = paste0(round(avg_stay_minutes), " minutes")),
    inherit.aes = FALSE,
    hjust = 1.05,            # nudge slightly inside from right edge
    vjust = -0.3,            # nudge above the line a bit
    size = 4
  ) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 12)) +
  labs(x = "Hour First Parked", y = "Minutes In Parking Spot") +
  facet_wrap(~lot_name_full, scales = "free_y") +
  scale_fill_manual(values = c("#5e8fff")) +
  ggtitle("Parking Duration, By Hour and Lot", 
          subtitle = "Length of stay in each lot by hour first park.\nLabelled with the number of vehicles that parked during that hour and departed before data collection ended.\nThe horizontal line indicates the average parking duration throughout the day.") +
  coord_cartesian(clip = "off")


# save those plots
ggsave(filename = "acad_stay_length_plot.png", 
       plot = acad_stay_length_plot, 
       path = paste0(p_dir_base, "visuals/"),
       width = 10, height = 7)
ggsave(filename = "acad_availability_plot.png", 
       plot = acad_availability_plot, 
       path = paste0(p_dir_base, "visuals/"),
       width = 10, height = 7)
# 
# ggplot(in_acad_table[hour_parked != "all"], aes(x = hour_parked, y = median_stay_minutes, fill = lot)) +
#   geom_col(width = 0.8, position = position_dodge(width = 0.9)) +
#   geom_text(aes(label = count_vehicles_arrival_and_exit_witnessed), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.6) +
#   theme_bw() +
#   facet_wrap(~attraction) +
#   ggtitle("Median Stay by Hour Parked and Lot", subtitle = "Labelled with the number of cars parked during that hour who left the lot before 4 pm.")




# i think I should do the same for the count of cars coming and going, don't really need a capacity plot because
# they are always at capacity but could plot it anyway
# if i make assumptions, need to share that -- actually, yes i need to check my assumptions so far!