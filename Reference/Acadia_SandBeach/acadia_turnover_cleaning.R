#### merging nps data for congestion management database

# loading the libraries I often work with
library(readxl)
library(data.table)
library(dplyr)

# establishing my base directory rather than actually changing my directory, I just find this easier
p_file_path <- "~/parkingData_from_app.xlsx"

####################
### SAND BEACH LOWER

# read in the sand beach lower sheet, make it a data table
in_sbl <- data.table(read_xlsx(p_file_path, sheet = "SBL"))
in_sbl[, cleanerTime := format(in_sbl$cleanerTime, format = "%H:%M:%S")]

# some times still coming through as decimals, let's manually fix that
in_sbl[!grepl(":", cleanerTime), tmp_hour := 24*as.numeric(cleanerTime)]
in_sbl[!grepl(":", cleanerTime), tmp_minute := floor(60*(tmp_hour - floor(tmp_hour)))]
in_sbl[!grepl(":", cleanerTime), cleanerTime := paste0(as.character(floor(tmp_hour)), ":", as.character(tmp_minute)) ]
in_sbl[, c("tmp_hour", "tmp_minute") := NULL ]

# drop case sensitivity from vehicle ids
in_sbl[, vehicleID := tolower(vehicleID)]

# sand beach lower data cleaning -- will make edits here rather than in the actual dataset

cleaned_sbl_base <- copy(in_sbl)
# accidentally assigned y85 to e15 instead of e16
cleaned_sbl_base[timestamp == "2025-08-26T09:36:00.0000000" & spotID == "E15", `:=` (spotID = "E16", spot = 16)]
# accidentally assigned ndl to e16 instead of e17
cleaned_sbl_base[vehicleID == "ndl" & spotID == "E16", `:=` (spotID = "E17", spot = 17)]
# accidentally assigned 539 to c10 instead of c9
cleaned_sbl_base[spotID == "C10" & vehicleID == "539", `:=` (spotID = "C9", spot = 9)]
# accidentally assigned 799 to D17 instead of D16
cleaned_sbl_base[spotID == "D17" & vehicleID == "799", `:=` (spotID = "D16", spot = 16)]
# accidentally assigned k31 to e12 instead of e13
cleaned_sbl_base[spotID == "E12" & vehicleID == "k31", `:=` (spotID = "E13", spot = 16)]
# accidentally assigned 443 to e17 instead of e14
cleaned_sbl_base[spotID == "E17" & vehicleID == "443", `:=` (spotID = "E14", spot = 16)]
# accidentally added too many digits to some, or other typos
cleaned_sbl_base[spotID == "E4" & vehicleID == "4230", vehicleID := '230']
cleaned_sbl_base[spotID == "E5" & vehicleID == "6542", vehicleID := '542']
cleaned_sbl_base[spotID == "F1" & vehicleID %in% c("9", "4009"), vehicleID := '009']
cleaned_sbl_base[spotID == "F3" & vehicleID == "23", vehicleID := 'r23']
cleaned_sbl_base[spotID == "D19" & vehicleID == "63", vehicleID := '063']
cleaned_sbl_base[spotID == "F10" & vehicleID == "mtv", vehicleID := 'mtc']
cleaned_sbl_base[spotID == "F7" & vehicleID == "746", vehicleID := '464']
# we were always the authorized vehicle
cleaned_sbl_base[spotID == "A6", vehicleID := "742"]
# fixing checks that appear vehicle was recorded one spot off
cleaned_sbl_base[spotID == "A9" & vehicleID == "157", `:=` (spotID = "A8", spot = 8)]
cleaned_sbl_base <- cleaned_sbl_base[!(spotID == "F10" & vehicleID == "0a1")]
# lets title this one more consistently
cleaned_sbl_base[spotID == "F17" & grepl("moto", vehicleID), vehicleID := "two motorcycles"]
cleaned_sbl_base <- cleaned_sbl_base[!(spotID == "F18" & grepl("moto", vehicleID))]
cleaned_sbl_base[spotID == "E2" & grepl("been here", vehicleID), vehicleID := "817"]
# context makes it very unclear what this refers to, have to drop the datapoint
cleaned_sbl_base <- cleaned_sbl_base[!(spotID == "F16" & vehicleID == "car left")]


# re-establish first and last time a given vehicle was in a spot
cleaned_sbl_base[, c("time_in", "time_out") := NULL]
# establish first and last time a given vehicle was in a spot
cleaned_sbl_base[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(spotID, vehicleID)]
# keep only one record per
sbl_deduped <- unique(cleaned_sbl_base[, .(parkingLot = "Sand Beach Lower", count_records = .N), .(spotID, vehicleID, time_in, time_out)])

#quick analysis of SBL: 
# before data cleaning, we were looking at only 484 unique cars in the lot over the course of the day
# while the lot contains about 110 parking spots (include non-legal locations cars parked). That's about 4.5 cars per spot from 9 to 4 pm
# now, after data cleaning, 463 unique cars 

################################################
#### sand beach upper data cleaning
in_sbu <- data.table(read_xlsx(p_file_path, sheet = "SBU"))
in_sbu[, vehicleID := tolower(vehicleID)]
in_sbu[, CleanerTime := format(in_sbu$CleanerTime, format = "%H:%M:%S")]

# establish first and last time a given vehicle was in a spot
in_sbu[, `:=` (time_in = min(CleanerTime), time_out = max(CleanerTime)), .(section, spot, vehicleID)]

# keep only one record per
sbu_deduped <- unique(in_sbu[, .(parkingLot = "Sand Beach Lower", count_records = .N), .(spotID = `Section + spot`, vehicleID, time_in, time_out)])

# manual corrections
cleaned_sbu <- copy(in_sbu)
setnames(cleaned_sbu, "Section + spot", "spotID")
# c33 accidentally assigend to a2 instead of b2
cleaned_sbu[spotID == "A2" & vehicleID == "c33", `:=` (spotID = "B2", section = "B")]
#d17 accidentally assigned to a5 instead of a4
cleaned_sbu[spotID == "A5" & vehicleID == "d17", `:=` (spotID = "A4", spot = 4)]
#k81 accidentally assigned to a11 instead of a12
cleaned_sbu[spotID == "A11" & vehicleID == "k81", `:=` (spotID = "A12", spot = 12)]
#113 accidentally assigned to a14 instead of a15
cleaned_sbu[spotID == "A14" & vehicleID == "113", `:=` (spotID = "A15", spot = 15)]
#985 accidentally assigned to a15 instead of a16
cleaned_sbu[spotID == "A15" & vehicleID == "985", `:=` (spotID = "A16", spot = 16)]
#s44 accidentally assigned to a16 instead of a17
cleaned_sbu[spotID == "A16" & vehicleID == "s44", `:=` (spotID = "A17", spot = 17)]
# i think this additional assignment of vehicle 537 was an accident
cleaned_sbu[spotID == "A3" & vehicleID == "537", `:=` (spotID = "B2", spot = 2)]

# re-establish first and last time a given vehicle was in a spot
cleaned_sbu[, c("time_in", "time_out") := NULL]
# establish first and last time a given vehicle was in a spot
cleaned_sbu[, `:=` (time_in = min(CleanerTime), time_out = max(CleanerTime)), .(spotID, vehicleID)]
# keep only one record per
sbu_deduped <- unique(cleaned_sbu[, .(parkingLot = "Sand Beach Lower", count_records = .N), .(spotID, vehicleID, time_in, time_out)])

################################################
#### jordan pond south data cleaning
in_jphs <- data.table(read_xlsx(p_file_path, sheet = "JPHS"))
in_jphs[, vehicleID := tolower(vehicleID)]
in_jphs[, cleanerTime := format(in_jphs$cleanerTime, format = "%H:%M:%S")]

# establish first and last time a given vehicle was in a spot
in_jphs[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(section, spot, vehicleID)]

# manual corrections
cleaned_jphs <- copy(in_jphs)
# 140 accidentally assigned to c1 instead of c16
cleaned_jphs[spotID == "C1" & vehicleID == "140", `:=` (spotID = "C16", spot = 16)]
# more consistent labeling
cleaned_jphs[vehicleID == "ir2" & spotID == "D2", vehicleID := "ie2"]
cleaned_jphs[vehicleID == "0bi" & spotID == "D2", vehicleID := "obi"]
cleaned_jphs[vehicleID == "l38" & spotID == "A2", vehicleID := "338"]
# mismarked empty as c, which makes sense it's next to x on keyboard
cleaned_jphs[vehicleID == "c", vehicleID := "x"]
# remove entries as the notes direct me to
cleaned_jphs <- cleaned_jphs[!(spotID == "A6" & timestamp == "2025-08-27T12:39:00.0000000")]
# for this use of the data, drop other notes
cleaned_jphs <- cleaned_jphs[section != "other notes"]

# re-establish first and last time a given vehicle was in a spot
cleaned_jphs[, c("time_in", "time_out") := NULL]
# establish first and last time a given vehicle was in a spot
cleaned_jphs[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(spotID, vehicleID)]
# keep only one record per
jphs_deduped <- unique(cleaned_jphs[, .(parkingLot = "Jordan Pond House South", count_records = .N), .(spotID, vehicleID, time_in, time_out)])

# before data cleaning, looks like 290 unique cars over the day with 76 total spots. Fewer than 4 cars per spot
# after we have 284 unique cars over the day with 75 total spots
nrow(jphs_deduped)
jphs_deduped[, uniqueN(spotID)]

################################################
#### jordan pond north data cleaning
in_jphn <- data.table(read_xlsx(p_file_path, sheet = "JPHN"))
# manually enter manual entry that is missing
in_jphn <- rbind(in_jphn, data.table(spotID = "G17", vehicleID = "v15", cleanerTime = "10:15:00", section = "G", spot = "17" ), fill = T)
in_jphn[, vehicleID := tolower(vehicleID)]
in_jphn[, cleanerTime := format(in_jphn$cleanerTime, format = "%H:%M:%S")]

# establish first and last time a given vehicle was in a spot
in_jphn[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(section, spot, vehicleID)]

# manual corrections
cleaned_jphn <- copy(in_jphn)
# one spot off
cleaned_jphn <- cleaned_jphn[!(spotID == "C24" & vehicleID == "385")]
cleaned_jphn[spotID == "C25", `:=` (spotID = "C24", notes = "")]
cleaned_jphn[spotID == "D22" & vehicleID == "vlr", `:=` (spotID = "D21", notes = "")]
cleaned_jphn[spotID == "D23" & vehicleID == "w95", `:=` (spotID = "D22", notes = "")]
cleaned_jphn[spotID == "D24" & vehicleID == "vrp", `:=` (spotID = "D23", notes = "")]
cleaned_jphn[spotID == "D25" & vehicleID == "vjm", `:=` (spotID = "D24", notes = "")]
cleaned_jphn[spotID == "A21" & cleanerTime == "09:14:00", spotID := "A22"]
cleaned_jphn[spotID == "A20" & cleanerTime == "09:14:00", spotID := "A21"]
cleaned_jphn[spotID == "A19" & cleanerTime == "09:14:00", spotID := "A20"]
cleaned_jphn[spotID == "A18" & cleanerTime == "09:14:00", spotID := "A19"]
cleaned_jphn[spotID == "A17" & cleanerTime == "09:14:00", spotID := "A18"]
cleaned_jphn[spotID == "A16" & cleanerTime == "09:13:00", spotID := "A17"]
cleaned_jphn[spotID == "A15" & cleanerTime == "09:13:00", spotID := "A16"]
cleaned_jphn[spotID == "A14" & cleanerTime == "09:13:00", spotID := "A15"]
cleaned_jphn[spotID == "A13" & cleanerTime == "09:13:00", spotID := "A14"]
cleaned_jphn[spotID == "A12" & cleanerTime == "09:13:00", spotID := "A13"]
cleaned_jphn[spotID == "A11" & cleanerTime == "09:13:00", spotID := "A12"]
cleaned_jphn[spotID == "A10" & cleanerTime == "09:13:00", spotID := "A11"]
cleaned_jphn[spotID == "A9" & cleanerTime == "09:12:00", spotID := "A10"]
cleaned_jphn[spotID == "A8" & cleanerTime == "09:12:00", spotID := "A9"]
cleaned_jphn[spotID == "A7" & cleanerTime == "09:12:00", spotID := "A8"]
cleaned_jphn[spotID == "A6" & vehicleID == "801", `:=` (spotID = "A7", notes = "")]
cleaned_jphn[spotID == "A5" & vehicleID == "9xm", `:=` (spotID = "A6", notes = "")]
# more consistent labeling
cleaned_jphn[spotID == "C16" & vehicleID == "same as before", vehicleID := "27"]
cleaned_jphn[spotID == "A4" & vehicleID == "same as before", vehicleID := "d69"]
cleaned_jphn[spotID == "C10" & vehicleID == "34", vehicleID := "034"]
cleaned_jphn[spotID == "A30" & vehicleID == "552", vehicleID := "55z"]
cleaned_jphn[spotID == "D18" & vehicleID == "593", vehicleID := "s93"]
cleaned_jphn[spotID == "D24" & vehicleID == "sienna", vehicleID := "704"]
cleaned_jphn[spotID == "C20" & vehicleID == "vcf", vehicleID := "ucf"]
cleaned_jphn[spotID == "B8" & vehicleID == "95", vehicleID := "095"]
cleaned_jphn[spotID == "C17" & vehicleID == "lsv", vehicleID := "1sv"]
cleaned_jphn[spotID == "E28" & vehicleID == "oxr", vehicleID := "0xr"]
cleaned_jphn[spotID == "H10" & vehicleID == "mwmx", vehicleID := "wmx"]
# fixing things i misentered from the manual data
cleaned_jphn[spotID == "H16" & cleanerTime == "10:00:00", vehicleID := "713"]
cleaned_jphn[spotID == "H2" & vehicleID == "vdz", vehicleID := "udz"]
cleaned_jphn[spotID == "H4" & vehicleID == "glf", vehicleID := "gcf"]
cleaned_jphn[spotID == "H5" & vehicleID == "vgs", vehicleID := "vgj"]
cleaned_jphn[spotID == "A1" & vehicleID == "rxd", vehicleID := "rzd"]
cleaned_jphn[spotID == "E13" & vehicleID == "6271", vehicleID := "271"]
cleaned_jphn[spotID == "f14", spotID := "F14"]
cleaned_jphn[spotID == "H14" & vehicleID == "876" & grepl("0.625", cleanerTime), cleanerTime := "15:00"]
cleaned_jphn[spotID == "H14" & vehicleID == "876" & cleanerTime == "13:45:00", cleanerTime := "12:45:00"]
cleaned_jphn <- cleaned_jphn[!(vehicleID == "minivan hood up")]
cleaned_jphn <- cleaned_jphn[!(spotID == "B5" & vehicleID == "690")]

# keep only one record per
jphn_deduped <- unique(cleaned_jphn[, .(parkingLot = "Jordan Pond House North", count_records = .N), 
                               .(spotID = spotID, vehicleID, time_in, time_out)])

# before data cleaning, looks like 790 unique cars over the day with 192 total spots. Just over 4 cars per spot
# after have increased to 794 unique cars in 189 spots, from realigning stuff I guess
nrow(jphn_deduped)
jphn_deduped[, uniqueN(spotID)]

#DOUBLE CHECK THIS AND DO LAKE THEN ROLL INTO NEXT STAGE
################################################
#### eagle lake roadside
in_elroad <- data.table(read_xlsx(p_file_path, sheet = "EL Roadside"))
in_elroad[, vehicleID := tolower(vehicleID)]
in_elroad[, cleanerTime := format(in_elroad$cleanerTime, format = "%H:%M:%S")]

# establish first and last time a given vehicle was in a spot
in_elroad[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(section, spot, vehicleID)]

# keep only one record per
elroad_deduped <- unique(in_elroad[, .(parkingLot = "Eagle Lake Roadside", count_records = .N), 
                               .(spotID = spotID, vehicleID, time_in, time_out)])

# manual corrections
cleaned_elroad <- copy(in_elroad)
# one spot off
cleaned_elroad[spotID == "A1" & vehicleID == "same as before", vehicleID := "a92"]
cleaned_elroad[spotID == "A10" & vehicleID %in% c("buick", "same car", "same", "same as before"), vehicleID := "834"]
cleaned_elroad[spotID == "A10" & vehicleID == "blue truck", vehicleID := "9nb"]
cleaned_elroad[spotID == "A10" & vehicleID == "kwl", spotID := "B10"]
cleaned_elroad[spotID == "A12" & vehicleID == "s30", spotID := "A16"]
cleaned_elroad[spotID == "A16" & vehicleID == "s30" & timestamp == "2025-08-28T09:53:00.0000000", cleanerTime := "09:56:00"] # fix this time to not conflict
cleaned_elroad[spotID == "A12" & vehicleID == "big white ruck", `:=` (spotID = "A13", vehicleID = "738")]
cleaned_elroad[spotID == "A13" & grepl("big white t.uck", vehicleID), vehicleID := "738"]
cleaned_elroad[spotID == "A7" & grepl("big white tahoe", vehicleID), vehicleID := "4zm"]
cleaned_elroad[spotID == "A15" & grepl("1wv", vehicleID), vehicleID := "1nv"]
cleaned_elroad[spotID == "A15" & grepl("uwd", vehicleID), vehicleID := "und"]
cleaned_elroad[spotID == "A16" & grepl("04m", vehicleID) & cleanerTime == "15:50:00", cleanerTime := "15:52:00"]
cleaned_elroad[spotID == "A3" & grepl("same", vehicleID), vehicleID := "65c"]
cleaned_elroad[spotID == "A4" & grepl("4230", vehicleID), vehicleID := "230"]
cleaned_elroad[spotID == "A4" & grepl("just parked|car that pulled in", vehicleID), vehicleID := "t20"]
cleaned_elroad[spotID == "A8" & cleanerTime <= "09:59:00", vehicleID := "427"]
cleaned_elroad[spotID == "A8" & grepl("same", vehicleID), vehicleID := "62"]
cleaned_elroad[vehicleID == "t20", spotID := gsub("B", "A", spotID)]
cleaned_elroad[vehicleID == "3mf", spotID := gsub("B", "A", spotID)]
cleaned_elroad[vehicleID == "786", spotID := gsub("B", "A", spotID)]
cleaned_elroad[vehicleID == "4zm", spotID := gsub("B", "A", spotID)]
cleaned_elroad[vehicleID == "478", spotID := gsub("B", "A", spotID)]
cleaned_elroad[vehicleID == "8ek", vehicleID := "98ek"]
cleaned_elroad[vehicleID == "d93", spotID := "B4"]
cleaned_elroad[vehicleID == "same as before" & spotID == "B10", vehicleID := "2ug"]
cleaned_elroad[vehicleID == "oopsie" & spotID == "B7", vehicleID := "364"]
cleaned_elroad[vehicleID == "9nb" & spotID == "B10", spotID := "A10"]
# A5 was a parked over space, need to come clean this one up for a while
cleaned_elroad[spotID == "A5" & grepl("98s", vehicleID), vehicleID := "98n"]

# # before data cleaning, looks like 179 unique cars over the day with 30 total spots.Almost 6 cars per spot
# nrow(elroad_deduped)
# elroad_deduped[, uniqueN(spotID)]
# 
# # data cleaning: let's look at times where the time in for a spot is between the time in and time out of another vehicle in that spot
# spot_vehicle_combos <- paste(elroad_deduped$spotID, elroad_deduped$vehicleID, sep = "_")
# ls_qc <- lapply(spot_vehicle_combos, 
#                 function(x) {
#                   vehicles_in_spot <- elroad_deduped[spotID == gsub("_.*", "", x)]
#                   vehicle <- gsub(".*_", "", x)
#                   curr_time_in <- vehicles_in_spot[vehicleID == vehicle]$time_in
#                   # return the vehicle we're looking at, as well as any it seems to conflict with
#                   check_against <- vehicles_in_spot[(time_in < curr_time_in & time_out > curr_time_in) | (time_in == curr_time_in)]
#                   # store those rows of data
#                   if(nrow(check_against) >1) { return(check_against) } else { return(NULL)}
#                   
#                 })
# names(ls_qc) <- spot_vehicle_combos
# View(rbindlist(ls_qc[lengths(ls_qc) >0]))
# 
# # data cleaning check 2: let's look at times where the vehicle id for a spot is also listed in the spot before or after
# # which is a mistake that may have happened if we were off by one spot
# ls_qc2 <- lapply(unique(elroad_deduped$vehicleID), 
#                  function(x) {
#                    spots_for_vehicle <- elroad_deduped[vehicleID == gsub(".*_", "", x), unique(spotID)]
#                    # store those rows of data
#                    if(length(spots_for_vehicle) >1) { return(spots_for_vehicle) } else { return(NULL)}
#                    
#                  })
# names(ls_qc2) <- unique(elroad_deduped$vehicleID)

################################################
#### eagle lake lake side
in_ellake <- data.table(read_xlsx(p_file_path, sheet = "EL Lakeside"))
in_ellake[, vehicleID := tolower(vehicleID)]
in_ellake[, cleanerTime := format(in_ellake$cleanerTime, format = "%H:%M:%S")]

# establish first and last time a given vehicle was in a spot
in_ellake[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(section, spot, vehicleID)]

# keep only one record per
ellake_deduped <- unique(in_ellake[, .(parkingLot = "Eagle Lake Lakeside", count_records = .N), 
                                   .(spotID = spotID, vehicleID, time_in, time_out)])

# manual corrections
cleaned_ellake <- copy(in_ellake)
# removing some entries that just don't make sense
cleaned_ellake <- cleaned_ellake[!(spotID == "C5" & cleanerTime == "11:58:00")]
cleaned_ellake <- cleaned_ellake[!(spotID == "C5" & cleanerTime == "13:59:00")]
cleaned_ellake[spotID == "D2" & cleanerTime == "13:09:00", spotID := "D3"]
cleaned_ellake[spotID == "D2" & vehicleID == "2226", vehicleID := "226"]
cleaned_ellake[spotID == "C2" & vehicleID == "686", vehicleID := "685"]
cleaned_ellake[spotID == "D3" & vehicleID == "kaq", spotID := "D4"]
# refresh the keep only one record per
cleaned_ellake[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime)), .(section, spot, vehicleID)]
ellake_deduped <- unique(cleaned_ellake[, .(parkingLot = "Eagle Lake Lakeside", count_records = .N), 
                                   .(spotID = spotID, vehicleID, time_in, time_out)])

# # before data cleaning, looks like 53 unique cars over the day with 13 total About 4 cars per spot
# nrow(ellake_deduped)
# ellake_deduped[, uniqueN(spotID)]
# 
# # data cleaning: let's look at times where the time in for a spot is between the time in and time out of another vehicle in that spot
# spot_vehicle_combos <- paste(ellake_deduped$spotID, ellake_deduped$vehicleID, sep = "_")
# ls_qc <- lapply(spot_vehicle_combos, 
#                 function(x) {
#                   vehicles_in_spot <- ellake_deduped[spotID == gsub("_.*", "", x)]
#                   vehicle <- gsub(".*_", "", x)
#                   curr_time_in <- vehicles_in_spot[vehicleID == vehicle]$time_in
#                   # return the vehicle we're looking at, as well as any it seems to conflict with
#                   check_against <- vehicles_in_spot[(time_in < curr_time_in & time_out > curr_time_in) | (time_in == curr_time_in)]
#                   # store those rows of data
#                   if(nrow(check_against) >1) { return(check_against) } else { return(NULL)}
#                   
#                 })
# names(ls_qc) <- spot_vehicle_combos
# View(rbindlist(ls_qc[lengths(ls_qc) >0]))
# 
# # data cleaning check 2: let's look at times where the vehicle id for a spot is also listed in the spot before or after
# # which is a mistake that may have happened if we were off by one spot
# ls_qc2 <- lapply(unique(ellake_deduped$vehicleID), 
#                  function(x) {
#                    spots_for_vehicle <- ellake_deduped[vehicleID == gsub(".*_", "", x), unique(spotID)]
#                    # store those rows of data
#                    if(length(spots_for_vehicle) >1) { return(spots_for_vehicle) } else { return(NULL)}
#                    
#                  })
# names(ls_qc2) <- unique(ellake_deduped$vehicleID)

###########################################
## LET'S PULL EVERYTHING TOGETHER
names(cleaned_sbu) <- c("parkingLot", "section", "positionCode", "spot", "vehicleID",
                        "cleanerTime", "timestamp", "spotID","notes", "time_in", "time_out")

stacked_clean_data <- rbind(cleaned_sbu[, lot := "sbu"], 
                            cleaned_sbl_base[, lot := "sbl"], 
                            cleaned_jphs[, lot := "jphsouth"], 
                            cleaned_jphn[, lot := "jphnorth"], 
                            cleaned_ellake[, lot := "el lake"], 
                            cleaned_elroad[, lot := "el road"], fill=T)

# parse down to columns I'm interested in
stacked_clean_data <- stacked_clean_data[, .(lot, spotID, vehicleID, cleanerTime, notes)]

# consolidate "empty" signifiers to always be dash
stacked_clean_data[vehicleID %in% c("-", "x", "wmpty", "rmpty", "blank", "blocked") | grepl("empty", vehicleID, ignore.case = T), vehicleID := "-"]

# from notes label spots as typical, ada, ada loading, gov, not real spot, bus 
# (could also classify typical with old print about RV, oversize, other types)
#ADA
stacked_clean_data[, tmp_flag := ifelse(grepl("ada|accessible", notes,ignore.case = T), 1, 0 )]
stacked_clean_data[, tmp_flag := max(tmp_flag), .(lot, spotID)]
stacked_clean_data[, spotType := ifelse(tmp_flag == 1, "ADA", NA)]

# ada loading
stacked_clean_data[(lot == "sbl" & spotID %in% c("A11", "A14")) | 
                  (lot == "jphnorth" & spotID == "A23") | 
                  (lot == "jphsouth" & spotID %in% c("A2", "A8")), spotType := "ADA Loading"]
# gov spot
stacked_clean_data[, tmp_flag := ifelse(grepl("gov", notes,ignore.case = T), 1, 0 )]
stacked_clean_data[, tmp_flag := max(tmp_flag), .(lot, spotID)]
stacked_clean_data[, spotType := ifelse(tmp_flag == 1 & spotID != "B6", "Authorized Vehicle", spotType)]

# illegal spot
stacked_clean_data[, tmp_flag := ifelse(grepl("illegal|not a spot|not a real|no parking|dead spot|trail entrance", notes,ignore.case = T), 1, 0 )]
stacked_clean_data[, tmp_flag := max(tmp_flag), .(lot, spotID)]
stacked_clean_data[, spotType := ifelse(tmp_flag == 1 & spotID != "B6" & is.na(spotType), "Not Real Spot", spotType)]

# bus spot
stacked_clean_data[lot == "sbl" & spotID == "A15", spotType := "bus area"]

# make sure any NA spots in JPH south before A10 are marked as ADA
stacked_clean_data[lot == "jphsouth" & is.na(spotType) & grepl("A", spotID) & as.numeric(gsub("A", "", spotID)) < 10, spotType := "ADA"]
# except A1, which is not a real spot
stacked_clean_data[lot == "jphsouth" & spotID == "A1", spotType := "Not Real Spot"]

## can revisit and refine that, but now let's clarify precise times where noted
stacked_clean_data[, inPreciseTime := ifelse(grepl("just parked|just arrived|arriving|pulled in|pulling in|parking now|parked now|just now|just parking", notes, ignore.case = T), 1, 0)]
stacked_clean_data[, outPreciseTime := ifelse(grepl("leaving|just left|pulled out|pulling out|heading out|just departed|about to leave", notes, ignore.case = T), 1, 0)]
## some cases are going to flag oddly, manually overwriting for what I can see
stacked_clean_data[spotID == "D1" & vehicleID == "ebc" & cleanerTime == "12:15", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "D1" & vehicleID == "846" & cleanerTime == "12:14", `:=` (inPreciseTime = 0, outPreciseTime = 1)]
stacked_clean_data[spotID == "E3" & vehicleID == "d70" & cleanerTime == "13:50", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "E4" & vehicleID == "v94" & cleanerTime == "10:24", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "E4" & vehicleID == "529" & cleanerTime == "10:24", `:=` (inPreciseTime = 0, outPreciseTime = 1)]
stacked_clean_data[spotID == "B3" & vehicleID == "345" & cleanerTime == "14:43:00", `:=` (inPreciseTime = 0, outPreciseTime = 0)]
stacked_clean_data[spotID == "E14" & vehicleID == "443" & cleanerTime == "12:32", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "A3" & vehicleID == "297" & cleanerTime == "16:02:00", `:=` (inPreciseTime = 0, outPreciseTime = 0)]
stacked_clean_data[spotID == "A3" & vehicleID == "65c" & cleanerTime == "15:16:00", `:=` (inPreciseTime = 0, outPreciseTime = 0)]
stacked_clean_data[spotID == "E3" & vehicleID == "0a1" & cleanerTime == "12:52", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "B20" & vehicleID == "627" & cleanerTime == "09:50", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "A9" & vehicleID == "478" & cleanerTime == "14:56:00", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "E1" & vehicleID == "upy" & cleanerTime == "10:59", `:=` (inPreciseTime = 1, outPreciseTime = 0)]
stacked_clean_data[spotID == "C13" & vehicleID == "560" & cleanerTime == "12:34:00", `:=` (inPreciseTime = 0, outPreciseTime = 1)]
stacked_clean_data[spotID == "B10" & vehicleID == "2ug" & cleanerTime == "09:24:00", `:=` (inPreciseTime = 0, outPreciseTime = 1)]
stacked_clean_data[spotID == "E2" & vehicleID == "3zp" & cleanerTime == "11:56:00", `:=` (inPreciseTime = 0, outPreciseTime = 1)]
# doesn't make sense where the 413 comes from, just have to drop unfortunately
stacked_clean_data <- stacked_clean_data[!(lot == "el road" & spotID == "A7" & vehicleID == "413")]
# move over for this messy parking
stacked_clean_data[(lot == "el road" & vehicleID == "w82"), spotID := "A7"]
stacked_clean_data[(lot == "el road" & vehicleID == "azm"), spotID := "4zm"]
stacked_clean_data <- stacked_clean_data[!(lot == "el road" & spotID == "A4" & cleanerTime == "14:51:00")]
# in reading comments, finding some rows I wanted to drop
stacked_clean_data <- stacked_clean_data[!(spotID == "F15" & vehicleID == "swu" & lot == "sbl")]

# i want "empty" to stay its own record -- add time on to the id so they stay unique
stacked_clean_data[vehicleID == "-", vehicleID := paste0(vehicleID, cleanerTime)]

# refresh to keep only one record per
stacked_clean_data[, `:=` (time_in = min(cleanerTime), time_out = max(cleanerTime),
                           inPreciseTime = max(inPreciseTime), outPreciseTime = max(outPreciseTime)), .(lot, spotID, vehicleID)]
stacked_clean_data <- unique(stacked_clean_data[, .(count_records = .N), 
                                        .(lot, spotID, spotType, vehicleID, time_in, time_out, inPreciseTime, outPreciseTime)])

# now can re simplify the empties
stacked_clean_data[grepl("-", vehicleID), vehicleID := "-"]

# must separate two different kul vehicles parking in the same spot
kul_entry_combined <- stacked_clean_data[lot == "jphnorth" & spotID == "F2" & vehicleID == "kul"]
stacked_clean_data[lot == "jphnorth" & spotID == "F2" & vehicleID == "kul", time_out := "13:15:00"]
stacked_clean_data <- rbind(stacked_clean_data, kul_entry_combined[, time_in := "15:30:00"])

# quick cleaning double check
# data cleaning: let's look at times where the time in for a spot is between the time in and time out of another vehicle in that spot
spot_vehicle_combos <- paste(stacked_clean_data$lot, stacked_clean_data$spotID, stacked_clean_data$vehicleID, sep = "_")
# ls_qc <- lapply(spot_vehicle_combos,
#                 function(x) {
#                   vehicles_in_spot <- stacked_clean_data[paste(stacked_clean_data$lot, stacked_clean_data$spotID, sep = "_") == sub('_[^_]*$', '', x)]
#                   vehicle <- gsub(".*_", "", x)
#                   curr_time_in <- vehicles_in_spot[vehicleID == vehicle]$time_in
#                   # return the vehicle we're looking at, as well as any it seems to conflict with
#                   check_against <- vehicles_in_spot[(time_in < curr_time_in & time_out > curr_time_in) | (time_in == curr_time_in)]
#                   # store those rows of data
#                   if(nrow(check_against) >1) { return(check_against) } else { return(NULL)}
# 
#                 })
# names(ls_qc) <- spot_vehicle_combos
# View(unique(rbindlist(ls_qc[lengths(ls_qc) >0])))


##################################
# Create initial metrics
final_assumptions_set <- copy(stacked_clean_data)

# order by time out
setorderv(final_assumptions_set, "time_out")

# number order by parking spots
final_assumptions_set[, spot_index := 1:.N, .(lot, spotID)]

#reorder by time in -- if we get different orders, we need to reconcile
setorderv(final_assumptions_set, "time_in")
final_assumptions_set[, spot_index_in := 1:.N, .(lot, spotID)]

# look at cases, make some adjustments
final_assumptions_set[spot_index != spot_index_in]
final_assumptions_set[lot == "jphsouth" & spotID == "A18" & vehicleID == "s49", spot_index := 4]
final_assumptions_set[lot == "jphsouth" & spotID == "A18" & vehicleID == "617", spot_index := 5]
final_assumptions_set <- final_assumptions_set[!(lot == "el road" & spotID == "A4" & time_in == "14:51:00")]
final_assumptions_set[, c("spot_index_in") := NULL]

# buff up times so I can do this posixct conversion
final_assumptions_set[nchar(trimws(time_in)) == 5, time_in := paste0(trimws(time_in), ":00")]
final_assumptions_set[nchar(trimws(time_out)) == 5, time_out := paste0(trimws(time_out), ":00")]

# some straggler times
final_assumptions_set[lot == "sbl" & spotID == "B15" & vehicleID == "xv3", time_out := "14:01:00"]
final_assumptions_set[lot == "sbl" & spotID == "B4" & vehicleID == "jbe", time_out := "15:01:00"]
final_assumptions_set[lot == "sbl" & spotID == "A9" & vehicleID == "vxz", time_out := "15:01:00"]
final_assumptions_set[lot == "sbl" & spotID == "B12" & vehicleID == "1348", time_out := "15:01:00"]
final_assumptions_set[lot == "sbl" & spotID == "A13" & vehicleID == "147", time_out := "15:01:00"]

# assign assumed time in -- leave NA for initial observation unless precise
final_assumptions_set[, assumedTimeIn := as.POSIXct(time_in, format = "%H:%M:%S")]
# assign assumed time out -- will assume leave NA for final observation unless precise, handling that in later loop
final_assumptions_set[, assumedTimeOut := as.POSIXct(time_out, format = "%H:%M:%S")]

# function to find midpoint between two times
min_midpoint <- function(laterTime, earlierTime) {
  
  timeDiff <- difftime(laterTime, earlierTime, units = "min")
  midpoint <- earlierTime + floor(abs(timeDiff)/2)
  
  return(midpoint)
}

final_assumptions_set[, checked := "tbd"]
# for each spot, let's go between entries to infer the time
unique_spots <- paste(final_assumptions_set$lot, final_assumptions_set$spotID, sep = "_")
final_assumptions_set[, tempid := paste(lot, spotID, sep = "_")]
for(singleSpot in unique_spots) {
  # get all instances at that spot
  sub_clean_data <- final_assumptions_set[tempid == singleSpot]
  # now iterate over the vehicles that were there
  for (i in 1:max(sub_clean_data$spot_index)) {
    curr_veh <- sub_clean_data[spot_index == i]
    print(paste(curr_veh$vehicleID, " in ", singleSpot))
    
      # if in time is not precise, infer backward. don't infer backward for initial observation
      if(curr_veh$inPreciseTime == 0 & i >1) {
        # if in time of next is precise, assume 1 minutes before next instance in time. otherwise, assume midpoint 
        prev_veh <- sub_clean_data[spot_index == i - 1]
        final_assumptions_set[tempid == singleSpot & spot_index == i, 
                           assumedTimeIn := ifelse(prev_veh$outPreciseTime == 1, prev_veh$assumedTimeOut + as.difftime(1, units = "mins"),
                                           min_midpoint(curr_veh$assumedTimeIn, prev_veh$assumedTimeOut) + as.difftime(1, units = "mins"))]
      }
      
      # if initial observation, leave in time NA unless precise
      if(i == 1 & curr_veh$inPreciseTime == 0) {
        final_assumptions_set[tempid == singleSpot & spot_index == i, 
                              assumedTimeIn := NA]
      }
      
      # if out time is not precise, infer forward. don't infer forward for final observation
      if(curr_veh$outPreciseTime == 0 & i != max(sub_clean_data$spot_index)) {
        # if in time of next is precise, assume 1 minutes before next instance in time. otherwise, assume midpoint 
        next_veh <- sub_clean_data[spot_index == i + 1]
        final_assumptions_set[tempid == singleSpot & spot_index == i, 
                           assumedTimeOut := ifelse(next_veh$inPreciseTime == 1, next_veh$assumedTimeOut - as.difftime(1, units = "mins"),
                                            min_midpoint(next_veh$assumedTimeIn, curr_veh$assumedTimeOut))]
      }
      
      # if final observation, leave out time NA unless precise
      if(i == max(sub_clean_data$spot_index) & curr_veh$outPreciseTime == 0) {
        final_assumptions_set[tempid == singleSpot & spot_index == i, 
                           assumedTimeOut := NA]
      }
    
    #qc check
    final_assumptions_set[tempid == singleSpot & spot_index == i, checked := "yup"]
  }
}

# output
write.csv(final_assumptions_set, "~/acadia_parking_data_clean.csv", row.names = F)
