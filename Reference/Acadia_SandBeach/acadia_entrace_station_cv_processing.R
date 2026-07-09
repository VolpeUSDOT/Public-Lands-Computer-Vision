#### merging nps data for congestion management database

# loading the libraries I often work with
library(readxl)
library(data.table)
library(dplyr)

# establishing my base directory rather than actually changing my directory, I just find this easier
p_file_path <- "/Users/Nineveh.OConnell/OneDrive - DOT OST/volpe-proj-VXAGA1-NPS NERO - ACAD Data Collection/ACAD Data Collection/7- Video Analysis/SandBeachEntrance/"

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Load all processed Sand Beach Entrance Station Files --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# load all files in the folder, excluding the log
file_paths <- list.files(p_file_path, pattern = "cv_output")
ls_files <- lapply(file_paths, function(x) {
  in_file <- fread(paste0(p_file_path, x))
  })
names(ls_files) <- gsub("cv_output|.MP4.csv", "", file_paths)
stacked_sb_entrance_feed <- rbindlist(ls_files, idcol = "source_file", fill = T)

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Manually bridging the gaps between consecutive videos  --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# TEMP: subset to just be those ending in 140 for starters
stacked_sb_entrance_feed141 <- stacked_sb_entrance_feed[grepl("141$", source_file)]
stacked_sb_entrance_feed <- stacked_sb_entrance_feed[grepl("140$", source_file)]

# for each video segment, add the max timestamp of the previous segment
chunk_titles <- stacked_sb_entrance_feed[, unique(source_file)]
for(i in 2:length(chunk_titles)) {
  max_prev_timestamp <- stacked_sb_entrance_feed[source_file == chunk_titles[i - 1], max(timestamp)]
  stacked_sb_entrance_feed[source_file == chunk_titles[i], timestamp := timestamp + max_prev_timestamp]
}

stacked_sb_entrance_feed[source_file == "GH010140" & id == 442, `:=` (source_file = "GH010140_GH020140", id = 1442, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH020140" & id == 1, `:=` (source_file = "GH010140_GH020140", id = 1442, flag_bridge = 1)]

# there were no vehicles present between 02 and 03

stacked_sb_entrance_feed[source_file == "GH030140" & id == 340, `:=` (source_file = "GH030140_GH040140", id = 2340, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH040140" & id == 2, `:=` (source_file = "GH030140_GH040140", id = 2340, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH030140" & id == 365, `:=` (source_file = "GH030140_GH040140", id = 1365, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH040140" & id == 1, `:=` (source_file = "GH030140_GH040140", id = 1365, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH030140" & id == 389, `:=` (source_file = "GH030140_GH040140", id = 3389, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH040140" & id == 3, `:=` (source_file = "GH030140_GH040140", id = 3389, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH030140" & id == 373, `:=` (source_file = "GH030140_GH040140", id = 4373, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH040140" & id == 4, `:=` (source_file = "GH030140_GH040140", id = 4373, flag_bridge = 1)]

stacked_sb_entrance_feed[source_file == "GH040140" & id == 375, `:=` (source_file = "GH040140_GH050140", id = 3375, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 3, `:=` (source_file = "GH040140_GH050140", id = 3375, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH040140" & id == 391, `:=` (source_file = "GH040140_GH050140", id = 1391, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 1, `:=` (source_file = "GH040140_GH050140", id = 1391, flag_bridge = 1)]

stacked_sb_entrance_feed[source_file == "GH050140" & id == 418, `:=` (source_file = "GH050140_GH060140", id = 5418, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 5, `:=` (source_file = "GH050140_GH060140", id = 5418, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 470, `:=` (source_file = "GH050140_GH060140", id = 3470, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 3, `:=` (source_file = "GH050140_GH060140", id = 3470, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 479, `:=` (source_file = "GH050140_GH060140", id = 2479, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 2, `:=` (source_file = "GH050140_GH060140", id = 2479, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 522, `:=` (source_file = "GH050140_GH060140", id = 1522, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 1, `:=` (source_file = "GH050140_GH060140", id = 1522, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH050140" & id == 531, `:=` (source_file = "GH050140_GH060140", id = 4531, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 4, `:=` (source_file = "GH050140_GH060140", id = 4531, flag_bridge = 1)]

stacked_sb_entrance_feed[source_file == "GH060140" & id == 512, `:=` (source_file = "GH060140_GH070140", id = 5512, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 5, `:=` (source_file = "GH060140_GH070140", id = 5512, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 542, `:=` (source_file = "GH060140_GH070140", id = 2542, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 2, `:=` (source_file = "GH060140_GH070140", id = 2542, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 569, `:=` (source_file = "GH060140_GH070140", id = 4569, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 4, `:=` (source_file = "GH060140_GH070140", id = 4569, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 548, `:=` (source_file = "GH060140_GH070140", id = 1548, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 1, `:=` (source_file = "GH060140_GH070140", id = 1548, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH060140" & id == 559, `:=` (source_file = "GH060140_GH070140", id = 3559, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 3, `:=` (source_file = "GH060140_GH070140", id = 3559, flag_bridge = 1)]

stacked_sb_entrance_feed[source_file == "GH070140" & id == 329, `:=` (source_file = "GH070140_GH080140", id = 7329, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 7, `:=` (source_file = "GH070140_GH080140", id = 7329, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 375, `:=` (source_file = "GH070140_GH080140", id = 3375, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 3, `:=` (source_file = "GH070140_GH080140", id = 3375, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 383, `:=` (source_file = "GH070140_GH080140", id = 5383, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 5, `:=` (source_file = "GH070140_GH080140", id = 5383, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 387, `:=` (source_file = "GH070140_GH080140", id = 6387, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 6, `:=` (source_file = "GH070140_GH080140", id = 6387, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 394, `:=` (source_file = "GH070140_GH080140", id = 1394, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 1, `:=` (source_file = "GH070140_GH080140", id = 1394, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 334, `:=` (source_file = "GH070140_GH080140", id = 8334, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 8, `:=` (source_file = "GH070140_GH080140", id = 8334, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 402, `:=` (source_file = "GH070140_GH080140", id = 2402, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 2, `:=` (source_file = "GH070140_GH080140", id = 2402, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH070140" & id == 419, `:=` (source_file = "GH070140_GH080140", id = 4419, flag_bridge = 1)]
stacked_sb_entrance_feed[source_file == "GH080140" & id == 4, `:=` (source_file = "GH070140_GH080140", id = 4419, flag_bridge = 1)]

# make a unique id
stacked_sb_entrance_feed[, unique_id := paste(source_file, id, sep = "_")]

############ let's do this all again for the 141s 

# for each video segment, add the max timestamp of the previous segment
chunk_titles141 <- stacked_sb_entrance_feed141[, unique(source_file)]
for(i in 2:length(chunk_titles141)) {
  max_prev_timestamp <- stacked_sb_entrance_feed141[source_file == chunk_titles141[i - 1], max(timestamp)]
  stacked_sb_entrance_feed141[source_file == chunk_titles141[i], timestamp := timestamp + max_prev_timestamp]
}

stacked_sb_entrance_feed141[source_file == "GH010141" & id == 665, `:=` (source_file = "GH010141_GH020141", id = 5662, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 5, `:=` (source_file = "GH010141_GH020141", id = 5662, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 670, `:=` (source_file = "GH010141_GH020141", id = 6670, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 6, `:=` (source_file = "GH010141_GH020141", id = 6670, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 679, `:=` (source_file = "GH010141_GH020141", id = 3679, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 3, `:=` (source_file = "GH010141_GH020141", id = 3679, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 682, `:=` (source_file = "GH010141_GH020141", id = 2682, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 2, `:=` (source_file = "GH010141_GH020141", id = 2682, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 683, `:=` (source_file = "GH010141_GH020141", id = 8683, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 8, `:=` (source_file = "GH010141_GH020141", id = 8683, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 692, `:=` (source_file = "GH010141_GH020141", id = 1692, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 1, `:=` (source_file = "GH010141_GH020141", id = 1692, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH010141" & id == 694, `:=` (source_file = "GH010141_GH020141", id = 4694, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 4, `:=` (source_file = "GH010141_GH020141", id = 4694, flag_bridge = 1)]

stacked_sb_entrance_feed141[source_file == "GH020141" & id == 459, `:=` (source_file = "GH020141_GH030141", id = 2459, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 2, `:=` (source_file = "GH020141_GH030141", id = 2459, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH020141" & id == 504, `:=` (source_file = "GH020141_GH030141", id = 1504, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 1, `:=` (source_file = "GH020141_GH030141", id = 1504, flag_bridge = 1)]

stacked_sb_entrance_feed141[source_file == "GH030141" & id == 588, `:=` (source_file = "GH030141_GH040141", id = 4588, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 4, `:=` (source_file = "GH030141_GH040141", id = 4588, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 656, `:=` (source_file = "GH030141_GH040141", id = 2656, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 2, `:=` (source_file = "GH030141_GH040141", id = 2656, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 663, `:=` (source_file = "GH030141_GH040141", id = 6663, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 6, `:=` (source_file = "GH030141_GH040141", id = 6663, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 667, `:=` (source_file = "GH030141_GH040141", id = 5667, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 5, `:=` (source_file = "GH030141_GH040141", id = 5667, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 669, `:=` (source_file = "GH030141_GH040141", id = 3669, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 3, `:=` (source_file = "GH030141_GH040141", id = 3669, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH030141" & id == 672, `:=` (source_file = "GH030141_GH040141", id = 1672, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 1, `:=` (source_file = "GH030141_GH040141", id = 1672, flag_bridge = 1)]

stacked_sb_entrance_feed141[source_file == "GH040141" & id == 687, `:=` (source_file = "GH040141_GH050141", id = 2687, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 2, `:=` (source_file = "GH040141_GH050141", id = 2687, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 675, `:=` (source_file = "GH040141_GH050141", id = 3675, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 3, `:=` (source_file = "GH040141_GH050141", id = 3675, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 708, `:=` (source_file = "GH040141_GH050141", id = 4708, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 4, `:=` (source_file = "GH040141_GH050141", id = 4708, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 725, `:=` (source_file = "GH040141_GH050141", id = 5725, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 5, `:=` (source_file = "GH040141_GH050141", id = 5725, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 746, `:=` (source_file = "GH040141_GH050141", id = 1746, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 1, `:=` (source_file = "GH040141_GH050141", id = 1746, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 668, `:=` (source_file = "GH040141_GH050141", id = 7668, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 7, `:=` (source_file = "GH040141_GH050141", id = 7668, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH040141" & id == 762, `:=` (source_file = "GH040141_GH050141", id = 8762, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 8, `:=` (source_file = "GH040141_GH050141", id = 8762, flag_bridge = 1)]

stacked_sb_entrance_feed141[source_file == "GH050141" & id == 665, `:=` (source_file = "GH050141_GH060141", id = 1665, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 1, `:=` (source_file = "GH050141_GH060141", id = 1665, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 674, `:=` (source_file = "GH050141_GH060141", id = 2674, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 2, `:=` (source_file = "GH050141_GH060141", id = 2674, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 643, `:=` (source_file = "GH050141_GH060141", id = 3643, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 3, `:=` (source_file = "GH050141_GH060141", id = 3643, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 653, `:=` (source_file = "GH050141_GH060141", id = 4653, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 4, `:=` (source_file = "GH050141_GH060141", id = 4653, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 649, `:=` (source_file = "GH050141_GH060141", id = 6649, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 6, `:=` (source_file = "GH050141_GH060141", id = 6649, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 647, `:=` (source_file = "GH050141_GH060141", id = 5647, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 5, `:=` (source_file = "GH050141_GH060141", id = 5647, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH050141" & id == 634, `:=` (source_file = "GH050141_GH060141", id = 8634, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 8, `:=` (source_file = "GH050141_GH060141", id = 8634, flag_bridge = 1)]

stacked_sb_entrance_feed141[source_file == "GH060141" & id == 744, `:=` (source_file = "GH060141_GH070141", id = 1744, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 1, `:=` (source_file = "GH060141_GH070141", id = 1744, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 762, `:=` (source_file = "GH060141_GH070141", id = 3762, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 3, `:=` (source_file = "GH060141_GH070141", id = 3762, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 761, `:=` (source_file = "GH060141_GH070141", id = 2761, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 2, `:=` (source_file = "GH060141_GH070141", id = 2761, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 757, `:=` (source_file = "GH060141_GH070141", id = 4757, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 4, `:=` (source_file = "GH060141_GH070141", id = 4757, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 729, `:=` (source_file = "GH060141_GH070141", id = 6729, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 6, `:=` (source_file = "GH060141_GH070141", id = 6729, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH060141" & id == 697, `:=` (source_file = "GH060141_GH070141", id = 5697, flag_bridge = 1)]
stacked_sb_entrance_feed141[source_file == "GH070141" & id == 5, `:=` (source_file = "GH060141_GH070141", id = 5697, flag_bridge = 1)]

#check in on this 734 or 729 situation here after the merge
# make a unique id
stacked_sb_entrance_feed141[, unique_id := paste(source_file, id, sep = "_")]

#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#
# Preliminary analysis  --------------------
#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#

# TEMP: let's merge them for initial analysis, apply the false and temporary assumption that 141 videos immediately follow 140 videos
stacked_sb_entrance_feed <- rbind(stacked_sb_entrance_feed, stacked_sb_entrance_feed141[, timestamp := timestamp + 5369.947])

# minutes covered
stacked_sb_entrance_feed[, max(timestamp)/60]

# only confidence greater than 35%
stacked_sb_entrance_feed <- stacked_sb_entrance_feed[confidence_numeric > 0.35]

# all vehicles should be in frame long enough to be confidently recognized 3 times over the course of 1.5 seconds
stacked_sb_entrance_feed[, times_recognized := .N, unique_id]
stacked_sb_entrance_feed <- stacked_sb_entrance_feed[times_recognized >= 3]

# additionally cut lane classifications by y expectation -- only preprocessed x in the python script
# i forget why I made that decision, but here we are
stacked_sb_entrance_feed[lane_id_minx == 1, lane_id_minx := ifelse(cy >= 630 & cy <= 800, 1, NA)]
stacked_sb_entrance_feed[lane_id_minx == 2, lane_id_minx := ifelse(cy >= 560 & cy <= 650, 2, NA)]
stacked_sb_entrance_feed[lane_id_minx == 3, lane_id_minx := ifelse(cy >= 530 & cy <= 600, 3, NA)]

# number of cars that went through, by lane
stacked_sb_entrance_feed[lane_id_minx == lane_id_maxx, uniqueN(unique_id), lane_id_minx]

# processing time by lane on average
tbl_per_vehicle <- stacked_sb_entrance_feed[!is.na(lane_id_minx) & lane_id_minx == lane_id_maxx, 
                                            .(processing_time = max(timestamp) - min(timestamp), veh_types = paste0(unique(class), collapse = ", ")), 
                                            .(unique_vehicle_id = unique_id, entry_lane_id = lane_id_minx)]
# let's add the time between processed frames to processing time, assuming vehicles were likely in the area for half of the time before and half of the time after
tbl_per_vehicle[, processing_time := processing_time + 0.4]

# summarize processing time over this entire undefined time period so far
tbl_per_vehicle[, .(count_vehicles = .N, avg = mean(processing_time), median = median(processing_time), min = min(processing_time), max = max(processing_time)), entry_lane_id]

# output for mike: tbl_per_vehicle with the time included as well

#thinking about on street parking
#i think an approach that could make sense is:
#look at the video for a bit, how visible are vehicles level with the front of ali's car
#i'm guessing the front of ali's car should stay at the same level pixel wise in the 
#frame, so the information I want to take down is the time at which the rear of a parked car
#is level with the front of ali's car and the time at which the front of a parked car is 
#leve with front of ali's car. All parked cars should be to the right of the front of ali's car
#want the model to also recognize people, and would want to generally pin if they are to the right
#of the car, crossing in front of Ali's car, or to the left of the car
#run computer vision for the whole stretc

