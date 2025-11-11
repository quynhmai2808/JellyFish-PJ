
# Define directory
data_dir <- "/qa/data1/PMI/Archive/SQL_Output/"

retailer_name <- "EXXON"

# Find matching files
files <- list.files(path = data_dir, pattern = paste0("^", retailer_name, ".*\\.csv$"), full.names = TRUE)

# Extract type from filename (e.g., OTP, PMG, ECIG, CIG)
# Filename format: EXXON_<TYPE>_OUTLET_202540.csv
file_info <- data.frame(file_path = files, file_name = basename(files), stringsAsFactors = FALSE)

file_info$retailer <- sub("_.*", "", file_info$file_name)
file_info$type <- sub(paste0("^", retailer_name, "_([A-Z]+).*"), "\\1", file_info$file_name)
file_info$subtype <- ifelse(grepl("ECIG", file_info$type), str_extract(file_info$file_name, "(?<=OUTLET_)\\d{4}"), NA)
                            
# Combine type and subtype for unique grouping
file_info$type_group <- ifelse(!is.na(file_info$subtype),
                               paste0(file_info$type, "_", file_info$subtype),
                               file_info$type)


file_info$week <- sub(paste0("^", retailer_name, ".*(\\d{2})\\.csv$"), "\\1", file_info$file_name)
  

# Group files by type
type_groups <- split(file_info$file_name, file_info$type_group)

# Create a list of data frames grouped by type: type_group -> week -> data frame

# Split by type_group
data_by_group <- lapply(split(file_info, file_info$type_group), function(type_df) {
  
  # Split this subset by week
  data_week_lst <- lapply(split(type_df, type_df$week), function(week_df) {
    
    # Read all files for this week and combine into one data frame
    do.call(rbind, lapply(week_df$file_path, function(fp) {
      read_delim(fp, delim = ";", escape_double = FALSE, trim_ws = TRUE)
    }))
    
  })
})

# Optional: Name the lists by type_group and week for clarity
names(data_by_group) <- names(split(file_info, file_info$type_group))

lapply(names(data_by_group), function(g) {
  names(data_by_group[[g]]) <- names(split(file_info[file_info$type_group == g, ], file_info$week[file_info$type_group == g]))
})

# Check structure
# str(data_by_group, max.level = 3)

# # PMG Dataframe
# pmi_output_lst <- data_by_group$PMG
