# =============================================================================
# data.R
# -----------------------------------------------------------------------------
# Responsibilities:
#   1.  Scan the entire SQL_Output directory once to retrieve the list of file names only (without reading CSV contents). 
#       This list will be used to populate the Retailer, Report, and Week dropdowns in the dashboard.
# 
#   2. Parse file names in a robust manner:
#       Do not make assumptions about retailer names.
#       Support retailer names containing multiple words and/or multiple underscores.
#       Example: TABAK_BRUCKER_PMG_202601.csv.
# 
#   3. Provide a lazy-loading data retrieval function:
#       Read only the specific CSV file(s) required when the user selects Retailer + Report + Week and clicks the button.
#       Do not load all CSV files from the directory when the application starts.
# 
#   4. Standardize column names for each report type at the data source level, so that dashboard.R does not 
#       need to infer column mappings or apply setNames() based on report_name.
# =============================================================================

# Folder containing reports that will be released to customers.
data_dir <- "/qa/data1/PMI/Archive/SQL_Output/"

REPORT_TYPE_PATTERNS <- c(
  "ECIG_OUTLET_9961", "ECIG_OUTLET_9962", "ECIG_OUTLET_9963",
  "OTP_OUTLET", "CIG_OUTLET", "PMG"
)

# Clean type for having only main report type: PMG, OTP, CIG, ECIG_9961,...)
clean_report_label <- function(raw_type) {
  raw_type <- sub("^ECIG_OUTLET_", "ECIG_", raw_type)
  raw_type <- sub("_OUTLET$", "", raw_type)
  raw_type
}

# -----------------------------------------------------------------------------
# 1. Parse FILENAME -> retailer / report type / year / week
# -----------------------------------------------------------------------------
# Filename format: <Retailer>_<REPORT_TYPE>_<yyyyww>.csv
# Retailer can contents underscore (vd TABAK_BRUCKER), so not used as default
# Instead of, anchor from RIGHT: find REPORT_TYPE + yyyyww + .csv,
# the rest link is retailer name.
parse_report_filenames <- function(file_names) {
  
  type_alt <- paste(REPORT_TYPE_PATTERNS, collapse = "|")
  pattern  <- paste0("^(.+)_(", type_alt, ")_([0-9]{4})([0-9]{2})\\.csv$") # 3 capture groups: 
  
  # base::regexec() = finds the starting position and length of the full match and each capture group.
  # base::regmatches() = uses those positions and lengths to extract the actual matched strings.
  
  m <- regmatches(file_names, regexec(pattern, file_names)) 
  
  parsed <- lapply(m, function(x) {
    if (length(x) == 0) return(c(NA, NA, NA, NA))
    x[2:5]  # retailer, report_type, year, week
  })
  
  parsed_df <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
  names(parsed_df) <- c("retailer", "report_type_raw", "year", "week")
  
  parsed_df$year <- as.integer(parsed_df$year)
  parsed_df$week <- as.integer(parsed_df$week)
  parsed_df$report_type <- clean_report_label(parsed_df$report_type_raw)
  
  parsed_df
}

# -----------------------------------------------------------------------------
# 2. Build FILE INDEX (only metadata, not read data)
# -----------------------------------------------------------------------------
build_file_index <- function(dir_path = data_dir) {
  
  all_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(all_files) == 0) {
    return(data.frame(
      file_path = character(0), file_name = character(0),
      retailer = character(0), report_type = character(0),
      year = integer(0), week = integer(0), week_label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  file_names <- basename(all_files)
  parsed <- parse_report_filenames(file_names)
  
  file_index <- data.frame(
    file_path   = all_files,
    file_name   = file_names,
    retailer    = parsed$retailer,
    report_type = parsed$report_type,
    year        = parsed$year,
    week        = parsed$week,
    stringsAsFactors = FALSE
  )
  
  # Skip files whose names do not follow the naming convention
  file_index <- file_index[!is.na(file_index$retailer), ]
  
  #Week display label in the format "2025-W46" to distinguish weeks from different years
  file_index$week_label <- sprintf("%d-W%02d", file_index$year, file_index$week)
  
  file_index
}

# Run once at application startup: only list file names, without reading CSV contents,
# to populate all dynamic dropdowns.
file_index <- build_file_index()

# -----------------------------------------------------------------------------
# 3. Functions for retrieving dropdown option lists based on the pre-built file_index.
# -----------------------------------------------------------------------------
get_retailers <- function() {
  sort(unique(file_index$retailer))
}

get_report_types <- function(retailer) {
  sort(unique(file_index$report_type[file_index$retailer == retailer]))
}

# List of years with available data for the selected retailer and report type.
get_available_years <- function(retailer, report_type){
  sub_idx <- file_index[file_index$retailer == retailer &
                          file_index$report_type == report_type,]
  
  sort(unique(sub_idx$year))
}


# List of available weeks (week numbers only, e.g., 41, 42, ..., 46)
# for the selected retailer, report type, and year.
# A year must be selected first, since week numbers are not unique across years.
get_available_weeks <- function(retailer, report_type, year) {
  sub_idx <- file_index[file_index$retailer == retailer &
                          file_index$report_type == report_type &
                          file_index$year == year, ]
  sort(unique(sub_idx$week))
}

# -----------------------------------------------------------------------------
# 4. Apply report-specific column name mappings to standardize source data.
# -----------------------------------------------------------------------------
standardize_columns <- function(df, report_type) {
  
  base_type <- if (grepl("^ECIG", report_type)) "ECIG" else report_type
  
  new_names <- switch(base_type,
                      "PMG"  = c("FileName", "Date", "Store", "ProductNr", "Description",
                                 "Sales", "Sticks", "Revenue"),
                      "OTP"  = c("FileName", "Date", "Store", "Total_G", "Revenue"),
                      "CIG"  = c("FileName", "Date", "Store", "Sticks", "Revenue"),
                      "ECIG" = c("FileName", "Date", "Store", "Sales", "Revenue"),
                      NULL
  )
  
  if (is.null(new_names) || ncol(df) != length(new_names)) {
    warning(sprintf(
      "standardize_columns(): Column count mismatch for report type '%s': actual = %d, expected = %s. Original column names will be preserved.",
      ncol(df), length(new_names), report_type
    ))
    return(df)
  }
  
  setNames(df, new_names)
}

# Using ISO-8601 Rule: converts an ISO year/week combination into the corresponding month (YYYYMM) 
# by determining the Thursday of that ISO week and returning the year-month of that date.
# Using Base R (no package lubridate).
iso_week_to_month <- function(year, week) {
  jan4 <- as.Date(paste0(year, "-01-04"))
  wday_jan4 <- as.integer(format(jan4, "%u"))     # 1 = Monday ... 7 = Sunday
  week1_monday <- jan4 - (wday_jan4 - 1)
  target_thursday <- week1_monday + (week - 1) * 7 + 3
  as.integer(format(target_thursday, "%Y%m"))
}

# "Volume" is a generic column used across report types for the common
# "Sales Volume" KPI and Average Price calculations. Its meaning depends on
# the report type, as each report uses a different unit of measure
# (e.g., Sales, Sticks, or Total_G).

get_volume_column <- function(report_type) {
  base_type <- if (grepl("^ECIG", report_type)) "ECIG" else report_type
  switch(base_type,
         "PMG"  = "Sales",
         "OTP"  = "Total_G",
         "CIG"  = "Sticks",
         "ECIG" = "Sales",
         NA_character_
  )
}

# -----------------------------------------------------------------------------
# 5. Load data on demand (lazy loading) for reading CSV files.
# This is the only function responsible
# -----------------------------------------------------------------------------
# weeks_selected: vector WEEK (vd 46, or c(41, 42, ..., 46) in a YEAR.
# Give back ONE data.frame aggregated across all weeks, column names have been standardized
# and additional derived columns have been added: Week_Label & Month.
read_report_data <- function(retailer, report_type, year, weeks_selected) {
  
  weeks_selected <- as.integer(week_selected)
  
  req_rows <- file_index[
    file_index$retailer == retailer &
      file_index$report_type == report_type &
      file_index$year == year &
      file_index$week %in% weeks_selected,
  ]
  
  if (nrow(req_rows) == 0) return(NULL)
  
  df_list <- lapply(seq_len(nrow(req_rows)), function(i) {
    row <- req_rows[i, ]
    raw_df <- readr::read_delim(row$file_path, delim = ";", escape_double = FALSE,
                         trim_ws = TRUE, show_col_types = FALSE)
    std_df <- standardize_columns(as.data.frame(raw_df), report_type)
    std_df$Week_Label <- row$week
    std_df$Month <- iso_week_to_month(row$year, row$week)
    std_df
  })
  
  dplyr::bind_rows(df_list)
}
