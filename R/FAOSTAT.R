# Load all the required packages
library(cli)
library(dplyr)
library(tidyr)
library(FAOSTAT)
library(data.table)

# ----------------------------------------------------------------- #
# ----- Step 0: Initialize the folder paths to save the files ----- # 
# ----------------------------------------------------------------- #

# Call function from another R script to get current working directory 
source("get_cwd.R") 
current_dir <- get_script_dir()

# From the R script, move to the root folder where all the other folders are present 
root_dir <- normalizePath(file.path(current_dir, ".."))

# Assign folder for raw and cleansed data
dir_raw   <- file.path(root_dir, "data_raw")
dir_stage <- file.path(root_dir, "data_stage")


# --------------------------------------------------------------------- #
# ----- Step 1: Download FAOSTAT QCL bulk (goes to raw directory) ----- # 
# --------------------------------------------------------------------- #

# This downloads the latest bulk files, including the Normalized one, into dir_raw.
# cat(sprintf("Downloading FAOSTAT QCL bulk dataset to: %s", dir_raw))

# Function to suppress messages when downloading FAOSTAT data 
quiet <- function(expr) {
  tf <- file()
  sink(tf)                     # capture stdout
  sink(tf, type = "message")   # capture messages
  on.exit({sink(); sink(type = "message"); close(tf)}, add = TRUE)
  suppressWarnings(suppressMessages(eval.parent(substitute(expr))))
}

crops_info <- quiet(FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = "data_raw"))

# crops_info <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = dir_raw)


# --------------------------------------------------------------------- #
# ----------- Step 2: Find and unzip the 'Normalized' file ------------ # 
# --------------------------------------------------------------------- #
norm_zip <- list.files(dir_raw,
                       pattern = "Normalized.*\\.zip$",
                       full.names = TRUE,
                       ignore.case = TRUE)

if (length(norm_zip) == 0L) {
  stop("Could not find the 'Normalized' zip file in ", dir_raw,
       ". Contents:\n", paste(list.files(dir_raw), collapse = "\n"))
}

# Extract content from the zip files 
unzipped_files <- unzip(norm_zip[1], exdir = dir_raw, overwrite = TRUE)

# Locate the normalized CSV (either from unzip result or by pattern)
norm_csv <- unzipped_files[grepl("\\.csv$", unzipped_files, ignore.case = TRUE)]
if (!length(norm_csv)) {
  # Fallback: search the folder
  norm_csv <- list.files(dir_raw,
                         pattern = "Normalized.*\\.csv$",
                         full.names = TRUE,
                         ignore.case = TRUE)
}

if (length(norm_csv) == 0L) {
  stop("Could not find the Normalized CSV after unzipping in ", dir_raw)
}

norm_csv <- norm_csv[1]
# cat(sprintf("Normalized CSV found: %s", norm_csv))


# --------------------------------------------------------------------- #
# ------ Step 3: Read, filter, and select the columns in FAOSTAT ------ # 
# --------------------------------------------------------------------- #
crops <- data.table::fread(norm_csv, nThread = max(1, parallel::detectCores() - 1)) %>%
  dplyr::filter(Area == "Mauritius",
                Year >= 2000,
                !is.na(Value)) %>%
  dplyr::select(Area, Item, Element, Year, Value, Unit)

# Check our resulting table
# View(crops)

# Saved our results into a csv file 
cleansed_csv_path <- file.path(dir_stage, "FAOSTAT_stage.csv")
data.table::fwrite(crops, cleansed_csv_path)

# cat(sprintf("Cleansed CSV written to: %s", cleansed_csv_path))
