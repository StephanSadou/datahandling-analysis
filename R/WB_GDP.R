# Load all the required packages
library(httr)
library(jsonlite)
library(dplyr)

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


# --------------------------------------------------------- #
# ----- Step 1: Create a function to build the API UR ----- # 
# --------------------------------------------------------- #

wdi_url <- function(
    country = "MUS", # Mauritius ISOcode
    indicator = "NV.AGR.TOTL.ZS", # Agriculture GDP share (% of GDP)
    start = 2000, # Year range 2000-2024 
    end = 2024
    ) 
{sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&per_page=1000&date=%s:%s",
         country, indicator, start, end)}

# --------------------------------------------------------- #
# ---------- Step 2: Download and parse the JSON ---------- # 
# --------------------------------------------------------- #

# We already have predefined parameters in the function so we can just call it 
wb_raw <- fromJSON(wdi_url(), simplifyDataFrame = TRUE)

# Output the raw JSON file into a csv file 
write.csv(wb_raw[[2]], file=file.path(dir_raw, "wb_raw.csv"),row.names=FALSE)


# --------------------------------------------------------- #
# ----------- Step 3: Extract and tidy the data ----------- # 
# --------------------------------------------------------- #

wb_df <- wb_raw[[2]] |>
  select(countryiso3code, country, date, value) |>
  mutate(
    date = as.integer(date), # keep only the year as integer
    #indicator.value = as.character(indicator.value),
    unit = "%",
    countryiso3code = as.character(countryiso3code),
    value = as.numeric(value)
  ) |>
  arrange(date)

# Output the World Bank dataframe into a csv file in the same location as the script 
write.csv(wb_df, file=file.path(dir_stage, "wb_stage.csv"), row.names=FALSE)


# Optional - View the dataset in RStudio 
# View(wb_df)
# View(wb_raw)


