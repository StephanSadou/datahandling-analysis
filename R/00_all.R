# Main script to run all the other scripts to extract the Mauritius datasets 

# --------------------------------------------------------------------------- #
# ----- Phase 0 : Ensure that all the required packages are installed  ------ # 
# ----- Phase 1 : Retrieve data from the FAOSTAT R package  ----------------- # 
# ----- Phase 2 : Obtaining data from the NASA POWER API  ------------------- #
# ----- Phase 3 : Retrieve data from the FAOSTAT R package  ----------------- # 
# --------------------------------------------------------------------------- #

message("Checking if all the required packages are installed...")
source("packages_installation.R")

library(cli)
data_pipeline <- function() {
  cli_progress_step("Phase 1: Retrieving the FAOSTAT data")
  source("FAOSTAT_V2.R")
  cli_progress_step("Phase 2: Retrieving the NASA POWER data")
  source("NASA_V2.R")
  cli_progress_step("Phase 3: Retrieving the World Bank data.")
  source("WB_GDP_V2.R")
  cli_progress_step("Phase 4: Data loading stage")
  source("Data_Loading.R")
  
  cli_progress_step("Phase 5: Analysis Stage")
  source(file.path(getwd(), "Analysis", "Descriptive", "FAOSTAT.r"))
  source(file.path(getwd(), "Analysis", "Descriptive", "NASA_FINAL_v3.r"))
  source(file.path(getwd(), "Analysis", "Descriptive", "WB_FINAL_v2.r"))
  source(file.path(getwd(), "Analysis", "Explanatory", "Explanatory_Analysis_Final.R"))
  source(file.path(getwd(), "Analysis", "Predictive", "Predictive_Analysis_Final.R"))
  # cli_progress_step("Phase 6: Part B - NYC Taxi Exercise")
  # source("<Part B script goes here>")
}

data_pipeline()
