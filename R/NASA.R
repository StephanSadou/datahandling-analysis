# Load all the required packages 
library(cli)      # for adding the progress bar 
library(httr)     # for GET() to request data from the web
library(jsonlite) # for fromJSON() to parse JSON into R objects
library(dplyr)    # for piping (%>%) and data wrangling
library(tidyr)    # for unnest_longer() to expand nested lists
library(tibble)   # for enframe() to turn lists into data-frames

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


# -------------------------------------------------------------- #
# ------ Step 1: Build the NASA POWER API request URL ---------- # 
# -------------------------------------------------------------- #

# Data granularity: Daily 
# Location: Port Louis, Mauritius 
# Weather Features: precipitation, average temperature, humidity, solar radiation

params <- c(
  "T2M",                 # avg daily temperature
  "PRECTOTCORR",         # precipitation (bias-corrected)
  "RH2M",                # humidity
  "ALLSKY_SFC_SW_DWN"    # solar radiation
)

nasa_api_url <- sprintf(
  paste0("https://power.larc.nasa.gov/api/temporal/daily/point?",
         "latitude=%s&longitude=%s&community=AG&parameters=%s&",
         "start=%s&end=%s&format=JSON"),
  -20.3, 57.5, paste(params, collapse = ","),
  20000101, 20241231
)


# ----------------------------------------------------------------------------- #
# ------ Step 2: Download the data & Extract Parameter section from JSON ------ # 
# ----------------------------------------------------------------------------- #

# Send GET request to the NASA POWER API that we have built
# nasa_raw is just a raw JSON response 
nasa_raw <- GET(nasa_api_url)|> content("text", encoding = "UTF-8")

# Saving the raw JSON into the data_raw folder 
writeLines(nasa_raw, file.path(dir_raw, "nasa_raw.json"))

# Use fromJSON from the jsonlite package to read the raw JSON and convert it into a list/dataframe 
# Specify the $properties$parameter to retrieve the climate data stored in JSON 
nasa_list <- fromJSON(nasa_raw)$properties$parameter


# -------------------------------------------------------------------- #
# ------ Step 3: Convert NASA POWER JSON into a tidy data frame ------ # 
# -------------------------------------------------------------------- #

nasa_df <- enframe(nasa_list, name = "variable", value = "ts") %>% 
  # Unnest each variable (T2M, PRECTOTCORR, RH2M, ALLSKY_SFC_SW_DWN)
  unnest_longer(ts, indices_to = "date", values_to = "value") %>%
  # Convert YYYYMMDD to proper Date format
  mutate(date = as.Date(date, "%Y%m%d"))


# ---------------------------------------------- #
# ------ Step 4: Reshape into wide format ------ # 
# ---------------------------------------------- #

# Currently nasa_df has 3 columns: variable (e.g. T2M), value (numeric), and date (Date)
# The final dataframe will have one row per date with tidy climate variables

final_nasa_pow <- nasa_df %>%
  # Spread variables into separate columns
  pivot_wider(names_from = variable, values_from = value) %>%
  
  # Rename columns into more human-readable labels
  rename(
    Temperature_in_degree_celsius = T2M,          # Avg daily temp (°C)
    Precipitation_in_mm = PRECTOTCORR,            # Precipitation (mm/day)
    Humidity_percent = RH2M,                      # Relative humidity (%)
    Solar_radiation_kWh_m2 = ALLSKY_SFC_SW_DWN    # Solar radiation (kWh/m²/day)
  )

# Optional: View our results and some key statistical figures 
# View(final_nasa_pow)
# summary(final_nasa_pow)

#Export the final dataframe into the data_stage folder 
csv_path <- file.path(dir_stage, "nasa_stage.csv")
write.csv(final_nasa_pow, file=csv_path, row.names=FALSE)

