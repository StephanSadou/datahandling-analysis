# ---- Packages ----
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Call function from another R script to get current working directory 
source("get_cwd.R")
current_dir <- get_script_dir()
  
# From the R script, move to the root folder where all the other folders are present 
root_dir <- normalizePath(file.path(current_dir, ".."))
  
# Assign folder for raw and cleansed data
dir_raw   <- file.path(root_dir, "data_raw")
dir_stage <- file.path(root_dir, "data_stage")
  
# ---- Load ----
df <- read_csv(file.path(dir_stage, "nasa_stage_V2.csv"), show_col_types = FALSE) %>%
  mutate(
    date  = as_date(date),
    year  = year(date),
    month = month(date)
  ) %>%
  filter(!is.na(date))

# ---- Agri year (July -> June) & Phases ----
# agri_year Y: 1 Jul Y .. 30 Jun Y+1
df <- df %>%
  mutate(
    agri_year = if_else(month >= 7, year, year - 1L),
    phase = case_when(
      month %in% 1:3 ~ "Harvest",      # Jan–Mar
      month %in% 4:6 ~ "Maturation",   # Apr–Jun
      month %in% 7:9 ~ "Plantation",   # Jul–Sep
      month %in% 10:12 ~ "Growth"      # Oct–Mar
    )
  ) 

# Rainy days 
df <- df %>% mutate(rainy_day = as.integer(Precipitation_in_mm >= 1))

# ---- Aggregate (by agri_year, phase) with YOUR rules ----
# Temperature_in_degree_celsius -> SUM
# Precipitation_in_mm           -> SUM
# Humidity_percent              -> MEAN
# Solar_radiation_kWh_m2        -> MEAN
# Rainy days                    -> SUM 
phase_summ <- df %>%
  group_by(agri_year, phase) %>%
  summarise(
    Avg_Humidity        = mean(Humidity_percent, na.rm = TRUE),
    Avg_Solar_Radiation = mean(Solar_radiation_kWh_m2, na.rm = TRUE),
    Avg_Temperature     = sum(Temperature_in_degree_celsius, na.rm = TRUE),  
    Total_Rainfall      = sum(Precipitation_in_mm, na.rm = TRUE),
    Total_Rainy_days    = sum(rainy_day, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Pivot to requested wide columns ----
nasa_phase_yearly <- phase_summ %>%
  tidyr::pivot_wider(
    names_from  = phase,
    values_from = c(Avg_Humidity, Avg_Solar_Radiation, Avg_Temperature, Total_Rainfall, Total_Rainy_days),
    names_sep   = "_"
  ) %>%
  arrange(agri_year)

# ---- Keep exactly the columns you asked for (Plantation/Growth/Maturation only) ----
keep_cols <- c(
  "agri_year",
  "Avg_Humidity_Maturation","Avg_Humidity_Growth","Avg_Humidity_Plantation",
  "Avg_Solar_Radiation_Maturation","Avg_Solar_Radiation_Growth","Avg_Solar_Radiation_Plantation",
  "Avg_Temperature_Maturation","Avg_Temperature_Growth","Avg_Temperature_Plantation",
  "Total_Rainfall_Maturation","Total_Rainfall_Growth","Total_Rainfall_Plantation",
  "Total_Rainy_days_Maturation","Total_Rainy_days_Growth","Total_Rainy_days_Plantation"
)

# Ensure stable schema even if a phase is missing for a year
for (cc in setdiff(keep_cols, names(nasa_phase_yearly))) {
  nasa_phase_yearly[[cc]] <- NA_real_
}

nasa_phase_yearly <- nasa_phase_yearly %>% select(any_of(keep_cols))

# ---- (Optional) Write to disk ----
# readr::write_csv(nasa_phase_yearly, "nasa_phase_yearly.csv")

# ---- Preview ----
print(head(nasa_phase_yearly, 10))

csv_path <- file.path(dir_stage, "NASA_Dataprep.csv")
write.csv(nasa_phase_yearly, file=csv_path, row.names=FALSE)
