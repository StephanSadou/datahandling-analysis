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
  

# NASA: Creating seasonal classification for: 
# Plantation (Jul - Sep)
# Growth (Oct - Mar)
# Maturation (Apr - Jun)
# Harvest (Jul - Dec)

nasa_df <- read_csv(file.path(dir_stage, "nasa_stage_V2.csv"), show_col_types = FALSE) %>%
  rename(
    Date = date,
    Temperature = Temperature_in_degree_celsius, 
    Rainfall = Precipitation_in_mm, 
    Humidity = Humidity_percent, 
    SolarRadiation = Solar_radiation_kWh_m2
  ) %>% 
  mutate(
    Date = as_date(Date),
    Year  = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(7, 8, 9) ~ "Plantation",      
      Month %in% c(10, 11, 12, 1, 2, 3) ~ "Growth",   
      Month %in% c(4, 5, 6) ~ "Maturation",   
    ),
    Rainyday = as.integer(Rainfall >= 1), 
    Harvest = ifelse(Month %in% 7:12, 1 , 0) # Harvest flag
  ) %>%
  select(Year, Month, Season, Harvest, Temperature, Rainfall, Rainyday, Humidity, SolarRadiation)

View(nasa_df)

# Aggregate climate data by year and Season 
nasa_annual <- nasa_df %>% 
  group_by(Year, Season) %>%
  summarise(
    Total_Rainfall = sum(Rainfall, na.rm = TRUE), 
    Avg_Temperature = mean(Temperature, na.rm = TRUE),
    Avg_Humidity = mean(Humidity, na.rm = TRUE), 
    Avg_SolarRadiation = mean(SolarRadiation, na.rm = TRUE), 
    Total_Rainyday = sum(Rainyday, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = Season, 
    values_from = c(Total_Rainfall, Avg_Temperature, Avg_Humidity, Avg_SolarRadiation, Total_Rainyday),
    names_sep = "_"
  )

# ---- Preview ----
print(head(nasa_annual, 10))

csv_path <- file.path(dir_stage, "NASA_Dataprep.csv")
write.csv(nasa_annual, file=csv_path, row.names=FALSE)
