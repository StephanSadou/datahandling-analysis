# ---- NYC Yellow Taxi • Parquet demo (arrow) -------------------------------
# Goal: Read a Parquet file, select/filter, save cleaned Parquet,
# and produce one simple plot + one summary table.

# Packages
library("fs")
library("curl")
library("arrow")  
library("dplyr")
library("ggplot2")
library("lubridate")

# ---------------------------
# 0. Setting environment & current working directory 
# ---------------------------
# Get the current file directory where the script is located
source("get_cwd.R")
cwd <- get_script_dir()
nyc_folder <- file.path(cwd, "..", "skills_demo_nyc")

# We also have a results folder where we will store our output 
result_folder <- file.path(nyc_folder, "results")

# Checks if the folder exists or not - else creates it 
if (!dir.exists(result_folder)) {
  dir.create(result_folder, showWarnings = FALSE)
} 

# Defining the path folders 
data_raw   <- file.path(cwd, "..", "data_raw")
data_stage <- file.path(cwd, "..", "data_stage")
raw_file   <- file.path(data_raw, "yellow_tripdata_2024-01.parquet")  # <-- put your file here
clean_file <- file.path(data_stage, "taxi_clean.parquet")
plot_file1 <- file.path(result_folder, "trips_per_day.png")
plot_file2 <- file.path(result_folder, "Daily_average_fare_topyear.png")
sum_file   <- file.path(result_folder, "summary_by_day.csv")

# ---------------------------
# 1. Download the Yellow Taxi Parquet file + Read it 
# ---------------------------

# Define the parquet file 
raw_file <- file.path(data_raw, "yellow_tripdata_2024-01.parquet")

# Download the parquet file 
curl_download(url='https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2024-01.parquet',
              destfile=file.path(data_raw, "yellow_tripdata_2024-01.parquet"))

# Reading the Parquet file 
taxi_tbl <- read_parquet(raw_file)

# ---------------------------
# 2. Data Profiling 
# ---------------------------
# Identification of outliers using IQR method
# Outliers in Total_amount 

summary(taxi_tbl)
boxplot(taxi_tbl$total_amount,data=taxi_tbl)
box_stats_amount <- boxplot.stats(na.omit(taxi_tbl$total_amount))$stats

#  Outliers in Trip_distance
boxplot(taxi_tbl$trip_distance  ,data=taxi_tbl)
box_stats_distance <- boxplot.stats(na.omit(taxi_tbl$trip_distance))$stats

# ---------------------------
# 3. Data Cleansing 
# ---------------------------
 taxi_clean <- taxi_tbl %>%
  filter(!is.na(tpep_pickup_datetime),
         !is.na(trip_distance),
         !is.na(total_amount),
         trip_distance > 0, trip_distance < box_stats_distance[5],     # remove outliers in distance 
         total_amount >= 0, total_amount < box_stats_amount[5]) %>%    # remove outliers in total_amount 
  mutate(pickup_date = as_date(tpep_pickup_datetime)) %>%
  select(pickup_date, trip_distance, total_amount)

summary(taxi_clean)

# Save cleaned data back to Parquet 
write_parquet(taxi_clean, clean_file)

# ---------------------------
# 4. Data Analysis & Visualisation 
# ---------------------------
# Analysis 1: number of trips per day
trips_per_day <- taxi_clean %>%
  count(pickup_date)
summary(trips_per_day)

trips_per_day_plot <-ggplot(trips_per_day, aes(x = pickup_date, y = n)) +
  geom_col() +
  labs(title = "NYC Yellow Taxi — Trips per Day (sample)",
       x = "Date", y = "Number of trips") +
  theme_minimal(base_size = 12)

ggsave(plot_file1, trips_per_day_plot, width = 8, height = 4.5, dpi = 150)

# Analysis 2: average trip distance and average fare per day for top year
#2.1 Summary
summary_by_day <- taxi_clean %>% filter(pickup_date >= as.Date("2024-01-01")) %>%
  group_by(pickup_date) %>%
  summarise(avg_distance = mean(trip_distance),
            avg_fare     = mean(total_amount),
            trips        = n(), .groups = "drop")

write.csv(summary_by_day, sum_file, row.names = FALSE)

#2.2: Plot of daily average fare for top year

average_daily_fare <- ggplot(summary_by_day, aes(pickup_date, avg_fare)) +
  geom_col() +
  labs( title = paste("Average daily fare for top year: 2024"),x = "Day", y = "Average fare") +
  theme_minimal()

ggsave(plot_file2, average_daily_fare, width = 8, height = 4.5, dpi = 150)

cat("Done:\n",
    "- Cleaned Parquet ->", clean_file, "\n",
    "- Plot ->", plot_file2, "\n",
    "- Summary CSV ->", sum_file, "\n")
