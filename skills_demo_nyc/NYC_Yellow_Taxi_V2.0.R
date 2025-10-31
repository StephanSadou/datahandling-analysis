# ---- NYC Yellow Taxi • Parquet demo (arrow) -------------------------------
# Goal: Read a Parquet file, select/filter, save cleaned Parquet,
# and produce one simple plot + one summary table.

install.packages("fs")
install.packages("tzdb")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("rstudioapi")

# Packages
library(arrow)   # read_parquet(), write_parquet(), open_dataset()
library(dplyr)
library(lubridate)
library(ggplot2)
library(fs)      # tidy paths (optional but nice)
library(rstudioapi)

# Set the environment to the folder where the script is located 
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)


# ---- Paths (keep it project-relative) -------------------------------------
# Put raw file(s) in:   data_raw/
# Clean output in:      data_stage/
# Figures in:           figures/

folders <- c("data_raw", "data_stage", "figures")

for (f in folders) {
  folder_path <- file.path(script.dir, f)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
}

# Files inside their respective folders
raw_file   <- file.path(script.dir, "data_raw", "yellow_tripdata_2024-01.parquet")  # <-- put your file here
clean_file <- file.path(script.dir, "data_stage", "taxi_clean.parquet")
plot_file1  <- file.path(script.dir, "figures", "trips_per_day.png")
plot_file2  <- file.path(script.dir, "figures", "Daily_average_fare_topyear.png")
sum_file   <- file.path(script.dir, "data_stage", "summary_by_day.csv")

# ---- Step 1: Import Data -----------------------------------

taxi_tbl <- read_parquet(raw_file)

# ---- Step 2: Data Profiling --------------------------------------

#2.1 Identification of outliers using IQR method
#2.1.1 Outliers in Total_amount 

summary(taxi_tbl)
boxplot(taxi_tbl$total_amount,data=taxi_tbl)
box_stats_amount <- boxplot.stats(na.omit(taxi_tbl$total_amount))$stats
View(c(box_stats[1],box_stats[5]))


#2.1.2 Outliers in Trip_distance
boxplot(taxi_tbl$trip_distance  ,data=taxi_tbl)
box_stats_distance <- boxplot.stats(na.omit(taxi_tbl$trip_distance))$stats
View(c(box_stats_distance[1],box_stats_distance[5]))


# ---- Step 3: Data Cleansing --------------------------------------

 taxi_clean <- taxi_tbl %>%
  filter(!is.na(tpep_pickup_datetime),
         !is.na(trip_distance),
         !is.na(total_amount),
         trip_distance > 0, trip_distance < box_stats_distance[5],     # remove outliers in distance 
         total_amount >= 0, total_amount < box_stats_amount[5]) %>%    # remove outliers in total_amount 
  mutate(pickup_date = as_date(tpep_pickup_datetime)) %>%
  select(pickup_date, trip_distance, total_amount)

summary(taxi_clean)


# ---- Step 3: Save cleaned data back to Parquet -----------------------------
write_parquet(taxi_clean, clean_file)

# ---- Step 4: Data Analysis & Visualisation -----------------------------------------------
# Analysis 1: number of trips per day
trips_per_day <- taxi_clean %>%
  count(pickup_date)
summary(trips_per_day)

trips_per_day <-ggplot(trips_per_day, aes(x = pickup_date, y = n)) +
  geom_col() +
  labs(title = "NYC Yellow Taxi — Trips per Day (sample)",
       x = "Date", y = "Number of trips") +
  theme_minimal(base_size = 12)

ggsave(plot_file1, width = 8, height = 4.5, dpi = 150)



# Analysis 2: average trip distance and average fare per day for top year
#2.1 Summary
summary_by_day <- taxi_clean %>% filter(pickup_date >= as.Date("2024-01-01")) %>%
  group_by(pickup_date) %>%
  summarise(avg_distance = mean(trip_distance),
            avg_fare     = mean(total_amount),
            trips        = n(), .groups = "drop")
View(summary_by_day)

write.csv(summary_by_day, sum_file, row.names = FALSE)

#2.2: Plot of daily average fare for top year

ggplot(summary_by_day, aes(pickup_date, avg_fare)) +
  geom_col() +
  labs( title = paste("Average daily fare for top year: 2024"),x = "Day", y = "Average fare") +
  theme_minimal()

ggsave(plot_file2, width = 8, height = 4.5, dpi = 150)

cat("Done:\n",
    "- Cleaned Parquet ->", clean_file, "\n",
    "- Plot ->", plot_file, "\n",
    "- Summary CSV ->", sum_file, "\n")
