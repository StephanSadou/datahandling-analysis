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
plot_file  <- file.path(script.dir, "figures", "trips_per_day.png")
sum_file   <- file.path(script.dir, "data_stage", "summary_by_day.csv")

# ---- Step 1: Read a single Parquet file -----------------------------------
taxi_tbl <- read_parquet(raw_file)

# --- Get an idea of the data set ---
taxi_tbl |> glimpse()




# Summarize number of trips per year
trips_per_year <- taxi_tbl %>%
  mutate(year = year(tpep_pickup_datetime)) %>%
  group_by(year) %>%
  summarise(num_trips = n()) %>%
  collect()  

# 1) Monthly counts using UTC-safe year-month strings
monthly <- taxi_tbl %>%
  mutate(ym = strftime(tpep_pickup_datetime, format = "%Y-%m")) %>%  # stays in Arrow/UTC
  group_by(ym) %>%
  summarise(num_trips = n()) %>%
  arrange(ym) %>%
  collect()

# 2) Pick the month with the highest trip count
top_ym <- monthly %>% slice_max(num_trips, n = 1) %>% pull(ym)

# 3) Daily counts for that month (still Arrow-first)
daily_top <- taxi_tbl %>%
  mutate(
    ym  = strftime(tpep_pickup_datetime, "%Y-%m"),
    day = as.Date(strftime(tpep_pickup_datetime, "%Y-%m-%d"))  # materializes as Date safely
  ) %>%
  filter(ym == top_ym) %>%
  group_by(day) %>%
  summarise(num_trips = n()) %>%
  arrange(day) %>%
  collect()

# 4) Plot daily counts for the top month
ggplot(daily_top, aes(day, num_trips)) +
  geom_col() +
  labs(
    title = paste("Daily Trips in", top_ym),
    x = "Day",
    y = "Number of trips"
  ) +
  theme_minimal()



# ---- Step 2: Select + basic cleaning --------------------------------------
taxi_clean <- taxi_tbl %>%
  select(tpep_pickup_datetime, trip_distance, total_amount) %>%
  # Basic sanity checks
  filter(!is.na(tpep_pickup_datetime),
         !is.na(trip_distance),
         !is.na(total_amount),
         trip_distance > 0, trip_distance < 50,     # remove impossible/extreme distances
         total_amount >= 0, total_amount < 500) %>% # drop nonsense fares
  mutate(pickup_date = as_date(tpep_pickup_datetime))

# ---- Step 3: Save cleaned data back to Parquet -----------------------------
write_parquet(taxi_clean, clean_file)

# ---- Step 4: ONE simple plot -----------------------------------------------
# Example: number of trips per day in the file
trips_per_day <- taxi_clean %>%
  count(pickup_date)

ggplot(trips_per_day, aes(x = pickup_date, y = n)) +
  geom_col() +
  labs(title = "NYC Yellow Taxi — Trips per Day (sample)",
       x = "Date", y = "Number of trips") +
  theme_minimal(base_size = 12)

ggsave(plot_file, width = 8, height = 4.5, dpi = 150)

# ---- Step 5: (Optional) ONE simple summary table ---------------------------
# Example: average trip distance and average fare per day
summary_by_day <- taxi_clean %>%
  group_by(pickup_date) %>%
  summarise(avg_distance = mean(trip_distance),
            avg_fare     = mean(total_amount),
            trips        = n(), .groups = "drop")

write.csv(summary_by_day, sum_file, row.names = FALSE)

cat("Done:\n",
    "- Cleaned Parquet ->", clean_file, "\n",
    "- Plot ->", plot_file, "\n",
    "- Summary CSV ->", sum_file, "\n")
