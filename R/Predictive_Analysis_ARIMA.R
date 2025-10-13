# ===== ARIMA / ARIMAX for Sugarcane Yield using combined_df =====

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast) 

# Call function from another R script to get current working directory 
source("get_cwd.R")
current_dir <- get_script_dir()

# From the R script, move to the root folder where all the other folders are present 
root_dir <- normalizePath(file.path(current_dir, ".."))

# Assign folder for raw and cleansed data
dir_raw   <- file.path(root_dir, "data_raw")
dir_stage <- file.path(root_dir, "data_stage")

# ---------- 0) Prepare data ----------

# Read the FAOSTAT file and the NASA data preparation dataset 
faostat_df <- read.csv(file.path(dir_stage, "FAOSTAT_stageV2.csv")) %>% arrange(Year)
nasa_df <- read.csv(file.path(dir_stage, "NASA_Dataprep.csv"))

combined_df <- merge(nasa_df, faostat_df, by='Year')  
combined_df <- subset(combined_df, select = -c(Item, Production))

target <- "Yield"
area_col <- "Area_harvested" 

# Select climate/phase features (same patterning as before)

climate_cols <- c(
  "Avg_Humidity_Maturation","Avg_Humidity_Growth","Avg_Humidity_Plantation",
  "Avg_SolarRadiation_Maturation","Avg_SolarRadiation_Growth","Avg_SolarRadiation_Plantation",
  "Avg_Temperature_Maturation","Avg_Temperature_Growth","Avg_Temperature_Plantation",
  "Total_Rainfall_Maturation","Total_Rainfall_Growth","Total_Rainfall_Plantation",
  "Total_Rainyday_Maturation","Total_Rainyday_Growth","Total_Rainyday_Plantation"
)
terms_B      <- c(climate_cols, area_col)

# ---------- 1) Chronological 70/30 split ----------
n_total   <- nrow(combined_df)
split_idx <- floor(0.7 * n_total); stopifnot(split_idx >= 5, split_idx < n_total)  # need enough points
split_year <- combined_df$Year[split_idx]

train <- combined_df[seq_len(split_idx), , drop = FALSE]
test  <- combined_df[(split_idx + 1):n_total, , drop = FALSE]

# ---------- 2) Metric helpers ----------
rmse_fn <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
mae_fn  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
r2_fn   <- function(a, p) 1 - sum((a - p)^2, na.rm = TRUE) / sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)

# ---------- 3) MODEL A — Univariate ARIMA on Yield ----------
# Keep Year for plotting, but DO NOT feed it to the model
train_A <- train %>% select(Year, all_of(target)) %>% drop_na()
test_A  <- test  %>% select(Year, all_of(target)) %>% drop_na()

y_tr_A <- train_A[[target]]
y_te_A <- test_A[[target]]

fit_A <- auto.arima(
  y             = y_tr_A,
  seasonal      = FALSE,    # annual data (non-seasonal)
  stepwise      = FALSE,
  approximation = FALSE
)

fc_A       <- forecast(fit_A, h = nrow(test_A))
pred_A_te  <- as.numeric(fc_A$mean)
pred_A_tbl <- tibble::tibble(Year = test_A$Year, pred_A = pred_A_te)

cat("\n=== ARIMA Model A (univariate) ===\n")
cat(sprintf("Test -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2_fn(y_te_A, pred_A_te), rmse_fn(y_te_A, pred_A_te), mae_fn(y_te_A, pred_A_te)))

# ---------- 4) MODEL B — ARIMAX (exogenous = climate + Area) ----------
# Keep Year for plotting; predictors = terms_B (NO Year)
needed_B <- c(target, terms_B)

train_B <- train %>% select(Year, all_of(needed_B)) %>% drop_na()
test_B  <- test  %>% select(Year, all_of(needed_B))  %>% drop_na()

y_tr_B  <- train_B[[target]]
y_te_B  <- test_B[[target]]

xreg_tr <- as.matrix(train_B[, terms_B, drop = FALSE])  # <-- Year NOT included
xreg_te <- as.matrix(test_B[,  terms_B, drop = FALSE])  # <-- Year NOT included

fit_B <- auto.arima(
  y             = y_tr_B,
  xreg          = xreg_tr,
  seasonal      = FALSE,
  stepwise      = FALSE,
  approximation = FALSE
)

fc_B       <- forecast(fit_B, xreg = xreg_te, h = nrow(test_B))
pred_B_te  <- as.numeric(fc_B$mean)
pred_B_tbl <- tibble::tibble(Year = test_B$Year, pred_B = pred_B_te)

cat("\n=== ARIMAX Model B (with exogenous regressors) ===\n")
cat(sprintf("Test -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2_fn(y_te_B, pred_B_te), rmse_fn(y_te_B, pred_B_te), mae_fn(y_te_B, pred_B_te)))
