# ===== Baseline GLM (OLS) for Sugarcane Production =====

library(dplyr)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)


# Call function from another R script to get current working directory 
source("get_cwd.R")
current_dir <- get_script_dir()

# From the R script, move to the root folder where all the other folders are present 
root_dir <- normalizePath(file.path(current_dir, ".."))

# Assign folder for raw and cleansed data
dir_raw   <- file.path(root_dir, "data_raw")
dir_stage <- file.path(root_dir, "data_stage")


# Read the FAOSTAT file and the NASA data preparation dataset 
faostat_df <- read.csv(file.path(dir_stage, "FAOSTAT_stageV2.csv")) %>% arrange(Year)
nasa_df <- read.csv(file.path(dir_stage, "NASA_Dataprep.csv"))

combined_df <- merge(nasa_df, faostat_df, by.x='agri_year', by.y='Year')  
combined_df <- subset(combined_df, select = -c(Item, Production))

# Create Year column and drop the agri_year column 
combined_df <- combined_df %>%
  mutate(Year = dplyr::coalesce(.data[["agri_year"]])) %>%
  arrange(Year)
combined_df <- subset(combined_df, select = -c(agri_year))

target <- "Yield"
area_col <- "Area_harvested" 

# All climate columns except the harvest period 
# Area Harvested, Year and Yield omitted 
climate_cols <- c(
  "Avg_Humidity_Maturation","Avg_Humidity_Growth","Avg_Humidity_Plantation",
  "Avg_Solar_Radiation_Maturation","Avg_Solar_Radiation_Growth","Avg_Solar_Radiation_Plantation",
  "Avg_Temperature_Maturation","Avg_Temperature_Growth","Avg_Temperature_Plantation",
  "Total_Rainfall_Maturation","Total_Rainfall_Growth","Total_Rainfall_Plantation",
  "Total_Rainy_days_Maturation","Total_Rainy_days_Growth","Total_Rainy_days_Plantation"
)

# 2) Time-aware split (last 5 years = test)
# ---------- 2) Chronological split: 70% train / 30% test ----------
n_total   <- nrow(combined_df)
split_idx <- floor(0.7 * n_total)
stopifnot(split_idx >= 1, split_idx < n_total)

train <- combined_df[seq_len(split_idx), , drop = FALSE]
test  <- combined_df[(split_idx + 1):n_total, , drop = FALSE]
split_year <- train$Year[split_idx]

# ---------- 3) Metrics ----------
rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
r2   <- function(a, p) 1 - sum((a - p)^2, na.rm = TRUE) / sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)


# Drop rows with NA in the columns needed for each model (separately for train/test)
needed_A <- c(target, climate_cols)
train_A  <- train %>% tidyr::drop_na(dplyr::all_of(needed_A))
test_A   <- test  %>% tidyr::drop_na(dplyr::all_of(needed_A))

needed_B <- c(target, climate_cols, area_col)
train_B  <- train %>% tidyr::drop_na(dplyr::all_of(needed_B))
test_B   <- test  %>% tidyr::drop_na(dplyr::all_of(needed_B))

# ---------- 4) MODEL A — Climate-only ----------
form_A <- reformulate(termlabels = climate_cols, response = target)
fit_A  <- lm(form_A, data = train_A)

cat("\n=== MODEL A (Climate-only) — OLS summary ===\n")
print(summary(fit_A))

pred_A_tr <- predict(fit_A, newdata = train_A)
pred_A_te <- predict(fit_A, newdata = test_A)
y_tr_A <- train_A[[target]]
y_te_A <- test_A[[target]]

cat("\nMODEL A — Train:\n")
cat(sprintf("R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_tr_A, pred_A_tr), rmse(y_tr_A, pred_A_tr), mae(y_tr_A, pred_A_tr)))
cat("MODEL A — Test (30% last years):\n")
cat(sprintf("R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_te_A, pred_A_te), rmse(y_te_A, pred_A_te), mae(y_te_A, pred_A_te)))

# ---------- 5) MODEL B — Climate + Area harvested ----------
terms_B <- c(climate_cols, area_col)

# reformulate() builds a safe formula even if column names have spaces/symbols
form_B <- reformulate(termlabels = terms_B, response = target)
fit_B  <- lm(form_B, data = train_B)

cat("\n=== MODEL B (Climate + Area) — OLS summary ===\n")
print(summary(fit_B))

pred_B_tr <- predict(fit_B, newdata = train_B)
pred_B_te <- predict(fit_B, newdata = test_B)
y_tr_B    <- train_B[[target]]
y_te_B    <- test_B[[target]]

cat("\nMODEL B — Train:\n")
cat(sprintf("R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_tr_B, pred_B_tr), rmse(y_tr_B, pred_B_tr), mae(y_tr_B, pred_B_tr)))
cat("MODEL B — Test (30% last years):\n")
cat(sprintf("R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_te_B, pred_B_te), rmse(y_te_B, pred_B_te), mae(y_te_B, pred_B_te)))



# ---------- 6) Full-series predictions (for plotting) ----------
# For plotting we predict over the whole df. For each model we must drop rows with missing predictors.
df_A_ready <- combined_df %>% tidyr::drop_na(dplyr::all_of(climate_cols))
df_B_ready <- combined_df %>% tidyr::drop_na(dplyr::all_of(c(climate_cols, area_col)))

df_plot <- combined_df %>%
  transmute(Year = Year,
            y_true = !!rlang::sym(target)) %>%
  left_join(
    df_A_ready %>% mutate(pred_A = as.numeric(predict(fit_A, newdata = df_A_ready))) %>% select(Year, pred_A),
    by = "Year"
  ) %>%
  left_join(
    df_B_ready %>% mutate(pred_B = as.numeric(predict(fit_B, newdata = df_B_ready))) %>% select(Year, pred_B),
    by = "Year"
  )

y_max <- max(df_plot$y_true, na.rm = TRUE)

ggplot() +
  annotate("rect", xmin = split_year, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_vline(xintercept = split_year, linetype = "dotted") +
  geom_line(data = df_plot, aes(Year, y_true, color = "Observed"), linewidth = 1) +
  geom_line(data = df_plot, aes(Year, pred_A, color = "GLM A (Climate-only)"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  geom_line(data = df_plot, aes(Year, pred_B, color = "GLM B (Climate + Area)"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  annotate("text", x = split_year - 0.5, y = y_max, label = "TRAIN", vjust = -0.3, size = 3.5) +
  annotate("text", x = split_year + 0.5, y = y_max, label = "TEST",  vjust = -0.3, size = 3.5) +
  scale_color_manual(values = c("Observed" = "black",
                                "GLM A (Climate-only)"   = "#1b9e77",
                                "GLM B (Climate + Area)" = "#d95f02")) +
  labs(
    title = "Sugarcane Production — GLM Predictions vs Observed",
    subtitle = "70% Train (left) / 30% Test (right) — split is vertical line & shaded area",
    x = "Year", y = "Yield", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")