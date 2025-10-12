# ===== Random Forest (ranger) for Sugarcane Yield using combined_df =====

library(dplyr)
library(tidyr)
library(ggplot2)
library(ranger)   # fast RF with permutation importance

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

combined_df <- merge(nasa_df, faostat_df, by.x='agri_year', by.y='Year')  
combined_df <- subset(combined_df, select = -c(Item, Production))

# Create Year column and drop the agri_year column 
combined_df <- combined_df %>%
  mutate(Year = dplyr::coalesce(.data[["agri_year"]])) %>%
  arrange(Year)
combined_df <- subset(combined_df, select = -c(agri_year))

target   <- "Yield"
area_col <- "Area_harvested"

# climate features (incl. rainy days & harvest/phase metrics)
climate_cols <- c(
  "Avg_Humidity_Maturation","Avg_Humidity_Growth","Avg_Humidity_Plantation",
  "Avg_Solar_Radiation_Maturation","Avg_Solar_Radiation_Growth","Avg_Solar_Radiation_Plantation",
  "Avg_Temperature_Maturation","Avg_Temperature_Growth","Avg_Temperature_Plantation",
  "Total_Rainfall_Maturation","Total_Rainfall_Growth","Total_Rainfall_Plantation",
  "Total_Rainy_days_Maturation","Total_Rainy_days_Growth","Total_Rainy_days_Plantation"
)

# ---------- 1) Chronological 70/30 split ----------
n_total   <- nrow(combined_df)
split_idx <- floor(0.7 * n_total); stopifnot(split_idx >= 1, split_idx < n_total)

train <- combined_df[seq_len(split_idx), , drop = FALSE]
test  <- combined_df[(split_idx + 1):n_total, , drop = FALSE]
split_year <- train$Year[split_idx]

# ---------- 2) Metrics ----------
rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
r2   <- function(a, p) 1 - sum((a - p)^2, na.rm = TRUE) / sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)

# Prepare model frames (drop rows with NA only where required)
needed_A <- c(target, climate_cols)
train_A  <- train %>% drop_na(all_of(needed_A))
test_A   <- test  %>% drop_na(all_of(needed_A))

needed_B <- c(target, climate_cols, area_col)
train_B  <- train %>% drop_na(all_of(needed_B))
test_B   <- test  %>% drop_na(all_of(needed_B))

# ---------- 3) MODEL A — Climate-only RF ----------
form_A <- reformulate(termlabels = climate_cols, response = target)

fit_A <- ranger(
  formula         = form_A,
  data            = train_A,
  num.trees       = 1000,
  mtry            = max(1, floor(sqrt(length(climate_cols)))),
  min.node.size   = 5,
  importance      = "permutation",   # safe even if not used later
  respect.unordered.factors = "order",
  seed            = 42
)

pred_A_tr <- predict(fit_A, data = train_A)$predictions
pred_A_te <- predict(fit_A, data = test_A)$predictions
y_tr_A    <- train_A[[target]]
y_te_A    <- test_A[[target]]

cat("\n=== MODEL A (Climate-only) — Random Forest ===\n")
cat(sprintf("Train  -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_tr_A, pred_A_tr), rmse(y_tr_A, pred_A_tr), mae(y_tr_A, pred_A_tr)))
cat(sprintf("Test   -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_te_A, pred_A_te), rmse(y_te_A, pred_A_te), mae(y_te_A, pred_A_te)))

# ---------- 4) MODEL B — Climate + Area RF ----------
terms_B <- c(climate_cols, area_col)
form_B  <- reformulate(termlabels = terms_B, response = target)

fit_B <- ranger(
  formula         = form_B,
  data            = train_B,
  num.trees       = 1000,
  mtry            = max(1, floor(sqrt(length(terms_B)))),
  min.node.size   = 5,
  importance      = "permutation",
  respect.unordered.factors = "order",
  seed            = 42
)

pred_B_tr <- predict(fit_B, data = train_B)$predictions
pred_B_te <- predict(fit_B, data = test_B)$predictions
y_tr_B    <- train_B[[target]]
y_te_B    <- test_B[[target]]

cat("\n=== MODEL B (Climate + Area) — Random Forest ===\n")
cat(sprintf("Train  -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_tr_B, pred_B_tr), rmse(y_tr_B, pred_B_tr), mae(y_tr_B, pred_B_tr)))
cat(sprintf("Test   -> R²: %.3f | RMSE: %0.0f | MAE: %0.0f\n",
            r2(y_te_B, pred_B_te), rmse(y_te_B, pred_B_te), mae(y_te_B, pred_B_te)))

# ---------- 5) Full-series predictions for plotting ----------
# Predict where predictors exist (keep NA where we can't predict)
df_A_ready <- combined_df %>% drop_na(all_of(climate_cols))
df_B_ready <- combined_df %>% drop_na(all_of(c(climate_cols, area_col)))

df_plot <- combined_df %>%
  transmute(Year, y_true = !!rlang::sym(target)) %>%
  left_join(df_A_ready %>%
              mutate(pred_A = as.numeric(predict(fit_A, data = df_A_ready)$predictions)) %>%
              select(Year, pred_A),
            by = "Year") %>%
  left_join(df_B_ready %>%
              mutate(pred_B = as.numeric(predict(fit_B, data = df_B_ready)$predictions)) %>%
              select(Year, pred_B),
            by = "Year")

y_max <- max(df_plot$y_true, na.rm = TRUE)

ggplot() +
  annotate("rect", xmin = split_year, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_vline(xintercept = split_year, linetype = "dotted") +
  geom_line(data = df_plot, aes(Year, y_true, color = "Observed"), linewidth = 1) +
  geom_line(data = df_plot, aes(Year, pred_A, color = "RF A (Climate-only)"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  geom_line(data = df_plot, aes(Year, pred_B, color = "RF B (Climate + Area)"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  annotate("text", x = split_year - 0.5, y = y_max, label = "TRAIN", vjust = -0.3, size = 3.5) +
  annotate("text", x = split_year + 0.5, y = y_max, label = "TEST",  vjust = -0.3, size = 3.5) +
  scale_color_manual(values = c("Observed" = "#472575",
                                "RF A (Climate-only)"   = "#228b8d",
                                "RF B (Climate + Area)" = "#b3dd2d")) +
  labs(
    title = "Sugarcane Yield — Random Forest Predictions vs Observed",
    subtitle = "70% Train / 30% Test (chronological)",
    x = "Year", y = "Yield", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ---------- 6) Feature importance (Model B) ----------
imp <- sort(fit_B$variable.importance, decreasing = TRUE)
imp_df <- tibble::tibble(Feature = names(imp), Importance = as.numeric(imp))

print(head(imp_df, 15))

ggplot(imp_df %>% slice_head(n = 15),
       aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Random Forest (Model B) — Permutation Importance (top 15)",
       x = NULL, y = "Importance") +
  theme_minimal(base_size = 12)
