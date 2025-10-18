# ===== Random Forest (ranger) for Sugarcane Yield using combined_df =====

library(gt)
library(DBI)
library(dplyr)
library(readr)
library(tidyr)
library(ranger)
library(Metrics)
library(ggplot2)
library(RMariaDB)
library(rprojroot)
library(lubridate)

# -------------------------------------------------------- #
# ----- Step 1: Connect to the database to read view ----- # 
# -------------------------------------------------------- #

# Read the environment file to obtain the database credentials 
root <- find_root(has_file(".Renviron"))
readRenviron(file.path(root, ".Renviron"))

# Use the credentials to connect to our local database 
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Use connection to connect to the combined view for FAOSTAT datasets
compiled_data <- dbReadTable(con, "current_compiled_data")
compiledlagged_data <- dbReadTable(con, "lagged_compiled_data")

# Close connection to database 
dbDisconnect(con)

# -------------------------------------------------------- #
# ----- Step 2: Feature Engineering & Selection ---------- # 
# -------------------------------------------------------- #

# We are trying to predict Yield --> yield is the dependent variables 
# The other factors that we are using to predict yield --> independent variables (features)

target <- "Yield"
predictors <- c("Avg_Temperature_Plantation", "Avg_Temperature_Growth", 
                "Avg_Temperature_Maturation", "Total_Rainfall_Plantation", 
                "Total_Rainfall_Growth", "Total_Rainfall_Maturation",
                "Total_rainy_days_Plantation", "Total_rainy_days_Growth", 
                "Total_rainy_days_Maturation")

# Fields in compiled lagged data is different 
predictors_lagged <- c("Avg_Temperature_Plantation_lag1", "Avg_Temperature_Growth_lag1", 
                       "Avg_Temperature_Maturation_lag1", "Total_Rainfall_Plantation_lag1", 
                       "Total_Rainfall_Growth_lag1", "Total_Rainfall_Maturation_lag1",
                       "Total_rainy_days_Plantation_lag1", "Total_rainy_days_Growth_lag1", 
                       "Total_rainy_days_Maturation_lag1")

# Select only the column that interest us in our predictive model 
# We select the same columns from both the compiled and the compiled lagged data
compiled_data <- compiled_data %>% select(c("Harvest_Year", predictors, target))
compiledlagged_data <- compiledlagged_data %>% select(c("Harvest_Year", predictors_lagged, target))

# -------------------------------------------------------- #
# -------- Step 3: Data Splitting (Train/Test) ----------- # 
# -------------------------------------------------------- #

# Performing a chronological split of the data
# 70% will be used for training & 30% will be used for testing 
n_total   <- nrow(compiled_data)
split_idx <- floor(0.7 * n_total)
stopifnot(split_idx >= 1, split_idx < n_total)

# We do the splitting for (A) Compiled data & (B) Compiled Lagged data 
train_A <- compiled_data[seq_len(split_idx), , drop = FALSE]
test_A  <- compiled_data[(split_idx + 1):n_total, , drop = FALSE]

train_B <- compiledlagged_data[seq_len(split_idx), , drop = FALSE]
test_B  <- compiledlagged_data[(split_idx + 1):n_total, , drop = FALSE]
split_year <- train_A$Year[split_idx]


# Define Metrics for Model Evaluation 

# R2 coefficient 
r2 <- function(obs, pred) 1 - sum((obs - pred)^2)/sum((obs - mean(obs))^2)

# Using the metrics library we will be evaluating the RMSE, MAE, MSE & MAPE 
metrics_tbl <- function(obs, pred) {
  data.frame(
    R2   = r2(obs, pred), 
    RMSE = rmse(obs, pred),
    MAE  = mae(obs, pred),
    MSE  = mse(obs, pred),
    MAPE = mape(obs, pred) * 100 
  )
}

# -------------------------------------------------------- #
# ------------- Step 4: Models Definitions --------------- # 
# -------------------------------------------------------- #

# Model A --> Model A is the compiled data only 
# Model B --> Model B is the compiled lagged data 

# Drop rows with NA in the columns needed for each model (separately for train/test)

train_A  <- train_A %>% tidyr::drop_na(dplyr::all_of(c(predictors, target))) 
test_A   <- test_A  %>% tidyr::drop_na(dplyr::all_of(c(predictors, target)))

train_B  <- train_B %>% tidyr::drop_na(dplyr::all_of(c(predictors_lagged, target)))
test_B   <- test_B  %>% tidyr::drop_na(dplyr::all_of(c(predictors_lagged, target)))



# ----------------------------------------------------------------------- #
# --------------- Step 5: Random Forest Models Training ----------------- # 
# ----------------------------------------------------------------------- #

#  MODEL A — Compiled data only 
form_A <- reformulate(termlabels = predictors, response = target)

fit_A <- ranger(
  formula         = form_A,
  data            = train_A,
  num.trees       = 1000,
  mtry            = max(1, floor(sqrt(length(predictors)))),
  min.node.size   = 3,
  importance      = "permutation",   # safe even if not used later
  respect.unordered.factors = "order",
  seed            = 42
)

# MODEL B — Compiled lagged data 
form_B  <- reformulate(termlabels = predictors_lagged, response = target)
fit_B <- ranger(
  formula         = form_B,
  data            = train_B,
  num.trees       = 1000,
  mtry            = max(1, floor(sqrt(length(predictors_lagged)))),
  min.node.size   = 5,
  importance      = "permutation",
  respect.unordered.factors = "order",
  seed            = 42
)

# ----------------------------------------------------------------------- #
# -------------- Step 5: Prediction & Models Evaluation ----------------- # 
# ----------------------------------------------------------------------- #

# Using the fitted Model A to predict on training data & then on testing data 
ModelA_training_values <- train_A[[target]]
ModelA_testing_values <- test_A[[target]]
ModelA_training_predictions <- predict(fit_A, newdata = train_A)
ModelA_testing_predictions <- predict(fit_A, newdata = test_A)

# Using the fitted Model B to predict on training data & then on testing data 
ModelB_training_values <- train_B[[target]]
ModelB_testing_values <- test_B[[target]]
ModelB_training_predictions <- predict(fit_B, newdata = train_B)$predictions
ModelB_testing_predictions <- predict(fit_B, newdata = test_B)$predictions

cat("\n=== MODEL A (Climate-only) — OLS summary ===\n")
print(summary(fit_A))

rbind(
  cbind(Set = "Train", metrics_tbl(ModelA_training_values, ModelA_training_predictions)),
  cbind(Set = "Test",  metrics_tbl(ModelA_testing_values,  ModelA_testing_predictions))
)


cat("\n=== MODEL B (Climate + Area) — OLS summary ===\n")
print(summary(fit_B))

rbind(
  cbind(Set = "Train", metrics_tbl(ModelB_training_values, ModelB_training_predictions)),
  cbind(Set = "Test",  metrics_tbl(ModelB_testing_values,  ModelB_testing_predictions))
)

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
