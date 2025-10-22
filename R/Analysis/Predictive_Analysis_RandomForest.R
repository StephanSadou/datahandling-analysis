# ===== Random Forest (ranger) for Sugarcane Yield using combined_df =====

library(gt)
library(DBI)
library(dplyr)
library(readr)
library(tidyr)
library(doParallel) # parallel Cross-validation to speed things up
library(caret)      # cross-validation, training wrappers
library(ranger)     # fast Random Forest engine
library(Metrics)
library(ggplot2)
library(RMariaDB)
library(rprojroot)
library(lubridate)

# ---------------------------
# 1. Connect to the database to read view
# ---------------------------

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
yield_climate_df <- dbReadTable(con, "current_compiled_data")

# Close connection to database 
dbDisconnect(con)

# ---------------------------
# 2. Feature Engineering & Feature Selection
# ---------------------------
# We are trying to predict Yield --> yield is the dependent variables 
# The other factors that we are using to predict yield --> independent variables (features)
target <- "Yield"
predictors <- c("Avg_Temperature_Growth","Total_Rainfall_Plantation" )


# Select only the column that interest us in our predictive model 
# We select the year column as well --> Will be used later for plotting 
yield_climate_df  <- yield_climate_df  %>% select(c("Harvest_Year", predictors, target))


# ---------------------------
# 3. Data Splitting (Train/Test)
# ---------------------------
# Performing a chronological split of the data
# 80% will be used for training & 30% will be used for testing 
n_total   <- nrow(yield_climate_df)
split_idx <- floor(0.8 * n_total)
stopifnot(split_idx >= 1, split_idx < n_total)

# We do the splitting for (A) Compiled data & (B) Compiled Lagged data 
train <- yield_climate_df[seq_len(split_idx), , drop = FALSE]
test <- yield_climate_df[(split_idx + 1):n_total, , drop = FALSE]
split_year <- train$Harvest_Year[split_idx]


# ---------------------------
# 4. Defining Metrics for Model Evaluation 
# ---------------------------
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

# ---------------------------
# 4. Model Definitions 
# ---------------------------

# Drop rows with NA in the columns needed for each model (separately for train/test)
train_df  <- train %>% select(c(predictors, target)) %>%
  drop_na(all_of(c(predictors, target)))
test_df   <- test  %>% select(c(predictors, target)) %>%
  drop_na(all_of(c(predictors, target)))

# ---------------------------
# 5. RF Models Training & Hyperparameter Tuning
# ---------------------------
# Random Forest (ranger) — Strong CV + Wider Grid using caret
# Goal: Find good hyperparameters with 10x repeated CV, then evaluate on test.
# Assumes the below already created:
#   - predictors:  character vector of feature names (length p)
#   - target:      single string naming the target column
#   - train, test: cleaned data with identical columns

# We will be using a caret tuning pipeline and returns: 
# A fitted model, predictions, and metrics table (model eval) for both train and test.

# Define model formula and problem size 
# Turn vector of predictors + target into a standard R formula: target ~ x1 + x2 + ...
set.seed(42)
form <- reformulate(predictors, target)
p <- length(predictors)
  
# Parallel processing setup 
# Use all but one CPU core to make cross-validation (CV) much faster.
cl <- makeCluster(max(1, parallel::detectCores() - 1))
registerDoParallel(cl)

# Cross-validation plan
# We’ll use 10-fold CV repeated 3 times (= 30 resamples per hyperparameter combo).
k_folds <- 10
repeats <- 3
  
# Build a wider, sensible hyperparameter grid 
# Random Forest key knobs (for regression with ranger via caret):
#  - mtry: how many variables to try at each split (search wide around sqrt(p))
#  - min.node.size: minimum samples in a leaf (smaller → deeper trees)
#  - splitrule: "variance" for regression (fixed here)
mtry_candidates <- unique(sort(pmax(1L, round(c(
  sqrt(p) * c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 2),  # around sqrt(p)
  seq(1, p, length.out = min(10, p))              # spread across full range if p is small
)))))

min_node_candidates <- c(1L, 2L, 3L, 5L, 7L, 10L, 15L, 20L, 30L)
  
tune_grid <- expand.grid(
  mtry          = mtry_candidates,
  splitrule     = "variance",
  min.node.size = min_node_candidates
)
  
# caret’s parallel CV requires a specific seed structure to be fully reproducible.
# This functions creates a list of seeds: one vector per resample + one final seed.
make_seeds <- function(n_resamples, grid_size, seed = 42) {
  set.seed(seed)
  c(replicate(n_resamples, sample.int(1e6, grid_size, replace = TRUE), simplify = FALSE),
  list(sample.int(1e6, 1)))
}
  
# Tell caret how to run cross validation and keep it reproducible
n_resamples <- k_folds * repeats
ctrl <- trainControl(
  method           = "repeatedcv",
  number           = k_folds,
  repeats          = repeats,
  verboseIter      = TRUE,
  allowParallel    = TRUE,
  seeds            = make_seeds(n_resamples, nrow(tune_grid), seed = 42),
  savePredictions  = "final",
  returnResamp     = "all"
)
  
# Training + Hyperparameter tuning of model
# caret will:
#   1) try every row in tune_grid,
#   2) run 10x3 CV for each,
#   3) pick the best model by RMSE,
#   4) refit the best model on the *full* training set.
model <- train(
  form,
  data      = train_df,
  method    = "ranger",
  trControl = ctrl,
  tuneGrid  = tune_grid,
  metric    = "RMSE",
  importance = "permutation",
  num.trees  = 1500,
  respect.unordered.factors = "order",
  seed = 42
)
  
# Always stop the cluster after training to free resources.
stopCluster(cl); registerDoSEQ()
  
# Get fitted values on the TRAIN set (in-sample predictions).
# Get predictions on the TEST set (unseen data you never trained on).
# Extract the ground-truth target values from TRAIN/TEST.
pred_train <- predict(model, newdata = train_df)
pred_test  <- predict(model, newdata = test_df)
truth_train <- train_df[[target]]
truth_test  <- test_df[[target]]
  
# ---------------------------
# 6. Prediction & Models Evaluation
# ---------------------------

# Final Output
# - model:       the tuned caret model object (includes CV results, fitted final model, etc.)
# - best_params: the best hyperparameters found by caret (mtry, min.node.size, ...)
# - train_metrics/test_metrics: Model evaluation metrics (R2, RMSE, MAE, MSE, MAPE)
# - model_evaluation: Table that contain the metrics for the 
rf_model_final <- list(
  model          = model,
  best_params    = model$bestTune,
  model_summary  = summary(model),
  train_metrics  = metrics_tbl(truth_train, pred_train),
  test_metrics   = metrics_tbl(truth_test,  pred_test),
  model_evaluation = rbind(
    cbind(Set = "Train", metrics_tbl(truth_train, pred_train)),
    cbind(Set = "Test",  metrics_tbl(truth_test,  pred_test))
  )
)
# Output  the results for the best Random Forest model 
rf_model_final$best_params
rf_model_final$model_summary
rf_model_final$model_evaluation

# ---------------------------
# 7. Full-series predictions for plotting
# ---------------------------
# Predict where predictors exist (keep NA where we can't predict)
# First we take all the data for the original compiled data 
model_df <- yield_climate_df %>%
  drop_na(all_of(c(predictors, target)))

# Using the best RF model 
model_df <- model_df %>%
  mutate(Predictions = as.numeric(predict(rf_model_final$model, newdata = model_df)))


# ── 3) Join actuals with both sets of predictions by Harvest_Year ──────────────
# Keep only the columns we need from each side for a clean join.
df_plot <- yield_climate_df %>% select(Harvest_Year, target) %>% # actuals (e.g., Yield)
  rename(Actual = target) %>%
  left_join(
    model_df %>% select(Harvest_Year, Predictions),
    by = "Harvest_Year"
  )

y_max <- max(df_plot$Actual, na.rm = TRUE)

ggplot() +
  annotate("rect", xmin = split_year, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_vline(xintercept = split_year, linetype="dotted") +
  geom_line(data = df_plot, aes(Harvest_Year, Actual, color = "Observed"), linewidth = 1) +
  geom_line(data = df_plot, aes(Harvest_Year, Predictions, color = "Random Forest Model"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  annotate("text", x = split_year - 2, y = y_max, label = "TRAIN", vjust = -0.3, size = 3.5, color="#472575") +
  annotate("text", x = split_year + 2, y = y_max, label = "TEST",  vjust = -0.3, size = 3.5, color="red") +
  scale_color_manual(values = c("Observed" = "black",
                                "Random Forest Model" = "#228b8d")) + 
  labs(
    title = "Sugarcane Production — Random Forest Predictions vs Observed",
    subtitle = "80% Train (left) / 20% Test (right) — split is vertical line & shaded area",
    x = "Year", y = "Yield", color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray50", margin = margin(t = 10)), # Caption at bottom left
    axis.title = element_text(size = 14, face = "bold", margin = margin(t = 10, b = 0)),
    axis.text = element_text(size = 11, color = "gray30"),
    legend.position = "top", # Move legend to top for better use of space, 
    panel.grid.minor = element_blank(), # Remove minor gridlines for cleaner look
    panel.grid.major.x = element_line(color = "grey90", linetype = "dotted", linewidth = 0.3), # Subtle vertical grid
    panel.grid.major.y = element_line(color = "grey90", linetype = "solid", linewidth = 0.5), # More prominent horizontal grid
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black", size = 0.6),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )
