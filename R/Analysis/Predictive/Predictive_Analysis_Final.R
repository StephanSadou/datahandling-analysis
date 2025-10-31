library(gt)       
library(dplyr)    
library(readr)    
library(tidyr)
library(caret) 
library(tibble)
library(ranger)  
library(Metrics)
library(ggplot2)
library(forecast)
library(doParallel) 

# ---------------------------
# (A) ARIMA Predictive Model (Baseline)
# ---------------------------

# ---------------------------
# 0. Getting current path of predictive analysis folder 
# ---------------------------
source("get_cwd.R") # Invoke script from root project folder 
cwd <- get_script_dir()
predictive_folder <- file.path(cwd, 'Analysis', "Predictive")
result_folder <- file.path(predictive_folder, "results")

# ---------------------------
# 1. Load the model produced in the explanatory analysis 
# --------------------------- 
# Read Explanatory model from Explanatory analysis folder  
explanatory_folder <- file.path(predictive_folder, "../Explanatory") 
explanatory_model <- read_csv(file.path(explanatory_folder, "results", "GAM_model.csv")) %>% arrange(Harvest_Year)

# ---------------------------
# 2. Create time-series dataset based on real yield values: 
# Using data prepped from the explanatory part
# Baseline model: ARIMA
# ---------------------------
yield_ts <- ts(explanatory_model$Yield_Value,
               start = min(explanatory_model$Harvest_Year),
               frequency = 1)

# ---------------------------
# 3. Data Splitting (Train/Test)
# Proportion for Training and Testing Sets:80%/20% 
# Proportion selected is as per research paper stated in report
# ---------------------------
n <- nrow(explanatory_model)
split_idx <- floor(0.8 * n)
train_end_year  <- explanatory_model$Harvest_Year[split_idx]
test_start_year <- train_end_year + 1

# Using window function, we slice through original time series (yield_ts) into training and testing segments.
# This ensures the time index remains perfectly aligned and continuous.
yield_train_ts <- window(yield_ts, end   = train_end_year)
yield_test_ts  <- window(yield_ts, start = test_start_year)

# ---------------------------
# 4. Fitting ARIMA model on the training dataset 
# We also forecast for the years in the testing dataset 
# ---------------------------

# auto.arima will select the best order (p,q,d) for the model 
arima_model <- auto.arima(yield_train_ts)
arima_forecast  <- forecast(arima_model, h = length(yield_test_ts))

# ---------------------------
# 5. Model Evaluation 
# Residual diagnostics + 
# We also evaluate Model Accuracy using the forecast values against the test data
# ---------------------------

# ACF graph should shows no sudden spikes & p value >0.05
# demonstrating residuals being white noise)
# Save the residuals plots in an image 
arima_rsd_plt <- file.path(result_folder, "ARIMA_residuals_check.png")
png(arima_rsd_plt, width = 1400, height = 900, res = 150)
checkresiduals(arima_model)
dev.off()

# Model Accuracy Evaluation 
accuracy_results <- forecast::accuracy(arima_forecast, yield_test_ts)
print(accuracy_results)

# ---------------------------
# 6. Plotting the ARIMA model for visualization 
# ---------------------------

# Obtaining the years for training, testing & forecast 
train_years <- as.integer(time(yield_train_ts))
test_years  <- as.integer(time(yield_test_ts))
fit_years   <- as.integer(time(fitted(arima_model)))
fc_years    <- as.integer(time(arima_forecast$mean))

# Create a dataframe using tibble of actual yield values for the whole period 
actual_df <- tibble(
  Year  = c(train_years, test_years),
  Value = c(as.numeric(yield_train_ts), as.numeric(yield_test_ts))
) %>%
  arrange(Year) %>%
  mutate(Part = "Actual")

# Build a tidy data frame of MODEL values for plotting (fitted on train + forecast on test)
model_df_plot <- tibble(
  Year   = c(fit_years, fc_years),
  Value  = c(as.numeric(fitted(arima_model)),       as.numeric(arima_forecast$mean)),
  Series = c(rep("ARIMA fitted (train)", length(fitted(arima_model))),
             rep("ARIMA forecast",       length(arima_forecast$mean)))
)

# Obtain the minimum year of the plot (x-axis) & highest yield values (y-axis)
# Those 2 variables will be then used for annotating the plot 
min_year <- min(actual_df$Year)
ymax_val <- max(c(actual_df$Value, model_df_plot$Value), na.rm = TRUE)

# Define the ggplot theme that will be reused on other subsequent plots 
ggplot_theme = theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
  axis.title = element_text(size = 14, face = "bold", margin = margin(t = 10, b = 0)),
  axis.text  = element_text(size = 11, color = "gray30"),
  legend.position = "top",
  legend.title = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_line(color = "grey90", linetype = "dotted", linewidth = 0.3),
  panel.grid.major.y = element_line(color = "grey90", linetype = "solid", linewidth = 0.5),
  panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
  axis.line = element_line(color = "black", linewidth = 0.6),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
)

# Using ggplot() to plot the ARIMA graph 
arima_plot <- ggplot() +
  annotate("rect", xmin = train_end_year + 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey80") +  # Testing area
  annotate("text", x = (min_year + train_end_year) / 2, y = ymax_val, label = "Training",
           vjust = -0.5, size = 5, color="#1f77b4") +
  annotate("text", x = (train_end_year + 1 + max(actual_df$Year)) / 2, y = ymax_val, label = "Testing",
           vjust = -0.5, size = 5, color="#137547") +
  # ACTUALS: color by Part + linetype by Part
  geom_line(data = actual_df, aes(Year, Value, color = Part), linewidth = 1) +
  geom_point(data = actual_df, aes(Year, Value, color = Part), shape = 17, size = 2.5) +   
  
  # MODEL: color by Series
  geom_line(data = model_df_plot, aes(Year, Value, color = Series), linewidth = 1) +
  geom_point(data = model_df_plot, aes(Year, Value, color = Series), shape = 22, size = 2.5) +   
  
  # Split marker
  geom_vline(xintercept = train_end_year + 0.5, linewidth = 0.4, alpha = 0.5, linetype = "dotted") +
  labs(title = "ARIMA: Fitted (Train) + Forecast vs Actual Yield", x = "Year", y = "Yield") +
  
  # Colors for BOTH groups (union of levels from Part and Series)
  scale_color_manual(values = c(
    "Actual"                = "#0072B2",
    "ARIMA fitted (train)"  = "#CB3E2D",
    "ARIMA forecast"        = "#6AA84F"
  )) +
  
  theme_minimal(base_size = 14) + ggplot_theme

# Saving the ARIMA plot 
filename = "ARIMA_Predictive_Model.png"
ggsave(filename = file.path(result_folder, filename), plot = arima_plot,
       width = 15, height = 10, dpi = 400)

# --------------------------- 
# (B) ARIMAX Model
# --------------------------- 

# --------------------------- 
# 1. External regressors aligned by YEAR (same rows as the ts windows)
# --------------------------- 
x_cols <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")

# Since we have already split our data into Train/Test, similar process need to be done for 
# the external regressors 
xreg_train <- as.matrix(
  explanatory_model %>% filter(Harvest_Year <= train_end_year) %>% select(all_of(x_cols))
)
xreg_test <- as.matrix(
  explanatory_model %>% filter(Harvest_Year >= test_start_year) %>% select(all_of(x_cols))
)

# --------------------------- 
# 2. Fit ARIMAX model on training dataset 
# We also evaluate the ARIMAX model
# --------------------------- 
arimax_model <- auto.arima(yield_train_ts, xreg = xreg_train, allowdrift = TRUE)

# Check ARIMAX model summary 
summary(arimax_model)

# Residual diagnostics
# Spikes could be seen in the ACF graph and p value is < 0.05, 
# indicating significant autocorrelation in residuals.
arimax_rsd_plt <- file.path(result_folder, "ARIMAX_residuals_check.png")
png(arimax_rsd_plt, width = 1400, height = 900, res = 150)
checkresiduals(arimax_model)
dev.off()

# --------------------------- 
# 3. Automated ARIMAX model selection loop
# ---------------------------

# # Create grid for searching the best order (p, q, d) for the ARIMAX model 
# p = Autoregressive lags, d = differences, q = Moving Average lags
orders <- expand.grid(p = 0:3, d = 0:2, q = 0:3)

best_arimax_model <- NULL
best_aicc <- Inf

# Brute-force search over all (p,d,q) combinations
for (i in 1:nrow(orders)) {
  order <- as.numeric(orders[i, ])
  
  try({
# Try fitting the ARIMAX model with external regressors for this order
    fit <- Arima(yield_train_ts, order = order, xreg = xreg_train)
    lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box")
    
# Keep this model only if residuals look like white noise AND it improves AICc
    if (lb_test$p.value > 0.05 && fit$aicc < best_aicc) {
      best_arimax_model <- fit
      best_aicc <- fit$aicc
    }
  }, silent = TRUE)
}

# Use the best ARIMAX model that we have found 
if (!is.null(best_arimax_model)) {
  arimax_model <- best_arimax_model
  cat("Best ARIMAX model selected:\n")
  print(arimax_model)
  checkresiduals(arimax_model)
} else {
  stop("No suitable ARIMAX model found with white noise residuals and low AICc.")
}

# --------------------------- 
# 4. Choose best model order(2,2,2) and fit ARIMAX model on training data
# --------------------------- 
arimax_model <- Arima(yield_train_ts, order = c(2,2,2), xreg = xreg_train)

# Best ARIMAX model summary 
summary(arimax_model)

# --------------------------- 
# 5. Forecast for the test period using the best ARIAMAX model 
# --------------------------- 
arimax_forecast <- forecast(arimax_model, xreg = xreg_test, h = length(yield_test_ts))

# --------------------------- 
# 6. ARIMAX model valuation 
# --------------------------- 
arimax_accuracy <- forecast::accuracy(arimax_forecast, yield_test_ts)
print(arimax_accuracy)

# ---------------------------
# 7. Forecasting for the next 3 years (2024-2026)
# ---------------------------

# If  there is no future climate data, use mean values for external regressors (approximation)
future_xreg <- matrix(
  data = c(rep(mean(explanatory_model$Avg_Temperature_Growth, na.rm = TRUE), 3),
           rep(mean(explanatory_model$Total_Rainfall_Plantation, na.rm = TRUE), 3)),
  ncol = 2
)
colnames(future_xreg) <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")

# Use the fitted ARIMAX model to forecast for the next 3 years 
future_forecast <- forecast(arimax_model, xreg = future_xreg, h = 3)

# ---------------------------
# 8. Plotting the ARIMAX model for visualization 
# ---------------------------

# Obtaining the years for training, testing & forecast 
train_years <- as.integer(time(yield_train_ts))
test_years  <- as.integer(time(yield_test_ts))
fit_years   <- as.integer(time(fitted(arimax_model)))
fc_years    <- as.integer(time(arimax_forecast$mean))

# Create a dataframe using tibble of actual yield values for the whole period
actual_df <- tibble(
  Year  = c(train_years, test_years),
  Value = c(as.numeric(yield_train_ts), as.numeric(yield_test_ts))
) %>%
  arrange(Year) %>%
  mutate(Part = "Actual")

# Build a tidy data frame of MODEL values for plotting (fitted on train + forecast on test)
model_df_plot <- tibble(
  Year   = c(fit_years, fc_years),
  Value  = c(as.numeric(fitted(arimax_model)), as.numeric(arimax_forecast$mean)),
  Series = c(rep("ARIMAX fitted (train)", length(fitted(arimax_model))),
             rep("ARIMAX forecast (test)", length(arimax_forecast$mean)))
)

# Construct a dataframe for the forecast for the next 3 years 
future_results <- tibble(
  Harvest_Year = max(explanatory_model$Harvest_Year) + 1:3,
  Forecast_Yield = as.numeric(future_forecast$mean),
  Lower_95 = as.numeric(future_forecast$lower[, 2]),
  Upper_95 = as.numeric(future_forecast$upper[, 2])
)

# Obtain the minimum year of the plot (x-axis) & highest yield values (y-axis)
# Those 2 variables will be then used for annotating the plot 
min_year <- min(actual_df$Year)
ymax_val <- max(c(actual_df$Value, model_df_plot$Value), na.rm = TRUE)

# Using ggplot() to plot the ARIMA graph 
arimax_plot <- ggplot() +
  annotate("rect", xmin = train_end_year + 0.5, xmax = max(actual_df$Year) + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey80") +  # Testing area
  annotate("rect", xmin = max(actual_df$Year) + 0.5 , xmax = Inf,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey70") +  # Forecast area
  annotate("text", x = (min_year + train_end_year) / 2, y = ymax_val, label = "Training",
           vjust = -0.5, size = 5, color="#1f77b4") +
  annotate("text", x = (train_end_year + 1 + max(actual_df$Year)) / 2, y = ymax_val, label = "Testing",
           vjust = -0.5, size = 5, color="#CB3E2D") +
  annotate("text", x = (max(future_results$Harvest_Year)) - 0.1, y = ymax_val, label = "Forecast",
           vjust = -0.5, size = 5, color="#137547") +
  
  # ACTUALS: color by Part + linetype by Part
  geom_line(data = actual_df, aes(Year, Value, color = Part), linewidth = 1) +
  geom_point(data = actual_df, aes(Year, Value, color = Part), shape = 17, size = 2.5) +   
  
  # MODEL: color by Series
  geom_line(data = model_df_plot, aes(Year, Value, color = Series), linewidth = 1) +
  geom_point(data = model_df_plot, aes(Year, Value, color = Series), shape = 22, size = 2.5) +   
  
  # Future 3 years forecast 
  geom_line(data = future_results, aes(Harvest_Year, Forecast_Yield, color = "Future Forecast"), , linewidth = 1, linetype="dashed") + 
  geom_point(data = future_results, aes(Harvest_Year, Forecast_Yield, color = "Future Forecast"), shape = 21, size = 2.5) +  

  # Split marker
  geom_vline(xintercept = train_end_year + 0.5, linewidth = 0.4, alpha = 0.5, linetype = "dotted") +
  geom_vline(xintercept = max(model_df_plot$Year) + 0.5, linewidth = 0.4, linetype = "dashed") +
  labs(title = "ARIMAX: Fitted (Train) + Forecast vs Actual Yield", x = "Year", y = "Yield") +
  
  # Colors for BOTH groups (union of levels from Part and Series)
  scale_color_manual(values = c(
    "Actual"                 = "#0072B2",
    "ARIMAX fitted (train)"  = "#CB3E2D",
    "ARIMAX forecast (test)" = "#F97306", 
    "Future Forecast"        = "#8CBF26" 
  )) +
  
  theme_minimal(base_size = 14) + ggplot_theme 

# Saving the ARIMAX plot 
filename = "ARIMAX_Predictive_Model.png"
ggsave(filename = file.path(result_folder, filename), plot = arimax_plot,
       width = 15, height = 10, dpi = 400)


# ---------------------------
# (C) Random Forest Predictive model 
# ---------------------------

# Define the features/predictors and target variable from the explanatory model 
predictors <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")          
target     <- "Yield_Value"   

# Build train/test frames aligned with the split (no duplicates)
train_df <- explanatory_model %>%
  filter(Harvest_Year <= train_end_year) %>%
  select(Harvest_Year, all_of(c(predictors, target))) %>%
  drop_na(all_of(c(predictors, target)))

test_df  <- explanatory_model %>%
  filter(Harvest_Year >= test_start_year) %>%
  select(Harvest_Year, all_of(c(predictors, target))) %>%
  drop_na(all_of(c(predictors, target)))

# ---------------------------
# 2. RF Model Training & Hyperparameter Tuning
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
on.exit({try(stopCluster(cl), silent = TRUE); registerDoSEQ()}, add = TRUE)

# Cross-validation plan
# We’ll use 10-fold CV repeated 3 times (= 30 resamples per hyperparameter combo).
k_folds <- 10 
repeats <- 3

# Build a wider, sensible hyperparameter grid 
# Random Forest key knobs (for regression with ranger via caret):
#  - mtry: how many variables to try at each split (search wide around sqrt(p))
#  - min.node.size: minimum samples in a leaf (smaller → deeper trees)
#  - splitrule: "variance" for regression (fixed here)
mtry_candidates <- unique(
  as.integer(
    pmin(p, pmax(1L, round(c(sqrt(p) * c(0.5, 1, 2), seq_len(p)))))
  )
)

# For very small p, ensure we have a simple set like {1, p}
if (p <= 2L) mtry_candidates <- unique(as.integer(c(1L, p)))

min_node_candidates <- c(1L, 2L, 3L, 5L, 7L, 10L)

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
  method = "repeatedcv",
  number = k_folds,
  repeats = repeats,
  verboseIter = FALSE,
  allowParallel = TRUE,
  seeds = make_seeds(n_resamples, nrow(tune_grid), 42),
  savePredictions = "final",
  returnResamp = "all"
)

# Training + Hyperparameter tuning of model
# caret will:
#   1) try every row in tune_grid,
#   2) run 10x3 CV for each,
#   3) pick the best model by RMSE,
#   4) refit the best model on the *full* training set.
rf_fit <- train(
  form,
  data = train_df,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = tune_grid,
  metric = "RMSE",
  importance = "permutation",
  num.trees = 1500,
  respect.unordered.factors = "order",
  seed = 42
)

# Predictions
pred_train <- predict(rf_fit, newdata = train_df)
pred_test  <- predict(rf_fit, newdata = test_df)

# Metrics (train/test)
# Using the metrics library we will be evaluating the RMSE, MAE, MSE & MAPE 
r2 <- function(obs, pred) 1 - sum((obs - pred)^2)/sum((obs - mean(obs))^2)
metrics_tbl <- function(obs, pred) data.frame(
  R2 = r2(obs, pred),
  RMSE = rmse(obs, pred),
  MAE = mae(obs, pred),
  MSE = mse(obs, pred),
  MAPE = mape(obs, pred) * 100
)
print(list(
  rf_best_params = rf_fit$bestTune,
  rf_train = metrics_tbl(train_df[[target]], pred_train),
  rf_test  = metrics_tbl(test_df[[target]],  pred_test)
))

# ---------------------------
# 3. Plotting the Random Forest model for visualization  
# ---------------------------

df_plot_rf <- explanatory_model %>%
  select(Harvest_Year, all_of(c(predictors, target))) %>%
  mutate(Predictions = as.numeric(predict(rf_fit, newdata = ., na.action = na.pass))) %>%
  rename(Actual = !!target)

# Obtain the highest value for yield which will be used for annotating text 
y_max <- max(df_plot_rf$Actual, na.rm = TRUE)

randomforest_plot <-ggplot() +
  annotate("rect", xmin = test_start_year - 0.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.10) +
  geom_vline(xintercept = train_end_year + 0.5, linetype = "dotted") +
  geom_line(data = df_plot_rf, aes(Harvest_Year, Actual, color = "Observed"), linewidth = 1) +
  geom_point(data = df_plot_rf, aes(Harvest_Year, Actual, color = "Observed"), shape = 17, size = 1.8) +
  geom_line(data = df_plot_rf, aes(Harvest_Year, Predictions, color = "Random Forest Model"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  geom_point(data = df_plot_rf, aes(Harvest_Year, Predictions, color = "Random Forest Model"),
             shape = 17, size = 1.8, na.rm = TRUE) +
  
  annotate("text", x = (min_year + train_end_year) / 2, y = ymax_val, label = "Training",
           vjust = -0.5, size = 5, color="#1f77b4") +
  annotate("text", x = (train_end_year + 1 + max(actual_df$Year)) / 2, y = ymax_val, label = "Testing",
           vjust = -0.5, size = 5, color="#137547") +
  scale_color_manual(values = c(
    "Observed"             = "#0072B2",
    "Random Forest Model"  = "#009E73")) +
  labs(title = "Random Forest: Predictions vs Observed Yield",
       subtitle = "80% Train (left) / 20% Test (right)",
       x = "Year", y = "Yield") +
  theme_minimal(base_size = 13) + ggplot_theme

# Save the Random Forest plot 
filename = "Random_Forest_Predictive_Model.png"
ggsave(filename = file.path(result_folder, filename),
       plot = randomforest_plot, width = 15, height = 10, dpi = 400)


# ---------------------------
# (D) Estimating Agricultural GDP from forecasted yield 
# ---------------------------

# ---------------------------
# 1. Calculate future Agriculture GDP share impact based on predicted values
# ---------------------------

# Obtaining the average value of the yield and Agriculture GDP share from the explanatory model 
baseline_yield <- mean(explanatory_model$Yield_Value, na.rm = TRUE)
avg_agri_share <- mean(explanatory_model$Agri_GDP_Share, na.rm = TRUE)

# Predicting the future GDP values 
future_results <- future_results %>%
  mutate(
    Yield_Change_pct = (Forecast_Yield - baseline_yield) / baseline_yield * 100,
    GDP_Impact_pct = Yield_Change_pct * (avg_agri_share / 100),
    GDP_Impact_Lower = ((Lower_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100),
    GDP_Impact_Upper = ((Upper_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100)
  )

# ---------------------------
# 2. Plotting the resulting graph for the estimated Agricultural GDP share
# ---------------------------
gdp_pred <- ggplot(future_results, aes(x = Harvest_Year, y = GDP_Impact_pct)) +
  geom_line(linewidth = 1.3, color = "#1f78b4") +
  geom_ribbon(aes(ymin = GDP_Impact_Lower, ymax = GDP_Impact_Upper),
              fill = "#1f78b4", alpha = 0.2) +
  labs(
    title = "Estimated Agricultural GDP Impact from Forecasted Yield",
    x = "Year",
    y = "GDP Impact (%)"
  ) +
  theme_minimal(base_size = 14) + ggplot_theme

# Saving the GDP forecast plot 
filename = "Agricultural_GDP_Imapct_Forecast.png"
ggsave(filename = file.path(result_folder, filename), plot = gdp_pred,
       width = 15, height = 10, dpi = 400)