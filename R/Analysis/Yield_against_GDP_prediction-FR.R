library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(data.table)
library(energy)    # distance correlation
library(Hmisc)     # hoeffd
library(car)       # VIF
library(mgcv)      # GAM
library(gratia)    # nicer GAM plots (optional)
library(broom)     # tidy outputs
library(lmtest)
library(DBI)
library(RMariaDB)
library(readr)
library(forecast)

#Sourcing explanatory script to refer to the model data frame


source("https://raw.githubusercontent.com/StephanSadou/datahandling-analysis/5ad5cb94312cbc830f25a09b215d50239170fd37/R/Analysis/Updated_Explanatory_code.R")

#1 Create time-series dataset based on real yield values
summary(model_df)
yield_ts <- ts(model_df$Yield_Value, 
               start = min(model_df$Harvest_Year), 
               frequency = 1)  # annual data

#2 Proportion for Training and Testing Sets:80/20

split_year <- floor(0.8 * nrow(model_df))   # 80% split index
train_data <- model_df[1:split_year, ]
test_data  <- model_df[(split_year + 1):nrow(model_df), ]

#3 Building time series on training dataset

yield_train_ts <- ts(train_data$Yield_Value, 
                     start = min(train_data$Harvest_Year), 
                     frequency = 1)

#4 Fitting ARIMA on the training dataset above
arima_model <- auto.arima(yield_train_ts)
summary(arima_model)

# Residual diagnostics (should look like white noise)
checkresiduals(arima_model)

# Then print Ljung-Box details explicitly
ljung_result <- Box.test(residuals(arima_model), lag = 7, type = "Ljung-Box")
cat("\nLjung-Box Test:\n")
print(ljung_result)

#5  Forecast for the Test Period: remaining 20% of full data set

h <- nrow(test_data)  # no. of years in test data set
yield_forecast <- forecast(arima_model, h = h)


test_data$Forecast_Yield <- as.numeric(yield_forecast$mean)  #Additing  forecasted values to test set



#6 Evaluate Model Accuracy (Validation)

accuracy_results <- accuracy(yield_forecast, test_data$Yield_Value)
print(accuracy_results)

#7.  Re-train ARIMAX on full dataset
full_arima_model <- auto.arima(yield_ts)
summary(full_arima_model)


# ===============================================================
#Model 2: ARIMAX
#ARIMAX MODEL is analysed as option for providing an improved timeforecasting model

#1 sort dataset in ascending order

model_df <- model_df %>% arrange(Harvest_Year)


# 2. Train-Test Split (80/20 by Year)

split_year <- floor(0.8 * nrow(model_df))   # 80% split index
train_data <- model_df[1:split_year, ]
test_data  <- model_df[(split_year + 1):nrow(model_df), ]

#3. Create time series based on above

yield_train_ts <- ts(train_data$Yield_Value, start = min(train_data$Harvest_Year), frequency = 1)

#4 External regressors (climate variables) for train and test
xreg_train <- as.matrix(train_data[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])
xreg_test  <- as.matrix(test_data[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])


#5 Fit ARIMAX model on training data

arimax_model <- auto.arima(yield_train_ts, xreg = xreg_train, allowdrift = TRUE)
summary(arimax_model)

# Residual diagnostics (should look like white noise)
checkresiduals(arimax_model)

#Spikes could be seen in the ACF graph and p value is < 0.05, indicating significant autocorrelation in redisuals.

#6 Automated ARIMAX model selection loop
orders <- expand.grid(p = 0:3, d = 0:2, q = 0:3)

best_arimax_model <- NULL
best_aicc <- Inf

for (i in 1:nrow(orders)) {
  order <- as.numeric(orders[i, ])
  
  try({
    fit <- Arima(yield_train_ts, order = order, xreg = xreg_train)
    lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box")
    
    if (lb_test$p.value > 0.05 && fit$aicc < best_aicc) {
      best_arimax_model <- fit
      best_aicc <- fit$aicc
    }
  }, silent = TRUE)
}

# Use best model if found
if (!is.null(best_arimax_model)) {
  arimax_model <- best_arimax_model
  cat("✅ Best ARIMAX model selected:\n")
  print(arimax_model)
  checkresiduals(arimax_model)
} else {
  stop("❌ No suitable ARIMAX model found with white noise residuals and low AICc.")
}

#Choose order(2,2,2) and test on residuals 
arimax_model <- Arima(yield_train_ts, order = c(2,2,2), xreg = xreg_train)
summary(arimax_model)


# 7 Forecast for the test period 

h <- nrow(test_data)
yield_forecast <- forecast(arimax_model, xreg = xreg_test, h = h)

# Add forecast results to test set
test_data$Forecast_Yield <- as.numeric(yield_forecast$mean)

# 8 Evaluate model accuracy on test data

accuracy_results <- accuracy(yield_forecast, test_data$Yield_Value)
print(accuracy_results)

#9.  Re-train ARIMAX on full dataset

yield_full_ts <- ts(model_df$Yield_Value, start = min(model_df$Harvest_Year), frequency = 1)
xreg_full <- as.matrix(model_df[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])

final_arimax_model <- Arima(yield_full_ts, order = c(2,2,2), xreg = xreg_full)
summary(final_arimax_model)


#10  Create future climate scenario for forecasting (e.g., next 7 years)

# If no future climate data, use mean values (approximation)
future_xreg <- matrix(
  data = c(rep(mean(model_df$Avg_Temperature_Growth, na.rm = TRUE), 7),
           rep(mean(model_df$Total_Rainfall_Plantation, na.rm = TRUE), 7)),
  ncol = 2
)
colnames(future_xreg) <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")


# 10 Forecasting yield for next 7 years

future_forecast <- forecast(final_arimax_model, xreg = future_xreg, h = 7)
autoplot(future_forecast) + ggtitle("ARIMAX Forecast of Sugarcane Yield")


#11 Calculate future GDP impact based on predicted values

baseline_yield <- mean(model_df$Yield_Value, na.rm = TRUE)
avg_agri_share <- mean(model_df$Agri_GDP_Share, na.rm = TRUE)

future_results <- data.frame(
  Year = max(model_df$Harvest_Year) + 1:7,
  Forecast_Yield = as.numeric(future_forecast$mean),
  Lower_95 = as.numeric(future_forecast$lower[,2]),
  Upper_95 = as.numeric(future_forecast$upper[,2])
) %>%
  mutate(
    Yield_Change_pct = (Forecast_Yield - baseline_yield) / baseline_yield * 100,
    GDP_Impact_pct = Yield_Change_pct * (avg_agri_share / 100),
    GDP_Impact_Lower = ((Lower_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100),
    GDP_Impact_Upper = ((Upper_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100)
  )

print(future_results)

# ===============================================================
# ✅ Final Results Interpretation
# ===============================================================
# - accuracy_results: Shows test-set MAPE, RMSE, bias → check model performance.
# - checkresiduals(): Should show white-noise residuals (p > 0.05).
# - future_results: Shows forecasted yield, % change, and estimated GDP imp
