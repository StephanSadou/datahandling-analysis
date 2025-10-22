library(dplyr)
library(forecast)
library(ggplot2)
library(readr)
library(tibble)

# --- 1) Load & order
model_df <- read_csv(
  "https://raw.githubusercontent.com/StephanSadou/datahandling-analysis/main/R/Analysis/GAM_model.csv"
) %>% arrange(Harvest_Year)

# --- 2) Build a single annual ts
yield_ts <- ts(model_df$Yield_Value,
               start = min(model_df$Harvest_Year),
               frequency = 1)

# --- 3) Split by index -> derive split year, then window()
n <- nrow(model_df)
split_idx <- floor(0.8 * n)
train_end_year  <- model_df$Harvest_Year[split_idx]
test_start_year <- train_end_year + 1

yield_train_ts <- window(yield_ts, end   = train_end_year)
yield_test_ts  <- window(yield_ts, start = test_start_year)

# --- 4) Fit + forecast
fit <- auto.arima(yield_train_ts, seasonal = FALSE)
fc  <- forecast(fit, h = length(yield_test_ts))

# --- 5) Build plotting data
train_years <- as.integer(time(yield_train_ts))
test_years  <- as.integer(time(yield_test_ts))
fc_years    <- as.integer(time(fc$mean))

actual_df <- tibble(
  Year  = c(train_years,        test_years),
  Value = c(as.numeric(yield_train_ts), as.numeric(yield_test_ts)),
  Part  = c(rep("Actual (train)", length(yield_train_ts)),
            rep("Actual (test)",  length(yield_test_ts)))
)

model_df_plot <- tibble(
  Year   = c(as.integer(time(fitted(fit))), fc_years),
  Value  = c(as.numeric(fitted(fit)),       as.numeric(fc$mean)),
  Series = c(rep("ARIMA fitted (train)", length(fitted(fit))),
             rep("ARIMA forecast",       length(fc$mean)))
)

# --- 6) Plot (no intervals), continuous look at the split
ggplot() +
  geom_line(data = actual_df, aes(Year, Value, linetype = Part)) +
  geom_line(data = model_df_plot, aes(Year, Value, color = Series)) +
  geom_vline(xintercept = train_end_year + 0.5, linewidth = 0.4, alpha = 0.5) +
  labs(title = "ARIMA: Fitted (Train) + Forecast vs Actual",
       x = "Year", y = "Yield") +
  scale_linetype_manual(values = c("Actual (train)" = "solid",
                                   "Actual (test)"  = "dashed")) +
  guides(linetype = guide_legend(title = NULL),
         color    = guide_legend(title = NULL)) +
  theme_minimal()


#################################################################################



# 1) Build the canonical annual time series once
yield_ts <- ts(model_df$Yield_Value,
               start = min(model_df$Harvest_Year),
               frequency = 1)

# 2) Split year by 80/20 index, then use window() to keep alignment
n <- nrow(model_df)
split_idx       <- floor(0.8 * n)
train_end_year  <- model_df$Harvest_Year[split_idx]
test_start_year <- train_end_year + 1

yield_train_ts <- window(yield_ts, end   = train_end_year)
yield_test_ts  <- window(yield_ts, start = test_start_year)

# 3) External regressors aligned by YEAR (same rows as the ts windows)
x_cols <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")

xreg_train <- as.matrix(
  model_df %>% filter(Harvest_Year <= train_end_year) %>% select(all_of(x_cols))
)
xreg_test <- as.matrix(
  model_df %>% filter(Harvest_Year >= test_start_year) %>% select(all_of(x_cols))
)

# (Optional) If scales differ a lot, consider standardizing:
# xreg_train <- scale(xreg_train); xreg_test <- scale(xreg_test,
#               center = attr(xreg_train, "scaled:center"),
#               scale  = attr(xreg_train, "scaled:scale"))

# 4) Fit ARIMAX on TRAIN ONLY
fit_x <- auto.arima(yield_train_ts,
                    xreg = xreg_train,
                    seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(fit_x)

# Residual diagnostics
checkresiduals(fit_x)

# 5) Forecast TEST horizon using TEST regressors (no PI in plot later)
h <- length(yield_test_ts)
fc_x <- forecast(fit_x, xreg = xreg_test, h = h)

# 6) Build plotting data (Actuals + Fitted + Forecast), with clear split
train_years <- as.integer(time(yield_train_ts))
test_years  <- as.integer(time(yield_test_ts))
fit_years   <- as.integer(time(fitted(fit_x)))
fc_years    <- as.integer(time(fc_x$mean))

actual_df <- tibble(
  Year  = c(train_years,            test_years),
  Value = c(as.numeric(yield_train_ts), as.numeric(yield_test_ts)),
  Part  = c(rep("Actual (train)", length(yield_train_ts)),
            rep("Actual (test)",  length(yield_test_ts)))
)

model_df_plot <- tibble(
  Year   = c(fit_years,             fc_years),
  Value  = c(as.numeric(fitted(fit_x)), as.numeric(fc_x$mean)),
  Series = c(rep("ARIMAX fitted (train)", length(fitted(fit_x))),
             rep("ARIMAX forecast (test)", length(fc_x$mean)))
)

# 7) Plot: actuals (train solid, test dashed) + fitted + forecast (no bands)
ggplot() +
  geom_line(data = actual_df, aes(Year, Value, linetype = Part)) +
  geom_line(data = model_df_plot, aes(Year, Value, color = Series)) +
  geom_vline(xintercept = train_end_year + 0.5, linewidth = 0.4, alpha = 0.5) +
  labs(title = "ARIMAX: Fitted (Train) + Forecast (Test) vs Actual",
       x = "Year", y = "Yield") +
  scale_linetype_manual(values = c("Actual (train)" = "solid",
                                   "Actual (test)"  = "dashed")) +
  guides(linetype = guide_legend(title = NULL),
         color    = guide_legend(title = NULL)) +
  theme_minimal()

# 8) Quantitative accuracy on the TEST period
accuracy(fc_x, as.numeric(yield_test_ts))







