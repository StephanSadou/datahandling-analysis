###############################################################################
# EXPLANATORY PIPELINE — Climate → Sugarcane Yield (Mauritius)
# Merges user's code with advanced diagnostics and GAM for explanation only
# NOTE: This is an explanatory analysis (no forecasting).
# Adapt paths as needed.
###############################################################################

# ---------------------------
# 0. Libraries & setup
# ---------------------------
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

#Connect to DB - User to include host and password information
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "Data_Handling",
  host = "127.0.0.1",
  user = "root",
  password = "abc123")

#⃣ Load data from views from SQL Database

if (!dir.exists("results")) dir.create("results")

df_yield  <- dbReadTable(con, "current_compiled_data")

str(df_yield)


# List of season-climate columns expected (some may be NA if no data)
climate_variables <- c(
  "Total_Rainfall_Plantation", "Avg_Temperature_Plantation",
  "Total_Rainfall_Growth", "Avg_Temperature_Growth",
  "Total_Rainfall_Maturation", "Avg_Temperature_Maturation"
)

# Convert to numeric (in case of character)
df_yield <- df_yield %>% mutate(across(all_of(climate_variables), as.numeric))

# Save merged snapshot
fwrite(df_yield, file = "results/df_yield_merged.csv")





# ---------------------------
# 7. Exploratory correlation: Pearson, Spearman heatmaps
# ---------------------------
# Prepare correlation dataset: Yield + climate season vars
cor_data <- df_yield %>% select(Yield, all_of(climate_variables)) %>% mutate(across(everything(), as.numeric))

# Pearson
pearson_cor <- cor(cor_data, use = "complete.obs", method = "pearson")
pearson_cor_melt <- reshape2::melt(pearson_cor)

pearson_plot <- ggplot(pearson_cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name = "Pearson") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Pearson Correlation Matrix (Yield & Seasonal Climate)")

ggsave("results/pearson_correlation_heatmap.png", pearson_plot, width = 9, height = 6, dpi = 300)

# Spearman
spearman_cor <- cor(cor_data, use = "complete.obs", method = "spearman")
spearman_cor_melt <- reshape2::melt(spearman_cor)
spearman_plot <- ggplot(spearman_cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name = "Spearman") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Spearman Correlation Matrix (Yield & Seasonal Climate)")

ggsave("results/spearman_correlation_heatmap.png", spearman_plot, width = 9, height = 6, dpi = 300)

# Sid-by-side display for easier comparison
pearson_plot + spearman_plot

# Save correlation matrices for inspection
fwrite(as.data.frame(pearson_cor), "results/pearson_cor_matrix.csv")
fwrite(as.data.frame(spearman_cor), "results/spearman_cor_matrix.csv")

# ---------------------------
# 8. Pairwise correlation significance tests
#    We will store p-values in a table for reporting rather than printing many cor.test lines
# ---------------------------
corr_tests <- function(x, y, method = "pearson") {
  ok <- complete.cases(x, y)
  if (sum(ok) < 4) return(list(stat = NA, p.value = NA))
  t <- cor.test(x[ok], y[ok], method = method)
  list(stat = t$estimate, p.value = t$p.value)
}

# Build table of tests for each season var vs Yield
vars <- climate_variables
test_res <- data.frame(
  Variable = vars,
  Pearson_r = NA, Pearson_p = NA,
  Spearman_r = NA, Spearman_p = NA
)
for (i in seq_along(vars)) {
  v <- vars[i]
  pt <- corr_tests(df_yield$Yield, df_yield[[v]], method = "pearson")
  st <- corr_tests(df_yield$Yield, df_yield[[v]], method = "spearman")
  test_res$Pearson_r[i] <- round(pt$stat, 3)
  test_res$Pearson_p[i] <- round(pt$p.value, 3)
  test_res$Spearman_r[i] <- round(st$stat, 3)
  test_res$Spearman_p[i] <- round(st$p.value, 3)
}
fwrite(test_res, "results/correlation_tests_summary.csv")

# ---------------------------
# 9. Lagged variables — keeping 1-year lags
# ---------------------------

lagged_df_yield  <- dbReadTable(con, "lagged_compiled_data")


lagged_df_yield <- df_yield %>%
  inner_join(lagged_df_yield, by = "Harvest_Year") %>%
  select(
    -Climate_Year.x,
    -GDP_Value.x,
    -Area_harvested.x,     
    -Production.x,
    -Yield.x
  )%>%rename(
    Climate_Year=Climate_Year.y,
    Yield=Yield.y ,
    Agri_GDP_Share=GDP_Value.y,
    Area_harvested=Area_harvested.y,
    Production=Production.y
  )
print(lagged_df_yield)
# Save lagged snapshot
fwrite(lagged_df_yield, "results/lagged_df_yield.csv")

# Correlation heatmaps for lagged variables (Pearson & Spearman)
lagged_variables <- c(
  "Total_Rainfall_Plantation_lag1", "Avg_Temperature_Plantation_lag1",
  "Total_Rainfall_Growth_lag1",     "Avg_Temperature_Growth_lag1",
  "Total_Rainfall_Maturation_lag1", "Avg_Temperature_Maturation_lag1"
)

lagged_cor_data <- lagged_df_yield %>%
  select(Yield, all_of(lagged_variables)) %>%
  mutate(across(everything(), as.numeric))

lagged_pearson_cor <- cor(lagged_cor_data, use = "complete.obs", method = "pearson")
lagged_spearman_cor <- cor(lagged_cor_data, use = "complete.obs", method = "spearman")

lagged_pearson_melt <- reshape2::melt(lagged_pearson_cor)
lagged_pearson_plot <- ggplot(lagged_pearson_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
                       limit = c(-1,1), space = "Lab") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Lagged Pearson Correlation Matrix")

lagged_spearman_melt <- reshape2::melt(lagged_spearman_cor)
lagged_spearman_plot <- ggplot(lagged_spearman_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
                       limit = c(-1,1), space = "Lab") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Lagged Spearman Correlation Matrix")

ggsave("results/lagged_pearson_heatmap.png", lagged_pearson_plot, width = 9, height = 6, dpi = 300)
ggsave("results/lagged_spearman_heatmap.png", lagged_spearman_plot, width = 9, height = 6, dpi = 300)

# Sid-by-side display for easier comparison
lagged_pearson_plot + lagged_spearman_plot

fwrite(as.data.frame(lagged_pearson_cor), "results/lagged_pearson_cor_matrix.csv")
fwrite(as.data.frame(lagged_spearman_cor), "results/lagged_spearman_cor_matrix.csv")


# Side-by-side comparison of pearson correlation and lagged pearson correlation
pearson_plot + lagged_pearson_plot

# Side-by-side comparison of pearson correlation and lagged pearson correlation
spearman_plot + lagged_spearman_plot

#The correlation heatmaps show that sugarcane yield is more positively influenced by temperature than by rainfall across all stages. 
#Moderate warmth during the growth and plantation phases tends to increase yield, while excessive rainfall, especially during plantation or maturation, slightly reduces it. 
#Relationships are mostly non-linear and stronger under Spearman than Pearson, suggesting that yield responds to climate within optimal thresholds rather than in a strictly linear way. 
#Lagged correlations indicate only weak carry-over effects from the previous season, confirming that current-season climate—particularly temperature during growth—plays the dominant role in determining sugarcane productivity.

# ---------------------------
# 10. Advanced dependence: Kendall, Distance correlation, Hoeffding
# ---------------------------
# Use the seasonal variables and yield (flatten to numeric)
adv_vars <- df_yield %>% select(all_of(climate_variables)) %>% mutate(across(everything(), as.numeric))
yield_vec <- df_yield$Yield

lagged_adv_vars <- lagged_df_yield %>% select(all_of(lagged_variables)) %>% mutate(across(everything(), as.numeric))
lagged_yield_vec <- lagged_df_yield$Yield

# Kendall correlations (yield vs each climate var)
kendall_vals <- sapply(adv_vars, function(x) cor(yield_vec, x, method = "kendall", use = "complete.obs"))

lagged_kendall_vals <- sapply(lagged_adv_vars, function(x) cor(lagged_yield_vec, x, method = "kendall", use = "complete.obs"))

# Distance correlation (detects nonlinear)
dcor_values <- sapply(adv_vars, function(x) dcor(yield_vec, x))

lagged_dcor_values <- sapply(lagged_adv_vars, function(x) dcor(lagged_yield_vec, x))

# Hoeffding's D (Hmisc::hoeffd returns matrix of D and p-values)
# Ensure adv_vars is numeric and a data frame
adv_vars_num <- adv_vars %>%
  mutate(across(everything(), as.numeric))

lagged_adv_vars_num <- lagged_adv_vars %>%
  mutate(across(everything(), as.numeric))

# Combine Yield and explanatory variables into one numeric matrix
hoeffd_input <- cbind(Yield = as.numeric(df_yield$Yield), adv_vars_num) %>%
  as.matrix()

lagged_hoeffd_input <- cbind(Yield = as.numeric(lagged_df_yield$Yield), lagged_adv_vars_num) %>%
  as.matrix()

# Run Hoeffding's D test
hoeffd_res <- hoeffd(hoeffd_input)

lagged_hoeffd_res <- hoeffd(lagged_hoeffd_input)

# hoeffd_res$D has one extra row/col (Yield included), we'll extract row 1 without the first element
hoeffd_D_with_yield <- hoeffd_res$D[1, -1]

lagged_hoeffd_D_with_yield <- lagged_hoeffd_res$D[1, -1]


adv_summary <- data.frame(
  Variable = names(adv_vars),
  Kendall = round(kendall_vals, 3),
  Distance = round(dcor_values, 3),
  HoeffdingD = round(as.numeric(hoeffd_D_with_yield), 3)
)
fwrite(adv_summary, "results/advanced_dependence_summary.csv")
print(adv_summary)

#Sugarcane yield in your dataset tends to rise modestly with higher temperatures, while rainfall shows little to slightly negative effect.
#The dependence is weak but not random, and non-linear patterns exist 
#— meaning yield likely peaks within optimal climate ranges rather than continuously increasing.


lagged_adv_summary <- data.frame(
  Variable = names(lagged_adv_vars),
  Kendall = round(lagged_kendall_vals, 3),
  Distance = round(lagged_dcor_values, 3),
  HoeffdingD = round(as.numeric(lagged_hoeffd_D_with_yield), 3)
)
fwrite(lagged_adv_summary, "results/lagged_advanced_dependence_summary.csv")
print(lagged_adv_summary)

#The lagged (previous-year) climate conditions have weak but non-negligible influence on sugarcane yield.
#Temperature patterns tend to have a slight positive residual effect, while rainfall tends to have neutral to slightly negative lagged effects.
#This implies that sugarcane yield is mostly driven by the current season’s climate, with limited memory from the past season 
#— except possibly for temperature-driven soil or ratoon continuity effects.

# ---------------------------
# 11. Multicollinearity: VIF using seasonal predictors
# ---------------------------
# Build a linear model with main seasonal predictors to compute VIF
present_predictors <- climate_variables[climate_variables %in% names(df_yield)]
lm_for_vif_formula <- as.formula(paste("Yield ~", paste(present_predictors, collapse = " + ")))
vif_model <- lm(lm_for_vif_formula, data = df_yield)
vif_values <- tryCatch(vif(vif_model), error = function(e) NA)
vif_df <- data.frame(Variable = names(vif_values), VIF = round(as.numeric(vif_values), 2))
fwrite(vif_df, "results/vif_values.csv")
print(vif_df)

# Flag problematic multicollinearity (VIF > 5)
if (any(!is.na(vif_values) & vif_values > 5)) {
  cat("\n⚠️ Warning: high VIF detected for variables:\n")
  print(names(vif_values[vif_values > 5]))
} else {
  cat("\nNo strong multicollinearity detected (VIF <= 5 for available predictors)\n")
}

# ---------------------------
# 12. Decide variables for GAM (Generalized Additive Model) : detect nonlinearity
#    Use threshold on distance correlation (data-driven)
# ---------------------------
# choose threshold: dCor > 0.5 (recommendation), but if sample is small, lower threshold e.g., 0.4
nonlinear_threshold <- 0.5
nonlinear_vars <- names(dcor_values[dcor_values > nonlinear_threshold & abs(pearson_cor["Yield", names(dcor_values)]) < 0.3])

cat("\nDetected nonlinear variables (distance corr >", nonlinear_threshold, "and low linear corr):\n")
print(nonlinear_vars)
#For explanatory GAM we include smooth terms for all climate variables (recommended),

# but we will pay special attention to variables flagged as nonlinear (interpret edf).
# Also include the 1-year lag terms (from lagged_df_yield) — merge them back.
model_df <- lagged_df_yield %>%
  select(Climate_Year,Harvest_Year,Yield,
         Total_Rainfall_Plantation, Avg_Temperature_Plantation,
         Total_Rainfall_Growth, Avg_Temperature_Growth,
         Total_Rainfall_Maturation, Avg_Temperature_Maturation,
         Total_Rainfall_Plantation_lag1,Avg_Temperature_Growth_lag1,
         Total_Rainfall_Maturation_lag1,Avg_Temperature_Maturation_lag1,
         Total_Rainfall_Growth_lag1,Avg_Temperature_Plantation_lag1,
         Agri_GDP_Share
         ) %>%
  rename(Yield_Value=Yield)%>%
  drop_na()



# ---------------------------
# 13. Explanatory GAM (current + 1-year lag) : smooth terms for each relevant variable
# ---------------------------
# Build GAM formula: smooth on each seasonal temp + rainfall and on lags
gam_formula_parts <- c(
  "s(Total_Rainfall_Plantation)",     "s(Avg_Temperature_Plantation)",
  "s(Total_Rainfall_Growth)",         "s(Avg_Temperature_Growth)",
  "s(Total_Rainfall_Maturation)",     "s(Avg_Temperature_Maturation)",
"s(Total_Rainfall_Plantation_lag1)","s(Avg_Temperature_Plantation_lag1)",
 "s(Total_Rainfall_Growth_lag1)",    "s(Avg_Temperature_Growth_lag1)",
 "s(Total_Rainfall_Maturation_lag1)","s(Avg_Temperature_Maturation_lag1)")
gam_formula <- as.formula(paste("Yield_Value ~", paste(gam_formula_parts, collapse = " + ")))

# Fit GAM with REML (stable smoothing)
gam_model <- gam(gam_formula, data = model_df, method = "REML")
saveRDS(gam_model, "results/gam_model.rds")

# Summary (edf indicates nonlinearity when > 1)
gam_summary <- summary(gam_model)
print(gam_summary)


#The GAM results suggest that sugarcane yield is mainly influenced by temperature during the growth phase and rainfall during plantation and maturation.
#The effects are mostly non-linear, meaning yield improves up to an optimal level of temperature/rainfall and then declines.
#The influence of previous season’s (lagged) weather is minimal.
#Overall, current-season conditions dominate yield outcomes, confirming your correlation analysis.

# ---------------------------
# 14. GAM diagnostics & plotting (explanatory)
# ---------------------------
# Plot partial effects (base R plots)
png("results/gam_partial_effects.png", width = 1600, height = 1200)
par(mfrow = c(4,3), mar = c(4,4,2,1))
plot(gam_model, shade = TRUE, seWithMean = TRUE, pages = 1)
dev.off()

# Nicer ggplots using gratia (if installed)
png("results/gam_draws.png", width = 1600, height = 1200)
draw(gam_model, residuals = FALSE)
dev.off()

# Residual diagnostics
model_df$gam_fitted <- predict(gam_model)
model_df$gam_resid <- residuals(gam_model)

png("results/gam_residuals_acf.png", width = 900, height = 600)
par(mfrow = c(2,1))
plot(model_df$Harvest_Year, model_df$gam_resid, type = "o", main = "GAM residuals over time", ylab = "Residual")
acf(model_df$gam_resid, main = "ACF of GAM residuals")
dev.off()

# Shapiro (normality) and Breusch-Pagan (heteroskedasticity) — interpret with caution for small samples
shapiro_res <- shapiro.test(model_df$gam_resid)
bp_res <- bptest(lm(gam_resid ~ gam_fitted, data = model_df))
capture.output(shapiro_res, file = "results/gam_shapiro.txt")
capture.output(bp_res, file = "results/gam_bp_test.txt")

# ---------------------------
# 15. Descriptive economic context (explanatory only)
# ---------------------------
model_df <- model_df %>%
  mutate(
    Predicted_Yield = gam_fitted,
    Yield_Change_pct = (Predicted_Yield - mean(Yield_Value, na.rm = TRUE)) / mean(Yield_Value, na.rm = TRUE) * 100,
    GDP_Impact_pct = Yield_Change_pct * (Agri_GDP_Share / 100)  # approximate, descriptive
  )

econ_summary <- model_df %>%
  summarise(
    Avg_Yield = mean(Yield_Value, na.rm = TRUE),
    Avg_Predicted_Yield = mean(Predicted_Yield, na.rm = TRUE),
    Avg_Yield_Change_pct = mean(Yield_Change_pct, na.rm = TRUE),
    Avg_GDP_Impact_pct = mean(GDP_Impact_pct, na.rm = TRUE)
  )
fwrite(econ_summary, "results/economic_summary.csv")
print(econ_summary)

# ---------------------------
# 16. Save tidy coefficient table (for explanatory report)
# ---------------------------
gam_coefs <- broom::tidy(gam_model)
fwrite(gam_coefs, "results/gam_coefs.csv")
# Also save partial effect edf summary
edf_table <- as.data.frame(cbind(term = rownames(gam_summary$s.table), gam_summary$s.table))
fwrite(edf_table, "results/gam_edf_table.csv")

#The economic summary shows that the model-predicted sugarcane yield aligns almost perfectly with observed data, 
#confirming a very accurate model fit. 
#On average, yield remains around 71,480 units, and the year-to-year change is negligible. 
#The estimated GDP impact of these yield variations is minimal (≈0.013%), 
#suggesting that while sugarcane is economically relevant, its short-term yield fluctuations have a limited macroeconomic effect on national GDP.

# ---------------------------
# 17. Final messages & recommendations
# ---------------------------
cat("\nAnalysis complete. Results and plots saved in ./results/\n")
cat("Key files:\n - results/df_yield_merged.csv\n - results/pearson_correlation_heatmap.png\n - results/spearman_correlation_heatmap.png\n - results/lagged_pearson_heatmap.png\n - results/gam_summary.txt\n - results/gam_partial_effects.png\n - results/economic_summary.csv\n\n")

# End of script
###############################################################################

#PREDICTIVE PART: YIELD VS AGRI_SHARE


#1 USE GAM TO BUILD A CLEAN YIELD TIME SERIES

model_df$Predicted_Yield <- predict(gam_model)   #Provides climate-adjusted yield estimate

# CREATE TIME SERIES FROM GAM-FITTED YIELD

yield_ts <- ts(model_df$Predicted_Yield, 
               start = min(model_df$Harvest_Year), 
               frequency = 1)  # annual data

#2 Split the Data into Training and Testing Sets:80/20

split_year <- 2015  #to update with whole dataset

train_data <- model_df %>% filter(Harvest_Year <= split_year)
test_data  <- model_df %>% filter(Harvest_Year > split_year)

#3 Build time series on training data

library(forecast)

yield_train_ts <- ts(train_data$Yield_Value, 
                     start = min(train_data$Harvest_Year), 
                     frequency = 1)

#4 Fit ARIMA on the training data
arima_model <- auto.arima(yield_train_ts)
summary(arima_model)

#5  Forecast for the Test Period

h <- nrow(test_data)  # number of years in test set
yield_forecast <- forecast(arima_model, h = h)


test_data$Forecast_Yield <- as.numeric(yield_forecast$mean)


#6 Evaluate Model Accuracy (Validation)

accuracy_results <- accuracy(yield_forecast, test_data$Yield_Value)
print(accuracy_results)

#7 Forecast Future Yield (2025-2027)

yield_full_ts <- ts(model_df$Yield_Value, 
                    start = min(model_df$Harvest_Year), 
                    frequency = 1)

final_arima_model <- auto.arima(yield_full_ts)
future_forecast <- forecast(final_arima_model, h = 7)  # next 7 years
autoplot(future_forecast)

#8 Estimate GDP Impact for the Forecast Period

baseline_yield <- mean(model_df$Yield_Value, na.rm = TRUE)
avg_agri_share <- mean(model_df$Agri_GDP_Share, na.rm = TRUE)

future_results <- data.frame(
  Year = max(model_df$Harvest_Year) + 1:7,
  Forecast_Yield = as.numeric(future_forecast$mean)
) %>%
  mutate(
    Yield_Change_pct = (Forecast_Yield - baseline_yield) / baseline_yield * 100,
    GDP_Impact_pct = Yield_Change_pct * (avg_agri_share / 100)
  )

#ARIMAX MODEL

#Load libraries
# ===============================================================
library(forecast)
library(dplyr)
library(ggplot2)

# ===============================================================
# Prepare data (replace model_df with your actual dataset)
# ===============================================================
# model_df must contain at least:
# Year, Yield_Value, Agri_GDP_Share, Avg_Temperature_Growth, Total_Rainfall_Plantation

# Ensure sorted by Year
model_df <- model_df %>% arrange(Harvest_Year)

# ===============================================================
# 3️⃣ Train-Test Split (80/20 by Year)
# ===============================================================
split_year <- floor(0.8 * nrow(model_df))   # 80% split index
train_data <- model_df[1:split_year, ]
test_data  <- model_df[(split_year + 1):nrow(model_df), ]

# Dependent variable (yield) - time series
yield_train_ts <- ts(train_data$Yield_Value, start = min(train_data$Harvest_Year), frequency = 1)

# External regressors (climate variables) for train and test
xreg_train <- as.matrix(train_data[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])
xreg_test  <- as.matrix(test_data[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])

# ===============================================================
# 4️⃣ Fit ARIMAX model on training data
# ===============================================================
arimax_model <- auto.arima(yield_train_ts, xreg = xreg_train, allowdrift = TRUE)
summary(arimax_model)

# ===============================================================
# 5️⃣ Forecast for the test period (validation)
# ===============================================================
h <- nrow(test_data)  # forecast horizon
yield_forecast <- forecast(arimax_model, xreg = xreg_test, h = h)

# Add forecast results to test set
test_data$Forecast_Yield <- as.numeric(yield_forecast$mean)

# ===============================================================
# 6️⃣ Evaluate model accuracy on test data
# ===============================================================
accuracy_results <- accuracy(yield_forecast, test_data$Yield_Value)
print(accuracy_results)

# Residual diagnostics (should look like white noise)
checkresiduals(arimax_model)

# ===============================================================
# 7️⃣ Re-train ARIMAX on full dataset
# ===============================================================
yield_full_ts <- ts(model_df$Yield_Value, start = min(model_df$Harvest_Year), frequency = 1)
xreg_full <- as.matrix(model_df[, c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")])

final_arimax_model <- auto.arima(yield_full_ts, xreg = xreg_full, allowdrift = TRUE)
summary(final_arimax_model)

# ===============================================================
# 8️⃣ Create future climate scenario for forecasting (e.g., next 7 years)
# ===============================================================
# If no future climate data, use mean values (approximation)
future_xreg <- matrix(
  data = c(rep(mean(model_df$Avg_Temperature_Growth, na.rm = TRUE), 7),
           rep(mean(model_df$Total_Rainfall_Plantation, na.rm = TRUE), 7)),
  ncol = 2
)
colnames(future_xreg) <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation")

# ===============================================================
# 9️⃣ Forecast future yield (e.g., 2025–2031)
# ===============================================================
future_forecast <- forecast(final_arimax_model, xreg = future_xreg, h = 7)
autoplot(future_forecast) + ggtitle("ARIMAX Forecast of Sugarcane Yield")

# ===============================================================
# 🔟 Calculate future GDP impact
# ===============================================================
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


#TO BE CONTINUED
#3 ETS MODEL

# ===============================================================
# 1️⃣ Load Libraries
# ===============================================================
library(forecast)
library(dplyr)
library(ggplot2)

# ===============================================================
# 2️⃣ Prepare Data
# ===============================================================
# Ensure your data frame is named model_df and has:
# - Year
# - Yield_Value (annual sugarcane yield)
# - Agri_GDP_Share (agricultural GDP share)

# Sort by Year
model_df <- model_df %>% arrange(Harvest_Year)

# ===============================================================
# 3️⃣ Create Time Series
# ===============================================================
yield_ts <- ts(model_df$Yield_Value, start = min(model_df$Harvest_Year), frequency = 1)

# ===============================================================
# 4️⃣ Train-Test Split (80/20)
# ===============================================================
split_index <- floor(0.8 * length(yield_ts))
yield_train_ts <- window(yield_ts, end = time(yield_ts)[split_index])
yield_test_ts  <- window(yield_ts, start = time(yield_ts)[split_index + 1])

# ===============================================================
# 5️⃣ Fit ETS Model on Training Data
# ===============================================================
ets_model <- ets(yield_train_ts)
summary(ets_model)

# ===============================================================
# 6️⃣ Forecast on Test Period
# ===============================================================
h <- length(yield_test_ts) # forecast horizon (test period length)
ets_forecast_test <- forecast(ets_model, h = h)

# Accuracy on test data
accuracy_results <- accuracy(ets_forecast_test, yield_test_ts)
print(accuracy_results)

# Plot actual vs forecast for test period
autoplot(ets_forecast_test) +
  autolayer(yield_test_ts, series = "Actual") +
  ggtitle("ETS Forecast vs Actual (Test Set)") +
  ylab("Yield") + xlab("Year")

# ===============================================================
# 7️⃣ Residual Diagnostics
# ===============================================================
checkresiduals(ets_model)

# ===============================================================
# 8️⃣ Re-fit ETS on Full Data (for Final Forecast)
# ===============================================================
final_ets_model <- ets(yield_ts)
summary(final_ets_model)

# ===============================================================
# 9️⃣ Forecast Future Yields (e.g., next 7 years)
# ===============================================================
future_forecast <- forecast(final_ets_model, h = 7)
autoplot(future_forecast) +
  ggtitle("ETS Forecast of Sugarcane Yield (2024–2030)") +
  ylab("Forecasted Yield")

# View forecasted values
print(future_forecast)

# ===============================================================
# 🔟 Calculate GDP Impact Based on Forecast
# ===============================================================
baseline_yield <- mean(model_df$Yield_Value, na.rm = TRUE)
avg_agri_share <- mean(model_df$Agri_GDP_Share, na.rm = TRUE)

future_results <- data.frame(
  Year = max(model_df$Year) + 1:7,
  Forecast_Yield = as.numeric(future_forecast$mean),
  Lower_95 = as.numeric(future_forecast$lower[, 2]),
  Upper_95 = as.numeric(future_forecast$upper[, 2])
) %>%
  mutate(
    Yield_Change_pct = (Forecast_Yield - baseline_yield) / baseline_yield * 100,
    GDP_Impact_pct = Yield_Change_pct * (avg_agri_share / 100),
    GDP_Impact_Lower = ((Lower_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100),
    GDP_Impact_Upper = ((Upper_95 - baseline_yield) / baseline_yield * 100) * (avg_agri_share / 100)
  )

print(future_results)



#4 Random forest
install.packages("caret")
library(randomForest)
library(dplyr)
library(ggplot2)
library(caret)

# ===============================================================
# 2️⃣ Prepare dataset
# ===============================================================
# Ensure model_df contains:
# - Year
# - Yield_Value
# - Climate variables: Avg_Temperature_Growth, Total_Rainfall_Plantation, etc.

model_df <- model_df %>% arrange(Year)
acf(model_df$Yield_Value, main = "ACF of Sugarcane Yield")
# (Optional) Create lagged yield to include temporal structure
model_df <- model_df %>%
  mutate(
    Yield_lag2 = lag(Yield_Value, 2),
  ) %>%
  drop_na()

# ===============================================================
# 3️⃣ Train-Test Split (80/20)
# ===============================================================
set.seed(123)
split_index <- floor(0.8 * nrow(model_df))
train_data <- model_df[1:split_index, ]
test_data  <- model_df[(split_index + 1):nrow(model_df), ]

# Define predictor variables (climate + lagged yield)
predictors <- c("Avg_Temperature_Growth", "Total_Rainfall_Plantation",
                "Total_Rainfall_Maturation", "Avg_Temperature_Maturation",
                "Yield_lag2")

# ===============================================================
# 4️⃣ Train Random Forest Model
# ===============================================================
rf_model <- randomForest(
  Yield_Value ~ ., 
  data = train_data[, c("Yield_Value", predictors)],
  ntree = 500,
  mtry = 3,
  importance = TRUE
)
print(rf_model)

# ===============================================================
# 5️⃣ Predict on Test Set
# ===============================================================
test_data$RF_Prediction <- predict(rf_model, newdata = test_data[, predictors])

# Model accuracy
postResample(pred = test_data$RF_Prediction, obs = test_data$Yield_Value)

# Plot actual vs predicted
ggplot(test_data, aes(x = Year)) +
  geom_line(aes(y = Yield_Value, color = "Actual")) +
  geom_line(aes(y = RF_Prediction, color = "Random Forest Predicted")) +
  labs(title = "Random Forest Prediction vs Actual Yield", y = "Yield") +
  scale_color_manual(values = c("Actual" = "black", "Random Forest Predicted" = "blue"))

# ===============================================================
# 6️⃣ Feature Importance
# ===============================================================
importance(rf_model)
varImpPlot(rf_model)

# ===============================================================
# 7️⃣ Train on Full Data for Future Forecast
# ===============================================================
rf_full <- randomForest(
  Yield_Value ~ ., 
  data = model_df[, c("Yield_Value", predictors)],
  ntree = 500,
  mtry = 3
)

# Create future scenario (e.g., mean climate values or projections)
future_years <- data.frame(
  Avg_Temperature_Growth = rep(mean(model_df$Avg_Temperature_Growth), 5),
  Total_Rainfall_Plantation = rep(mean(model_df$Total_Rainfall_Plantation), 5),
  Total_Rainfall_Maturation = rep(mean(model_df$Total_Rainfall_Maturation), 5),
  Avg_Temperature_Maturation = rep(mean(model_df$Avg_Temperature_Maturation), 5),
  Yield_lag2 = tail(model_df$Yield_Value, 1),
  Yield_lag3 = tail(model_df$Yield_Value, 2)[1]
)

future_predictions <- predict(rf_full, newdata = future_years)

future_results <- data.frame(
  Year = max(model_df$Year) + 1:5,
  Forecast_Yield = future_predictions
)
print(future_results)

# ===============================================================
# 8️⃣ GDP Impact Estimation
# ===============================================================
baseline_yield <- mean(model_df$Yield_Value, na.rm = TRUE)
avg_agri_share <- mean(model_df$Agri_GDP_Share, na.rm = TRUE)

future_results <- future_results %>%
  mutate(
    Yield_Change_pct = (Forecast_Yield - baseline_yield) / baseline_yield * 100,
    GDP_Impact_pct = Yield_Change_pct * (avg_agri_share / 100)
  )

print(future_results)

