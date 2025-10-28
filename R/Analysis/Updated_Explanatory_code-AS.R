###############################################################################
# EXPLANATORY PIPELINE — Climate → Sugarcane Yield (Mauritius)
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
  password = "HNBSdpl&2568pd")

#⃣ Load data from views from SQL Database

if (!dir.exists("results")) dir.create("results")

df_yield  <- dbReadTable(con, "current_compiled_data")

str(df_yield)


# List of season-climate columns expected (some may be NA if no data)
climate_variables <- c(
  "Total_Rainfall_Plantation", "Avg_Temperature_Plantation", "Avg_Humidity_Plantation", "Solar_Radiation_Plantation",
  "Total_Rainfall_Growth", "Avg_Temperature_Growth","Avg_Humidity_Growth", "Solar_Radiation_Growth",
  "Total_Rainfall_Maturation", "Avg_Temperature_Maturation", "Avg_Humidity_Maturation", "Solar_Radiation_Maturation"
)

# Convert to numeric (in case of character)
df_yield <- df_yield %>% mutate(across(all_of(climate_variables), as.numeric))

# Save merged snapshot
fwrite(df_yield, file = "results/df_yield_merged.csv")

# ---------------------------
# Exploratory correlation: Pearson, Spearman heatmaps
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
# Pairwise correlation significance tests
# We will store p-values in a table for reporting
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
# Lagged variables — keeping 1-year lags
# ---------------------------

lagged_df_yield  <- dbReadTable(con, "lagged_compiled_data")

str(lagged_df_yield)

# Save lagged snapshot
fwrite(lagged_df_yield, "results/lagged_df_yield.csv")

# Correlation heatmaps for lagged variables (Pearson & Spearman)
lagged_variables <- c(
  "Total_Rainfall_Plantation_lag1", "Avg_Temperature_Plantation_lag1","Avg_Humidity_Plantation_lag1", "Solar_Radiation_Plantation_lag1",
  "Total_Rainfall_Growth_lag1",     "Avg_Temperature_Growth_lag1","Avg_Humidity_Growth_lag1", "Solar_Radiation_Growth_lag1",
  "Total_Rainfall_Maturation_lag1", "Avg_Temperature_Maturation_lag1", "Avg_Humidity_Maturation_lag1", "Solar_Radiation_Maturation_lag1"
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
                       limit = c(-1,1), space = "Lab", name = "Lagged Pearson") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Lagged Pearson Correlation Matrix", x = NULL , y = NULL)

lagged_spearman_melt <- reshape2::melt(lagged_spearman_cor)
lagged_spearman_plot <- ggplot(lagged_spearman_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
                       limit = c(-1,1), space = "Lab" , name = "Lagged Spearman") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Lagged Spearman Correlation Matrix", x = NULL , y = NULL)

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

# ---------------------------
# Advanced dependence: Distance correlation
# ---------------------------
# Distance correlation (detects nonlinear)
adv_vars <- df_yield %>% select(all_of(climate_variables)) %>% mutate(across(everything(), as.numeric))
yield_vec <- df_yield$Yield

lagged_adv_vars <- lagged_df_yield %>% select(all_of(lagged_variables)) %>% mutate(across(everything(), as.numeric))
lagged_yield_vec <- lagged_df_yield$Yield

dcor_values <- sapply(adv_vars, function(x) dcor(yield_vec, x))

lagged_dcor_values <- sapply(lagged_adv_vars, function(x) dcor(lagged_yield_vec, x))

adv_summary <- data.frame(
  Distance = round(dcor_values, 3)
)
fwrite(adv_summary, "results/advanced_dependence_summary.csv")
print(adv_summary)

lagged_adv_summary <- data.frame(
  Distance = round(lagged_dcor_values, 3)
)

fwrite(lagged_adv_summary, "results/lagged_advanced_dependence_summary.csv")
print(lagged_adv_summary)

# ---------------------------
# Multicollinearity: VIF using seasonal predictors
# ---------------------------
# Build a linear model with main seasonal predictors to compute VIF
present_predictors <- climate_variables[climate_variables %in% names(df_yield)]
lm_for_vif_formula <- as.formula(paste("Yield ~", paste(present_predictors, collapse = " + ")))
vif_model <- lm(lm_for_vif_formula, data = df_yield)
vif_values <- tryCatch(vif(vif_model), error = function(e) NA)
vif_df <- data.frame(Variable = names(vif_values), VIF = round(as.numeric(vif_values), 2))
fwrite(vif_df, "results/vif_values.csv")
print(vif_df)

lagged_predictors <- climate_variables[climate_variables %in% names(lagged_df_yield)]
lm_for_lagged_vif_formula <- as.formula(paste("Yield ~", paste(lagged_predictors, collapse = " + ")))
lagged_vif_model <- lm(lm_for_lagged_vif_formula, data = lagged_df_yield)
lagged_vif_values <- tryCatch(vif(lagged_vif_model), error = function(e) NA)
lagged_vif_df <- data.frame(Variable = names(lagged_vif_values), VIF = round(as.numeric(lagged_vif_values), 2))
fwrite(lagged_vif_df, "results/lagged_vif_values.csv")
print(lagged_vif_df)

# Flag problematic multicollinearity (VIF > 5)
if (any(!is.na(vif_values) & vif_values > 5)) {
  cat("\n Warning: high VIF detected for variables:\n")
  print(names(vif_values[vif_values > 5]))
} else {
  cat("\nNo strong multicollinearity detected (VIF <= 5 for available predictors)\n")
}

# Flag problematic multicollinearity (VIF > 5)
if (any(!is.na(lagged_vif_values) & lagged_vif_values > 5)) {
  cat("\n⚠️ Warning: high VIF detected for variables:\n")
  print(names(lagged_vif_values[lagged_vif_values > 5]))
} else {
  cat("\nNo strong multicollinearity detected (VIF <= 5 for available predictors)\n")
}

# ---------------------------
# DATA MODEL
# ---------------------------
#we build the data model taking into consideration the multicollinearity

model_df <- lagged_df_yield %>%
  select(Climate_Year,Harvest_Year,Yield,
         Total_Rainfall_Plantation, Avg_Temperature_Plantation,
         Total_Rainfall_Growth, Avg_Temperature_Growth,
         Total_Rainfall_Maturation, Avg_Temperature_Maturation,
         Total_Rainfall_Plantation_lag1, Avg_Temperature_Plantation_lag1,
         Total_Rainfall_Growth_lag1, Avg_Temperature_Growth_lag1,
         Total_Rainfall_Maturation_lag1, Avg_Temperature_Maturation_lag1,
         Agri_GDP_Share
  ) %>%
  rename(Yield_Value=Yield)%>%
  drop_na()


fwrite(model_df, "results/GAM_model.csv")
print(model_df)

# ---------------------------
# Decide variables for different models : detect nonlinearity
# Use threshold on distance correlation
# ---------------------------
nonlinear_threshold <- 0.5
nonlinear_vars <- names(dcor_values[dcor_values > nonlinear_threshold & abs(pearson_cor["Yield", names(dcor_values)]) < 0.3])

cat("\nDetected nonlinear variables (distance corr >", nonlinear_threshold, "and low linear corr):\n")
print(nonlinear_vars)


# ---------------------------
# LINEAR REGRESSION MODEL
# ---------------------------
#check linear model

lm_model_1 <- lm(
  Yield_Value ~
    Total_Rainfall_Plantation + Avg_Temperature_Plantation+
  Total_Rainfall_Growth+ Avg_Temperature_Growth+
  Total_Rainfall_Maturation+ Avg_Temperature_Maturation+
  Total_Rainfall_Plantation_lag1+ Avg_Temperature_Plantation_lag1+
  Total_Rainfall_Growth_lag1+ Avg_Temperature_Growth_lag1+
  Total_Rainfall_Maturation_lag1+ Avg_Temperature_Maturation_lag1,
  data = model_df
)
summary(lm_model_1)

summary(lm_model_1)$adj.r.squared

#REFINED OLS REGRESSION MODEL
lm_model_2 <- lm(
  Yield_Value ~
    Avg_Temperature_Growth+
    Total_Rainfall_Growth,
  data = model_df
)
summary(lm_model_2)

summary(lm_model_2)$adj.r.squared

# ---------------------------
# Explanatory GAM (current + 1-year lag) : smooth terms for each relevant variable, excluding Solar_Radiation_Plantation & Solar_Radiation_Maturation
# ---------------------------
# Build GAM formula: smooth on each seasonal temp + rainfall + solar radiation and on lags
gam_formula_parts_1 <- c(
  "s(Total_Rainfall_Plantation)", "s(Avg_Temperature_Plantation)",
  "s(Total_Rainfall_Growth)", "s(Avg_Temperature_Growth)",
  "s(Total_Rainfall_Maturation)", "s(Avg_Temperature_Maturation)",
  "s(Total_Rainfall_Plantation_lag1)", "s(Avg_Temperature_Plantation_lag1)",
  "s(Total_Rainfall_Growth_lag1)", "s(Avg_Temperature_Growth_lag1)",
  "s(Total_Rainfall_Maturation_lag1)", "s(Avg_Temperature_Maturation_lag1)")
gam_formula_1 <- as.formula(paste("Yield_Value ~", paste(gam_formula_parts_1, collapse = " + ")))

# Fit GAM with REML (stable smoothing)
gam_model_1 <- gam(gam_formula_1, data = model_df, method = "REML")

# Summary (edf indicates nonlinearity when > 1)
gam_summary_1 <- summary(gam_model_1)
print(gam_summary_1)
gam_summary_1$r.sq

# REPEAT: Build GAM formula with only significant variables
gam_formula_parts_2 <- c(
  "s(Total_Rainfall_Plantation)", "s(Avg_Temperature_Growth)")

gam_formula_2 <- as.formula(paste("Yield_Value ~", paste(gam_formula_parts_2, collapse = " + ")))

gam_model_2 <- gam(gam_formula_2, data = model_df, method = "REML")

gam_summary_2 <- summary(gam_model_2)
print(gam_summary_2)
gam_summary_2$r.sq


# FINAL GAM MODEL
gam_formula_parts <- c(
  "s(Total_Rainfall_Plantation)", "s(Avg_Temperature_Plantation)",
  "s(Total_Rainfall_Growth)", "s(Avg_Temperature_Growth)",
  "s(Total_Rainfall_Maturation)", "s(Avg_Temperature_Maturation)",
  "s(Total_Rainfall_Plantation_lag1)", "s(Avg_Temperature_Plantation_lag1)",
  "s(Total_Rainfall_Growth_lag1)", "s(Avg_Temperature_Growth_lag1)",
  "s(Total_Rainfall_Maturation_lag1)", "s(Avg_Temperature_Maturation_lag1)")

gam_formula <- as.formula(paste("Yield_Value ~", paste(gam_formula_parts, collapse = " + ")))

gam_model <- gam(gam_formula, data = model_df, method = "REML")

gam_summary <- summary(gam_model)
print(gam_summary)
gam_summary$r.sq

saveRDS(gam_model, "results/gam_model.rds")

# ---------------------------
# GAM diagnostics & plotting (explanatory)
# ---------------------------
# Plot partial effects (base R plots)
png("results/gam_partial_effects.png", width = 1600, height = 1200)
par(mfrow = c(4,3), mar = c(4,4,2,1))
plot(gam_model, shade = TRUE, seWithMean = TRUE, pages = 1)
dev.off()

# Nicer ggplots using gratia
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


# ---------------------------
# Descriptive economic context (explanatory only)
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

yield_gdp_plot <- ggplot(model_df, aes(x = Yield_Change_pct, y = GDP_Impact_pct)) +
  geom_point(
    color = "#1A9850",  # Green points
    size = 3,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D73027",  # Red trend line
    linewidth = 1
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Relationship Between Sugarcane Yield and Agricultural GDP Share (1981–2023)",
    subtitle = "Mauritius — Source: FAOSTAT & World Bank",
    x = "Sugarcane Yield (hg/ha)",
    y = "% change of GDP",
    caption = "Note: Linear trend with 95% confidence interval"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray25"),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "gray40", face = "italic")
  )

print(yield_gdp_plot)

ggsave("results/yield_vs_gdp.png", yield_gdp_plot, width = 8, height = 6, dpi = 300)

# ---------------------------
# 16. Save tidy coefficient table (for explanatory report)
# ---------------------------
gam_coefs <- broom::tidy(gam_model)
fwrite(gam_coefs, "results/gam_coefs.csv")
# Also save partial effect edf summary
edf_table <- as.data.frame(cbind(term = rownames(gam_summary$s.table), gam_summary$s.table))
fwrite(edf_table, "results/gam_edf_table.csv")

# ---------------------------
# 17. Final messages & recommendations
# ---------------------------
cat("\nAnalysis complete. Results and plots saved in ./results/\n")
cat("Key files:\n - results/df_yield_merged.csv\n - results/pearson_correlation_heatmap.png\n - results/spearman_correlation_heatmap.png\n - results/lagged_pearson_heatmap.png\n - results/gam_summary.txt\n - results/gam_partial_effects.png\n - results/economic_summary.csv\n\n")

# End of script
###############################################################################
