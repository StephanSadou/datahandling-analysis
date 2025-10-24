# ===== Baseline GLM (OLS) for Sugarcane Production =====

library(gt)
library(DBI)
library(dplyr)
library(readr)
library(gridExtra)
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
yield_climate_df <- dbReadTable(con, "current_compiled_data")

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
                "Total_Rainfall_Growth", "Total_Rainfall_Maturation")

# Select only the column that interest us in our predictive model 
# We select the year column as well --> Will be used later for plotting 
yield_climate_df <- yield_climate_df %>% select(c("Harvest_Year", predictors, target))

# -------------------------------------------------------- #
# -------- Step 3: Data Splitting (Train/Test) ----------- # 
# -------------------------------------------------------- #

# Performing a chronological split of the data
# 80% will be used for training & 20% will be used for testing 
n_total   <- nrow(yield_climate_df)
split_idx <- floor(0.8 * n_total)
stopifnot(split_idx >= 1, split_idx < n_total)

# We do the splitting for (A) Compiled data & (B) Compiled Lagged data 
train <- yield_climate_df[seq_len(split_idx), , drop = FALSE]
test  <- yield_climate_df[(split_idx + 1):n_total, , drop = FALSE]

split_year <- train$Harvest_Year[split_idx]


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


# Drop rows with NA in the columns needed for each model (separately for train/test)

train <- train %>% select(c(predictors, target)) %>%
  drop_na(all_of(c(predictors, target)))
test  <- test  %>% select(c(predictors, target)) %>%
  drop_na(all_of(c(predictors, target)))

# -------------------------------------------------------- #
# -------------- Step 4: Models Training ----------------- # 
# -------------------------------------------------------- #

# MODEL A — Compiled data only 
form <- reformulate(termlabels = predictors, response = target)
fit  <- lm(form, data = train)

# ----------------------------------------------------------------------- #
# -------------- Step 5: Prediction & Models Evaluation ----------------- # 
# ----------------------------------------------------------------------- #

# Using the fitted GLM model to predict on training data & then on testing data 
glm_model_train_values <- train[[target]]
glm_model_test_values <- test[[target]]
glm_model_train_predictions <- predict(fit, newdata = train)
glm_model_testing_predictions <- predict(fit, newdata = test)

cat("\n=== GLM Model — OLS summary ===\n")
print(summary(fit))

rbind(
  cbind(Set = "Train", metrics_tbl(glm_model_train_values, glm_model_train_predictions)),
  cbind(Set = "Test",  metrics_tbl(glm_model_test_values,  glm_model_testing_predictions))
)

# ---------- 6) Full-series predictions (for plotting) ----------
# For plotting we predict over the whole df. For each model we must drop rows with missing predictors.

# First we take all the data from the original yield climate dataset 
model_df <- yield_climate_df %>% drop_na(all_of(c(predictors, target)))


# Joining the GLM Model predicted values together with the original dataset for plotting 
df_plot <- yield_climate_df %>% select(Harvest_Year, Yield) %>% 
  rename(Actual = Yield) %>%
  left_join(
    model_df %>% mutate(Predictions = as.numeric(predict(fit, newdata = model_df))) %>% select(Harvest_Year, Predictions),
            by = "Harvest_Year")

y_max <- max(df_plot$Actual, na.rm = TRUE)

ggplot() +
  annotate("rect", xmin = split_year, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_vline(xintercept = split_year, linetype="dotted") +
  geom_line(data = df_plot, aes(Harvest_Year, Actual, color = "Observed"), linewidth = 1) +
  geom_line(data = df_plot, aes(Harvest_Year, Predictions, color = "GLM Model"),
            linewidth = 1, linetype = "dashed", na.rm = TRUE) +
  annotate("text", x = split_year - 2, y = y_max, label = "TRAIN", vjust = -0.3, size = 3.5, color="#0b7509") +
  annotate("text", x = split_year + 2, y = y_max, label = "TEST",  vjust = -0.3, size = 3.5, color="blue") +
  scale_color_manual(values = c("Observed" = "black",
                                "GLM Model" = "#d95f02")) + 
  labs(
    title = "Sugarcane Production — GLM Predictions vs Observed",
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



