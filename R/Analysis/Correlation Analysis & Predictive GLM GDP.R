#install.packages("corrplot")
#install.packages("gt")
library(corrplot)
library(DBI)
library(RMariaDB)
library(dplyr)
library(gt)


con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "Data_Handling",
  host = "127.0.0.1",
  user = "root",
  password = "abc123"
)

compiled_data <- dbReadTable(con, "current_compiled_data")
#Correlation between agricultural data & nasa data

str(compiled_data)

num_df <- compiled_data %>% select(
    Avg_Temperature_Plantation,
    Avg_Temperature_Growth,
    Avg_Temperature_Maturation,
    Avg_Humidity_Plantation,
    Avg_Humidity_Growth,
    Avg_Humidity_Maturation,
    Solar_Radiation_Plantation,
    Solar_Radiation_Growth,
    Solar_Radiation_Maturation,
    Total_Rainfall_Plantation,
    Total_Rainfall_Growth,
    Total_Rainfall_Maturation,
    Total_rainy_days_Plantation,
    Total_rainy_days_Growth,
    Total_rainy_days_Maturation,
    Area_harvested,Production,
    Yield
  )
cor_matrix <- cor(num_df, method = "pearson", use = "complete.obs")



cor_matrix <- cor(num_df, use = "complete.obs", method = "pearson")
cor_df <- round(as.data.frame(cor_matrix), 2)
cor_df <- tibble::rownames_to_column(cor_df, "Variable")

gt(cor_df) %>%
  tab_header(
    title = "🌾 Pearson Correlation Matrix",
    subtitle = "Colour intensity shows strength of correlation"
  ) %>%
  data_color(
    columns = -Variable,
    colors = scales::col_numeric(
      palette = c("red", "white", "blue"),
      domain = c(-1, 1)
    )
  )

#Correlation between agricultural data & GDP Data


num_df2 <- compiled_data %>% select(
  Area_harvested,
  Production,
  Yield,
  GDP_Value
)
cor_matrix <- cor(num_df, method = "pearson", use = "complete.obs")



cor_matrix <- cor(num_df2, use = "complete.obs", method = "pearson")
cor_df <- round(as.data.frame(cor_matrix), 2)
cor_df <- tibble::rownames_to_column(cor_df, "Variable")

gt(cor_df) %>%
  tab_header(
    title = "🌾 Pearson Correlation Matrix",
    subtitle = "Colour intensity shows strength of correlation"
  ) %>%
  data_color(
    columns = -Variable,
    colors = scales::col_numeric(
      palette = c("red", "white", "blue"),
      domain = c(-1, 1)
    )
  )


#PREDICTION: MODEL A: GLM:- Climate on Production


library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(Metrics)


# Sort data by Year (important for time series!)
df <- compiled_data[order(compiled_data$Harvest_Year), ]

# ------------------------------------
# ✂️ 3. Chronological 70/30 Split
# ------------------------------------
n_total <- nrow(df)
split_idx <- floor(0.7 * n_total)
train <- df[1:split_idx, ]
test  <- df[(split_idx + 1):n_total, ]
split_year <- train$Year[split_idx]

# ------------------------------------
# 🤖 4. Train GLM Model: GDP_Value ~ Yield
# ------------------------------------
model <- glm((GDP_Value) ~ (Yield), data = train)

cat("\n=== MODEL SUMMARY ===\n")
print(summary(model))

# ------------------------------------
# 📉 5. Predictions
# ------------------------------------
train$Predicted <- predict(model_log, newdata = train, type = "response")
test$Predicted  <- predict(model_log, newdata = test, type = "response")

# ------------------------------------
# 📏 6. Performance Metrics
# ------------------------------------
rmse <- function(a, p) sqrt(mean((a - p)^2))
mae  <- function(a, p) mean(abs(a - p))
r2   <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

# Training
cat("\n📊 TRAIN METRICS\n")
cat(sprintf("R²: %.3f | RMSE: %.3f | MAE: %.3f\n",
            r2(train$GDP_Value, train$Predicted),
            rmse(train$GDP_Value, train$Predicted),
            mae(train$GDP_Value, train$Predicted)))

# Testing
cat("\n📊 TEST METRICS\n")
cat(sprintf("R²: %.3f | RMSE: %.3f | MAE: %.3f\n",
            r2(test$GDP_Value, test$Predicted),
            rmse(test$GDP_Value, test$Predicted),
            mae(test$GDP_Value, test$Predicted)))

# ------------------------------------
# 📈 7. Plot Actual vs Predicted (Train/Test)
# ------------------------------------
df_plot <- bind_rows(
  train %>% mutate(Set = "Train"),
  test %>% mutate(Set = "Test")
)

ggplot(df_plot, aes(x = Harvest_Year, group = 1)) +
  # Actual line
  geom_line(aes(y = GDP_Value, color = Set), size = 1.2) +
  # Predicted line (black dashed)
  geom_line(aes(y = Predicted), size = 1.2, linetype = "dashed", color = "black") +
  geom_vline(xintercept = split_year, linetype = "dotted") +
  annotate("text", x = split_year - 0.5, y = max(df_plot$GDP_Value), 
           label = "Train", vjust = -0.5, size = 4) +
  annotate("text", x = split_year + 0.5, y = max(df_plot$GDP_Value), 
           label = "Test", vjust = -0.5, size = 4) +
  scale_color_manual(values = c("Train" = "#1f78b4", "Test" = "#e31a1c")) +
  labs(
    title = "Total GDP vs Sugarcane Yield",
    subtitle = "Solid line = Actual | Dashed line = Predicted",
    y = "GDP Value",
    x = "Year",
    color = "Data Split"
  ) +
  theme_minimal(base_size = 14)
