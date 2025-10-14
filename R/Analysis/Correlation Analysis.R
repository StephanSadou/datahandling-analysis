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

compiled_data <- dbReadTable(con, "compiled_data")
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
    Area_harvested,
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
