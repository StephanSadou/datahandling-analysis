#Automatically push data_stage V2 into database called data_handling

#Install relevant packages

install.packages(c("DBI","RMariaDB","readr"))


library(DBI)
library(RMariaDB)
library(readr)

#  Connect to DB - User to include host and password information
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "Data_Handling",
  host = "127.0.0.1",
  user = "root",
  password = "abc123"
)

#⃣ Load data from datastage folder on GitHub

agriculture_df <- read_csv("https://raw.githubusercontent.com/StephanSadou/datahandling-analysis/main/data_stage/FAOSTAT_stageV2.csv")
climate_df <- read_csv("https://raw.githubusercontent.com/StephanSadou/datahandling-analysis/main/data_stage/nasa_stage_V2.csv")
economic_df <- read_csv("https://raw.githubusercontent.com/StephanSadou/datahandling-analysis/main/data_stage/wb_stage_V2.csv")


# Push data to tables created from step_1: SQL SCRIPT
dbWriteTable(con, "agriculture", agriculture_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "climate", climate_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "economic_indicator", economic_df, append = TRUE, row.names = FALSE)

# Task Completed
cat("✅ All data successfully pushed into MySQL database!\n")

# Close connection
dbDisconnect(con)