#Automatically push data_stage V2 into database called data_handling

library("DBI")
library("readr")
library("RMariaDB")
library("rprojroot")

# ---------------------------
# 1. Create connection to MySQL server using the credentials in the environment file
# ---------------------------

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  dbname = Sys.getenv("DB_NAME"),
  password = Sys.getenv("DB_PASSWORD")
)

# ---------------------------
# 2. Load data from data stage folder 
# ---------------------------

# Obtain the directory path where the script is being executed 
source("get_cwd.R")
cwd <- get_script_dir()

# Define the folder path of the data_stage folder 
data_stage <- file.path(cwd, "..", "data_stage")

agriculture_df <- read_csv(file.path(data_stage, "FAOSTAT_stageV2.csv"))
climate_df <- read_csv(file.path(data_stage,"nasa_stage_V2.csv"))
economic_df <- read_csv(file.path(data_stage,"wb_stage_V2.csv"))

# ---------------------------
# 3. Push data onto the SQL tables 
# ---------------------------

# Push data to tables created from step 1 using the SQL script 
dbWriteTable(con, "agriculture", agriculture_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "climate", climate_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "economic_indicator", economic_df, append = TRUE, row.names = FALSE)

# Task Completed
cat("✅ All data successfully pushed into MySQL database!\n")

# Close connection
dbDisconnect(con)