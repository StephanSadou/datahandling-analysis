library("DBI")
library("readr")
library("dplyr")
library("stringr")
library("RMariaDB")
library("rprojroot")

# ---------------------------
# 1. Define the SQL folder paths & scripts  
# ---------------------------

# Obtain the directory path where the script is being executed 
source("get_cwd.R")
cwd <- get_script_dir() 

# Navigate into the SQL_Schema folder and get the filepaths in it 
sqlfolder <- file.path(cwd, "..", "SQL_Schema")
step1_sql <- file.path(sqlfolder, "1.0_CREATING DB & TABLES.sql")
step2_r   <- file.path(sqlfolder, "2.0_Automatic Table Import.R")
step3_sql <- file.path(sqlfolder, "3.0_JOINING FILES & CREATE VIEW.sql")

# ---------------------------
# 2. Create the functions to execute the SQL scripts 
# ---------------------------

# Function to remove whitespaces and newlines in string using REGEX
normalize_newlines <- function(x) {
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("\n{2,}", " ", x, perl = TRUE)
  x <- gsub("(?<=\\S)\\n(?=\\S)", " ", x, perl = TRUE)
  x <- gsub("(?<=\\h)\\n(?=\\h)", "", x, perl = TRUE)
  x <- gsub("(?<=\\S)\\n(?=\\h)", " ", x, perl = TRUE)
  x <- gsub("(?<=\\h)\\n(?=\\S)", " ", x, perl = TRUE)
  x
}

# Function to execute SQL scripts on the MySQL server 
execute_sql <- function(dbconnection, sql_script) {
  # Read the SQL script and remove newlines and tab spaces 
  # Split sql statements between ";" and then store 
  # each statement as a row in a dataframe 
  sql_query <- paste(readLines(sql_script), collapse='\n') %>%
    str_replace_all(c("\t"="")) %>% normalize_newlines() %>%
    str_split(";") %>% as.data.frame(col.names = "Statements")
  
  # Further refinement -> split the strings again and remove white spaces 
  for (i in 1:nrow(sql_query)) {
    strings_split <- unlist(strsplit(sql_query$Statements[i], " "))
    new_statement <- vector()
    for (j in strings_split) {
      if (j != "") {
        new_statement <- append(new_statement, j)
      }
    }
    sql_query$Statements[i] = paste(new_statement, collapse = " ")
  }
  
  # Remove any rows where ANY column is NA, NULL, or empty string
  sql_query <- sql_query %>% 
    # turn "" into NA
    mutate(across(everything(), ~ na_if(trimws(.), ""))) %>%  
    # then drop any rows which are NA
    filter(if_all(everything(), ~ !is.na(.)))                 
  
  # Iterate through each statement and execute it on the MySQL server 
  for (sql in sql_query$Statements) {
    dbExecute(conn = dbconnection, statement = sql)
  }
}

# ---------------------------
# 3. Data Loading: CREATE SQL tables & views and load data into them 
# --------------------------

# Create connection to MySQL server using the credentials in the environment file
# No need to specify the database here - already mentioned in the SQL scripts 
conn <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Step 1: SQL tables creations 
execute_sql(conn, step1_sql)

# Step 2: Load data from CSVs into the tables 
source(step2_r)

# Step 3: Create the VIEWS using the SQL tables 
execute_sql(conn, step3_sql)

# Close database connection once done 
dbDisconnect(conn)
