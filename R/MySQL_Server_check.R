# --------------------------
# MySQL Server status check 
# This function is used to verify if a MySQL server instance is currently running.
# This ensures that MySQL scripts can be safely executed. 
# Simple MySQL/MariaDB reachability check using DBI + RMariaDB and tryCatch.
# Returns TRUE if a connection succeeds, FALSE otherwise.
# - You don't need an existing database name; "mysql" is the default system DB.
# - Keep timeout small so the preflight check is fast.
# --------------------------

library(DBI)
library(RMariaDB)
library(rprojroot)

# Read the environment file to obtain the database credentials 
root <- find_root(has_file(".Renviron"))
readRenviron(file.path(root, ".Renviron"))

mysql_status <- function(
    # Use the credentials in the R environment file to connect to server 
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    host     = Sys.getenv("DB_HOST"),
    port     = 3306,
    dbname   = "mysql",
    timeout  = 3) {
  
  # Starts with null connection 
  # Will hold the DB connection if it succeeds
  con <- NULL  
  
  # Ensure we disconnect the connection on any exit path (success or error)
  on.exit({
    if (!is.null(con)) {
      # Ignore any disconnect errors
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  }, add = TRUE)
  
  # Attempt to connect; on error, return NULL (no thrown error)
  con <- tryCatch(
    DBI::dbConnect(
      RMariaDB::MariaDB(),
      user = user,
      password = password,
      host = host,
      port = port,
      dbname = dbname,
      timeout = timeout  
    ),
    error = function(e) {
      # You could log e$message here if you want to see why it failed
      NULL
    }
  )
  
  # If con is not NULL, connection succeeded -> server is up/reachable
  !is.null(con)
}