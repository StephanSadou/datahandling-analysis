# Below are the packages required to run all our scripts for the assignment 
required_packages <- c("graph", "Rgraphiz", "zoo", "DBI", "cowplot", "patchwork",
                       "lubridate", "caret", "tidyverse", "broom", "ggrepel", "scales", 
                       "e1071", "outliers", "psych", "doParallel","gt","forecast","ranger", "Metrics",
                       "readr", "stringr", "RMariaDB", "httr", "arrow", "dplyr", "tidyr",
                       "tibble", "scales", "ggplot2","FAOSTAT", "jsonlite", "data.table",
                       "rstudioapi", "rprojroot", "gridExtra", "lmtest", "sandwich")

# Checking what are the R packages installed on the system 
installed_packages <- .packages(all.available = TRUE) 

# Create an empty vector to track packages which are not installed 
missing_packages <- vector(mode="character")

# Start iterating the to check if the required packages are available 
# If not available, we will try to install them 
for (pkgs in required_packages) {
  if(pkgs %in% installed_packages) {
    next 
  } else{
    # Append missing package to the vector
    missing_packages <- append(missing_packages, pkgs)  
    # install.packages(pkgs, dependencies = TRUE)
  }
}

# Create vectors to note down the package name if there is an installation error 
packages_error = vector(mode="character")

# Check if there are any missing packages, if true, we attempt to install them 
# Will put the operation in a tryCatch function to handle errors & warnings 
if (length(missing_packages) != 0) {
  for (mpkgs in missing_packages) {
    result <- tryCatch(
      {
        install.packages(mpkgs, dependencies = TRUE)
      },
      error = function(e) {
        cat(sprintf("[-] An error occurred while trying to install the %s package: %s\n",
                    mpkgs, e$message))
        packages_error <<- append(packages_error, mpkgs)  # <<- assigns to outer variable
      },
      warning = function(w) {
        cat(sprintf("Caught warning while installing %s: %s\n",
                    mpkgs, w$message))
        invokeRestart("muffleWarning")
      }
    )
  }
}
if (length(packages_error) != 0) {
  cat(sprintf("[-] Encountered errors while trying to install the following packages: %s \n", packages_error))
  cat("[-] Try to install them manually.")
} 
