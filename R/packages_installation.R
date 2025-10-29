# Below are the packages required to run all our scripts for the assignment 

required_packages <- c(
  "gt",         # Create formatted tables
  "car",        # Companion to Applied Regression (diagnostics, utilities)
  "zoo",        # Ordered time-series objects
  "DBI",        # Standard database interface for R
  "httr",       # HTTP requests for APIs
  "mgcv",       # Generalized Additive Models
  "caret",      # ML training & resampling wrappers
  "broom",      # Tidy model outputs (data frames)
  "arrow",      # Arrow/Parquet columnar I/O
  "e1071",      # ML algorithms (e.g., SVM, Naive Bayes)
  "graph",      # Graph data structures
  "dplyr",      # Data manipulation grammar
  "tidyr",      # Tidy reshaping (pivot/unnest)
  "readr",      # Fast, friendly CSV/TSV import
  "tibble",     # Modern data frames
  "ranger",     # Fast Random Forests
  "scales",     # Axis/label scaling helpers
  "energy",     # Energy statistics (e.g., distance correlation)
  "gratia",     # GAM diagnostics & plots
  "lmtest",     # Tests for linear models
  "Metrics",    # Model evaluation metrics (RMSE/MAE/…)
  "FAOSTAT",    # FAO datasets access
  "stringr",    # Consistent string handling
  "cowplot",    # Publication-ready ggplot helpers
  "ggrepel",    # Non-overlapping text labels for ggplot2
  "ggplot2",    # Grammar of graphics plotting
  "forecast",   # Time-series forecasting (ARIMA/ETS/…)
  "RMariaDB",   # MySQL/MariaDB driver via DBI
  "reshape2",   # Reshape long<->wide
  "jsonlite",   # JSON parsing & writing
  "sandwich",   # Robust covariance (sandwich) estimators
  "Rgraphiz",   # Graphviz bindings for graph viz
  "outliers",   # Outlier detection utilities
  "patchwork",  # Compose multiple ggplots
  "rprojroot",  # Project-root discovery
  "gridExtra",  # Arrange multiple grid-based plots
  "tidyverse",  # Opinionated data-science meta-pack
  "lubridate",  # Dates/times made easy
  "rstudioapi", # RStudio IDE API
  "data.table", # High-performance tables & fast aggregation
  "doParallel"  # Parallel backend for foreach
)

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
  cat(sprintf("Encountered errors while trying to install the following packages: %s \n", packages_error))
  cat("Try to install them manually.")
} 
