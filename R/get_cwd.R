# Load the rstudioapi library 
library(rstudioapi)

# -----------------    Resolve the script's directory ---------------------------
get_script_dir <- function() {
  tryCatch({
    # Case 1: If run via Rscript (command line)
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd_args, value = TRUE)
    if (length(file_arg) > 0) {
      return(dirname(normalizePath(sub("^--file=", "", file_arg))))
    }
    
    # Case 2: If run inside RStudio
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      return(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))
    }
    
    # Case 3: Fallback (interactive console / unknown case)
    return(getwd())
  },
  error = function(e) {
    # Last resort fallback
    return(getwd())
  })
}