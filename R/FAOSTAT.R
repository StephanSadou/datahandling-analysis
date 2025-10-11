# Load all the required packages
library(cli)
library(dplyr)
library(tidyr)
library(FAOSTAT)
library(data.table)

# ----------------------------------------------------------------- #
# ----- Step 0: Initialize the folder paths to save the files ----- # 
# ----------------------------------------------------------------- #

# Call function from another R script to get current working directory 
source("get_cwd.R") 
current_dir <- get_script_dir()

# From the R script, move to the root folder where all the other folders are present 
root_dir <- normalizePath(file.path(current_dir, ".."))

# Assign folder for raw and cleansed data
dir_raw   <- file.path(root_dir, "data_raw")
dir_stage <- file.path(root_dir, "data_stage")


# --------------------------------------------------------------------- #
# ----- Step 1: Download FAOSTAT QCL bulk (goes to raw directory) ----- # 
# --------------------------------------------------------------------- #

# This downloads the latest bulk files, including the Normalized one, into dir_raw.
# cat(sprintf("Downloading FAOSTAT QCL bulk dataset to: %s", dir_raw))

# Function to suppress messages when downloading FAOSTAT data 
quiet <- function(expr) {
  tf <- file()
  sink(tf)                     # capture stdout
  sink(tf, type = "message")   # capture messages
  on.exit({sink(); sink(type = "message"); close(tf)}, add = TRUE)
  suppressWarnings(suppressMessages(eval.parent(substitute(expr))))
}

crops_info <- quiet(FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = "data_raw"))

# crops_info <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = dir_raw)


# --------------------------------------------------------------------- #
# ----------- Step 2: Find and unzip the 'Normalized' file ------------ # 
# --------------------------------------------------------------------- #

norm_zip <- list.files(dir_raw,
                       pattern = "Normalized.*\\.zip$",
                       full.names = TRUE,
                       ignore.case = TRUE)

if (length(norm_zip) == 0L) {
  stop("Could not find the 'Normalized' zip file in ", dir_raw,
       ". Contents:\n", paste(list.files(dir_raw), collapse = "\n"))
}

# Extract content from the zip files 
unzipped_files <- unzip(norm_zip[1], exdir = dir_raw, overwrite = TRUE)

# Locate the normalized CSV (either from unzip result or by pattern)
norm_csv <- unzipped_files[grepl("\\.csv$", unzipped_files, ignore.case = TRUE)]
if (!length(norm_csv)) {
  # Fallback: search the folder
  norm_csv <- list.files(dir_raw,
                         pattern = "Normalized.*\\.csv$",
                         full.names = TRUE,
                         ignore.case = TRUE)
}

if (length(norm_csv) == 0L) {
  stop("Could not find the Normalized CSV after unzipping in ", dir_raw)
}

norm_csv <- norm_csv[1]
# cat(sprintf("Normalized CSV found: %s", norm_csv))


# --------------------------------------------------------------------- #
# -------------------Step 3:Data Cleansing ----------------------------
# --------------------------------------------------------------------- #

tb <- data.table::fread(norm_csv, nThread = max(1, parallel::detectCores() - 1)) %>%
  filter(!is.na(Value)) %>%                         #Filter out blank amounts
  filter(!grepl("^'F", `Item Code (CPC)`)) %>%      #Filter out subtotals to avoid duplicated amounts
  filter(Area=="Mauritius") %>%
  filter(Element == "Area harvested" | Element == "Production" | Element == "Yield")%>%          #Considering Area Harvested,Production & Yield
  group_by(Item, Year, Element) %>%
  summarise(Value = max(Value), .groups = "drop") %>% 
  pivot_wider(names_from=Element, values_from =Value)%>%            #Transpose data and rename columns
  rename(Area_harvested ="Area harvested")%>% 
  replace_na(list(Area_harvested = 0, Production = 0, Yield=0))%>%   #New blank amounts filled in as 0
  select(Item,Year,Area_harvested,Production,Yield)

# summary(tb)
# str(tb)

# --------------------------------------------------------------------- #
# ------Step 4: Analyzing raw data for further filtering conditions -----
# --------------------------------------------------------------------- #

# Step A: Group by crops and sum on area harvested

Crop_Contribution <- tb%>%group_by(Item) %>% 
  summarise(Total_area=sum(Area_harvested))%>% 
  arrange(desc(Total_area))

total_rows <- nrow(Crop_Contribution)
#Since there are 60 items, separate top 5 items and group remaining as 'others' for proper representation on pie chart
# Step B: Top 5 crops
top5 <- Crop_Contribution %>% slice_head(n = 5)

# Step C: Group the rest as "Others"
others <- Crop_Contribution %>%
  slice_tail(n = total_rows - 5) %>%
  summarise(Item = "Others", Total_area = sum(Total_area))

# Step D: Combine into pie_data and include percentage area harvested of each crop
pie_data <-  bind_rows(top5, others) %>%                                
  mutate(Percentage = Total_area / sum(Total_area),
         LegendLabel = paste0(Item, " (", percent(Percentage), ")"))

# Step E: Plot Piechart to illustrate percentage area harvested for each crop

ggplot(pie_data, aes(x = "", y = Total_area, fill = LegendLabel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Harvested Area by Crop (Top 5 + Others)", fill = "Crop") +
  theme_void()

# --------------------------------------------------------------------- #
# ---Step 5: Final criteria selection: Item-Sugarcane & year >-1981 ---
# --------------------------------------------------------------------- #
crops<-tb %>% 
  filter(Item=="Sugar cane")%>%             #Based on pie chart, Sugar cane contributes most to agricultural production
  filter(Year>=1981)                        #To align with other 3 dataset
select(Item,Year,'Area_harvested','Production','Yield')

View(crops)

# Saved our results into a csv file 
cleansed_csv_path <- file.path(dir_stage, "FAOSTAT_stage.csv")
data.table::fwrite(crops, cleansed_csv_path)

# cat(sprintf("Cleansed CSV written to: %s", cleansed_csv_path))
