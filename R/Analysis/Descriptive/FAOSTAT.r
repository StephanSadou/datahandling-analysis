# --- Libraries ---
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("zoo")) install.packages("zoo"); library(zoo)
if (!require("cowplot")) install.packages("cowplot"); library(cowplot)
library("patchwork")
library("lubridate")
library("tidyverse")
library("broom")
library("ggrepel")
library("scales")
library("ggrepel")
library("outliers")
library("e1071")
library("tidyr") #datasets
library("DBI")
library("RMariaDB")
library("rprojroot")

source("get_cwd.R")
# Read the environment file to obtain the database credentials 
root <- find_root(has_file(".Renviron"))
readRenviron(file.path(root, ".Renviron"))

descriptive_folder = get_script_dir()
result_folder = file.path(descriptive_folder, "results")
# Use the credentials to connect to our local database 
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# --- Step 1: Read Data ---
final_faostat <- dbReadTable(con, "current_compiled_data") %>% arrange(Climate_Year)


# --- Step 2: Extract Columns ---
year <- final_faostat$Climate_Year
yield_t_ha <- final_faostat$Yield
production_tonnes <- final_faostat$Production
area_harvested <- final_faostat$Area_harvested

# --- Step 3: Create Data Frames ---
mauritius_yield <- data.frame(Year = year, Yield_t_ha = yield_t_ha)
sugarcane_prod  <- data.frame(Year = year, Production_Tonnes = production_tonnes)
mauritius_area  <- data.frame(Year = year, Area_Harvested = area_harvested)

# --- Step 4: Compute 3-Year Moving Averages ---
mauritius_yield <- mauritius_yield %>%
  mutate(Moving_Avg = rollmean(Yield_t_ha, k = 3, fill = NA, align = "right"))
sugarcane_prod <- sugarcane_prod %>%
  mutate(Moving_Avg = rollmean(Production_Tonnes, k = 3, fill = NA, align = "right"))
mauritius_area <- mauritius_area %>%
  mutate(Moving_Avg = rollmean(Area_Harvested, k = 3, fill = NA, align = "right"))

# ========================================================= #
# --- 1. TREND PLOTS ---
# ========================================================= #

p_yield <- ggplot(mauritius_yield, aes(x = Year)) +
  geom_line(aes(y = Yield_t_ha, color = "Actual Yield"), size = 1.2) +
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), 
            size = 1, linetype = "dashed") +
  labs(title = "Yield (t/ha)", x = "Year", y = "Tonnes/ha") +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Actual Yield" = "darkgreen", "3-Year Moving Average" = "orange")) +
  theme(legend.position = "bottom")

p_prod <- ggplot(sugarcane_prod, aes(x = Year)) +
  geom_line(aes(y = Production_Tonnes, color = "Actual Production"), size = 1.2) +
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), 
            size = 1, linetype = "dashed") +
  labs(title = "Production (Tonnes)", x = "Year", y = "Tonnes") +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Actual Production" = "darkblue", "3-Year Moving Average" = "orange")) +
  theme(legend.position = "bottom")

p_area <- ggplot(mauritius_area, aes(x = Year)) +
  geom_line(aes(y = Area_Harvested, color = "Actual Area"), size = 1.2) +
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), 
            size = 1, linetype = "dashed") +
  labs(title = "Area Harvested (ha)", x = "Year", y = "Hectares") +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Actual Area" = "#06010e", "3-Year Moving Average" = "orange")) +
  theme(legend.position = "bottom")

# Combine legend from all three
legend_trend <- get_legend(p_yield + theme(legend.position = "bottom"))

trend_panel <- plot_grid(
  p_yield + theme(legend.position = "none"),
  p_prod + theme(legend.position = "none"),
  p_area + theme(legend.position = "none"),
  ncol = 3, align = "v"
)

trend_final <- plot_grid(
  trend_panel, legend_trend,
  ncol = 1, rel_heights = c(1, 0.08)
)

ggsave(
  filename = "sugarcane_trends_mauritius_1x3_combined.png",
  path     = result_folder,
  plot     = trend_final,
  width    = 18, height = 6, dpi = 300
)

# ========================================================= #
# --- 2. HISTOGRAM PLOTS ---
# ========================================================= #

h_yield <- ggplot(mauritius_yield, aes(x = Yield_t_ha)) +
  geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Yield_t_ha, na.rm = TRUE), color = "Mean"), 
             size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median(Yield_t_ha, na.rm = TRUE), color = "Median"), 
             size = 1, linetype = "dotted") +
  scale_color_manual(name = "Reference", values = c("Mean" = "blue", "Median" = "red")) +
  labs(title = "Yield Distribution", x = "Tonnes per Hectare", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

h_prod <- ggplot(sugarcane_prod, aes(x = Production_Tonnes)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Production_Tonnes, na.rm = TRUE), color = "Mean"), 
             size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median(Production_Tonnes, na.rm = TRUE), color = "Median"), 
             size = 1, linetype = "dotted") +
  scale_color_manual(name = "Reference", values = c("Mean" = "blue", "Median" = "red")) +
  labs(title = "Production Distribution", x = "Tonnes", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

h_area <- ggplot(mauritius_area, aes(x = Area_Harvested)) +
  geom_histogram(bins = 15, fill = "khaki", color = "darkgoldenrod", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Area_Harvested, na.rm = TRUE), color = "Mean"), 
             size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = median(Area_Harvested, na.rm = TRUE), color = "Median"), 
             size = 1, linetype = "dotted") +
  scale_color_manual(name = "Reference", values = c("Mean" = "blue", "Median" = "red")) +
  labs(title = "Area Harvested Distribution", x = "Hectares", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

legend_hist <- get_legend(h_yield + theme(legend.position = "bottom"))

hist_panel <- plot_grid(
  h_yield + theme(legend.position = "none"),
  h_prod + theme(legend.position = "none"),
  h_area + theme(legend.position = "none"),
  ncol = 3, align = "v"
)

hist_final <- plot_grid(
  hist_panel, legend_hist,
  ncol = 1, rel_heights = c(1, 0.08)
)

ggsave(
  filename = "sugarcane_distributions_mauritius_1x3_combined.png",
  path     = result_folder,
  plot     = hist_final,
  width    = 18, height = 6, dpi = 300
)



# ========================================================= #
cat("\n Saved combined figures with unified legends and dashed moving averages:\n")
cat("- sugarcane_trends_mauritius_1x3_combined.png\n")
cat("- sugarcane_distributions_mauritius_1x3_combined.png\n")
