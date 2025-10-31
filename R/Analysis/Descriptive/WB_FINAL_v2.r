#libraries
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
if (!require("sandwich")) install.packages("sandwich"); library("sandwich")
if (!require("lmtest")) install.packages("lmtest"); library("lmtest")

# ---Step 1 : Database connection 

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

# --- Step 2: Data Pulling ---
GDP_DATA <- dbReadTable(con, "current_compiled_data") %>% arrange(Climate_Year)

# --- Step 2: Data Preparation ---
agro_gdp <- GDP_DATA %>%
  mutate(
    year = as.integer(Climate_Year),
    value = as.numeric(GDP_Value)
  ) %>%
  arrange(year)

# --- Step 3: Add 3-Year Moving Average ---
agro_gdp <- agro_gdp %>%   # <— use the already mutated object, not GDP_DATA
  mutate(
    moving_avg = zoo::rollmean(value, k = 3, fill = NA, align = "right")
  )

# --- Step 4: Convert to Long Format for Plotting ---
agro_long <- agro_gdp %>%
  select(year, Actual = value, `3-Year Moving Average` = moving_avg) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "Value")

# --- Step 5: Plot Time Series (no final label) ---
GDP <- ggplot(agro_long, aes(x = year, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 1.1) +
  geom_point(size = 0) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "steelblue", "3-Year Moving Average" = "orange")) +
  scale_linetype_manual(values = c("Actual" = "solid", "3-Year Moving Average" = "solid")) +
  labs(
    title = "Agriculture, Forestry, and Fishing Value Added (% of GDP) — Mauritius (1981–2024)",
    x = "Year", y = "% of GDP",
    color = "Series Key", linetype = "Series Key",
    caption = "Source: World Bank"
  ) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "right")
# --- Step 6: Display and Save ---
print(GDP)

ggsave(filename = "agriculture_gdp_trend_mauritius.png",path=result_folder,plot= GDP,width = 8, height = 5, dpi = 300)



#%change in agricultural variables vs %change in GDP 
# ---------- 1) Compute YoY % changes (single source of truth) ----------
df_yoy <- GDP_DATA %>%
  arrange(Harvest_Year) %>%
  transmute(
    Year        = Harvest_Year,
    Yield,
    Production,
    Area_harvested,
    GDP         = GDP_Value
  ) %>%
  mutate(
    d_Yield      = (Yield / lag(Yield) - 1) * 100,
    d_Production = (Production / lag(Production) - 1) * 100,
    d_Area       = (Area_harvested / lag(Area_harvested) - 1) * 100,
    d_GDP        = (GDP / lag(GDP) - 1) * 100
  ) %>%
  filter(!is.na(d_GDP), !is.na(d_Yield), !is.na(d_Production), !is.na(d_Area))

print(df_yoy)

# ---------- 2) Long format for a single-panel plot ----------
plot_df <- df_yoy %>%
  select(Year, d_GDP, d_Yield, d_Production, d_Area) %>%
  pivot_longer(
    cols = c(d_Yield, d_Production, d_Area),
    names_to = "Metric",
    values_to = "Ag_YoY"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("d_Production", "d_Yield", "d_Area"), # legend order
                    labels = c("Production YoY (%)", "Yield YoY (%)", "Area Harvested YoY (%)")
    )
  )

# ---------- 3) palette ----------
Colours <- c(
  "Production YoY (%)"       = "orange",
  "Yield YoY (%)"            = "steelblue", 
  "Area Harvested YoY (%)"   = "green"  
)

# ---------- 4) Single visual: 3 metrics vs GDP change ----------
# compute neat limits rounded to the nearest 5
x_min <- floor(min(plot_df$d_GDP,    na.rm = TRUE) / 5) * 5
x_max <- ceiling(max(plot_df$d_GDP,  na.rm = TRUE) / 5) * 5
y_min <- floor(min(plot_df$Ag_YoY,   na.rm = TRUE) / 5) * 5
y_max <- ceiling(max(plot_df$Ag_YoY, na.rm = TRUE) / 5) * 5

percentage_change<-ggplot(plot_df, aes(x = d_GDP, y = Ag_YoY, color = Metric)) +
  # zero-reference lines (thin + subtle)
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  
  # points: slightly larger + a touch of transparency for readability
  geom_point(size = 2.8, alpha = 0.85) +
  
  # per-metric linear trend with visible but not heavy line, CI muted
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  
  scale_color_manual(values = Colours, name = NULL) +
  scale_x_continuous(
    "GDP YoY change (%)",
    breaks = seq(x_min, x_max, by = 5),
    labels = function(x) paste0(x, "%"),
    limits = c(x_min, x_max)
  ) +
  scale_y_continuous(
    "Agriculture YoY change (%)",
    breaks = seq(y_min, y_max, by = 5),
    labels = function(x) paste0(x, "%"),
    limits = c(y_min, y_max)
  ) +
  
  labs(
    title = "Agricultural Changes vs GDP Change ",
    subtitle = "Each point is a year; separate linear fits per metric with 95% confidence bands"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),   # centered title
    plot.subtitle = element_text(hjust = 0.5),               # centered subtitle
    panel.grid.minor = element_blank(),       # declutter
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title.position = "plot" ,                            # nicer spacing (optional)
    legend.text = element_text(size = 12)
  )
percentage_change
ggsave(filename = "sugarcane_changes_vs_GDP.png",path=result_folder,plot= percentage_change,width = 8, height = 5, dpi = 300)


#======================-----------END OF GDP SCRIPT---------======================#
