#=================-----------NASA POWER SEASONAL & TREND ANALYSIS---------=================#

# --- Libraries ---
library(ggplot2) #plots
library(forecast) #time series forecasting
library(tibble) #restructuring data frames
library(zoo) #for observed observation in irregular time series
library("patchwork") #combining 2/more plots into 1 visual 
library("lubridate") #for easy date/time manipulation
library("tidyverse") #collection of R packages (optional)
library("broom") #converts objects into tidy data frames
library("ggrepel") #prevents labels from overlapping
library("scales") #scales customisation
library("outliers") #outlier detection and handling
library("e1071") #for fuzzy clustering
library("dplyr") #data_manipulation
library("ggplot2") #graph visualisation
library("psych") #used for descriptive analysis
library("tidyr") #datasets
library("DBI") #SQL database connection
library("RMariaDB") #SQL database connection
library(gridExtra)

# ---- Output directory (ensures files are saved in a known location) -------------------------------
dir_stage <- file.path(getwd(), "outputs")         # define an "outputs" folder under current wd
if (!dir.exists(dir_stage)) dir.create(dir_stage, recursive = TRUE)  # create folder (with full directory) if missing

# ---------- 0) Prepare data ----------
#  Connect to DB - User to include host and password information
# Define connection parameters
db_name <- "data_handling"
db_host <- "127.0.0.1"
db_user <- "root"
db_password <- "786Suhqyr***"


# Create a safe connection
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = db_name,
  host = db_host,
  user = db_user,
  password = db_password
)

final_nasa <- dbReadTable(con, "climate")
# Parse date column
final_nasa$date <- as.Date(final_nasa$date)

# Replace negative solar radiation values with mean
solar_mean <- mean(final_nasa$Solar_radiation_kWh_m2[final_nasa$Solar_radiation_kWh_m2 > 0], na.rm = TRUE)
final_nasa_pow <- final_nasa %>%
  mutate(Solar_radiation_kWh_m2 = ifelse(Solar_radiation_kWh_m2 < 0, solar_mean, Solar_radiation_kWh_m2))

# --- Step 1: Aggregate to Monthly and Annual Means ---
nasa_monthly <- final_nasa_pow %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarise(across(c(Temperature_in_degree_celsius, Precipitation_in_mm,
                     Humidity_percent, Solar_radiation_kWh_m2),
                   mean, na.rm = TRUE), .groups = "drop")

nasa_annual <- nasa_monthly %>%
  group_by(year) %>%
  summarise(across(-month, mean, na.rm = TRUE))

# --- Step 2: Descriptive Statistics ---
cat("\n=== Descriptive Statistics (Annual Means) ===\n")
print(summary(nasa_annual))

# --- Step 3: Compute Linear Trends ---
trend_models <- list(
  Temperature = lm(Temperature_in_degree_celsius ~ year, data = nasa_annual),
  Rainfall = lm(Precipitation_in_mm ~ year, data = nasa_annual),
  Humidity = lm(Humidity_percent ~ year, data = nasa_annual),
  Solar = lm(Solar_radiation_kWh_m2 ~ year, data = nasa_annual)
)

trend_summary <- lapply(trend_models, function(model) {
  tidy(model) %>%
    filter(term == "year") %>%
    mutate(
      R2 = summary(model)$r.squared,
      Var = gsub(".*\\$|lm\\(", "", deparse(model$call)),
      Trend_per_year = estimate,
      P_value = p.value
    ) %>%
    select(Var, Trend_per_year, P_value, R2)
}) %>%
  bind_rows()

# Save trend summary
write.csv(trend_summary, file.path(dir_stage, "nasa_trend_summary.csv"), row.names = FALSE)

cat("\n=== Linear Trend Summary ===\n")
print(trend_summary)

# --- Step 4: Create Trend Plots with 3-Year Moving Average ---
plot_trend <- function(df, yvar, ylabel, title, color) {
  df %>%
    mutate(MA3 = rollmean(.data[[yvar]], k = 3, fill = NA, align = "right")) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = .data[[yvar]], color = "Actual"), size = 1) +
    geom_line(aes(y = MA3, color = "3-Year Moving Average"), size = 1, linetype = "solid") +
    geom_smooth(aes(y = .data[[yvar]]), method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    scale_color_manual(values = c("Actual" = color, "3-Year Moving Average" = "orange")) +
    labs(title = title, x = "Year", y = ylabel, color = "Legend") +
    theme_minimal(base_size = 13)
}

p_temp <- plot_trend(nasa_annual, "Temperature_in_degree_celsius", "Temperature (°C)",
                     "Trend of Mean Temperature (°C)", "#d1495b")

p_rain <- plot_trend(nasa_annual, "Precipitation_in_mm", "Precipitation (mm)",
                     "Trend of Mean Rainfall (mm)", "#1d4e89")

p_solar <- plot_trend(nasa_annual, "Solar_radiation_kWh_m2", "Solar Radiation (kWh/m²)",
                      "Trend of Mean Solar Radiation (kWh/m²)", "#b66904")

p_humidity <- plot_trend(nasa_annual, "Humidity_percent", "Humidity (%)",
                         "Trend of Mean Relative Humidity (%)", "#397d54")

# --- Step 5: Combine and Save Trend Plots ---
trend_combined <- grid.arrange(p_temp, p_rain, p_solar, p_humidity, ncol = 2)
trend_png_path <- file.path(dir_stage, "nasa_trend_profiles.png")
ggsave(filename = trend_png_path, plot = trend_combined, width = 12, height = 8, dpi = 300)

cat(paste0("\n📈 Trend plots saved to: ", trend_png_path, "\n"))

# --- Step 6: Seasonal Profiles (Existing Code) ---
nasa_seasonal <- nasa_monthly %>%
  group_by(month) %>%
  summarise(across(c(Temperature_in_degree_celsius, Precipitation_in_mm,
                     Humidity_percent, Solar_radiation_kWh_m2),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  ungroup() %>%
  mutate(Month = factor(month.abb[month], levels = month.abb, ordered = TRUE))

plot_seasonal <- function(df, var_mean, var_sd, y_label, title, color = "blue") {
  ggplot(df, aes(x = Month, y = .data[[var_mean]], group = 1)) +
    geom_ribbon(aes(ymin = .data[[var_mean]] - .data[[var_sd]],
                    ymax = .data[[var_mean]] + .data[[var_sd]]),
                fill = "lightblue", alpha = 0.3) +
    geom_line(color = color, size = 1.2) +
    theme_minimal(base_size = 13) +
    labs(title = title, y = y_label, x = "Month")
}

p1 <- plot_seasonal(nasa_seasonal, "Temperature_in_degree_celsius_mean", "Temperature_in_degree_celsius_sd",
                    "Temperature (°C)", "Seasonal Profile: Temperature", "#d1495b")
p2 <- plot_seasonal(nasa_seasonal, "Precipitation_in_mm_mean", "Precipitation_in_mm_sd",
                    "Precipitation (mm)", "Seasonal Profile: Rainfall", "#1d4e89")
p3 <- plot_seasonal(nasa_seasonal, "Humidity_percent_mean", "Humidity_percent_sd",
                    "Humidity (%)", "Seasonal Profile: Humidity", "#397d54")
p4 <- plot_seasonal(nasa_seasonal, "Solar_radiation_kWh_m2_mean", "Solar_radiation_kWh_m2_sd",
                    "Solar Radiation (kWh/m²)", "Seasonal Profile: Solar Radiation", "#f2a541")

seasonal_combined <- grid.arrange(p1, p2, p3, p4, ncol = 2)
seasonal_png_path <- file.path(dir_stage, "nasa_seasonal_profiles.png")
ggsave(filename = seasonal_png_path, plot = seasonal_combined, width = 12, height = 8, dpi = 300)


## --- Step 7: bar chart and violin plot for Rainy days ---
df <- final_nasa %>% select(
  Date=date,
  DTemp = Temperature_in_degree_celsius,
  DRain = Precipitation_in_mm,
  Humidity=Humidity_percent,
  Solar=Solar_radiation_kWh_m2
)

Rain_df <- df %>%
  select(Date, DRain) %>%
  mutate(
    month = month(Date, label = TRUE, abbr = FALSE, locale = "en") # Full month names for clarity
  )

#Frequency of Rainy days
plot_frequency <- Rain_df %>%
  group_by(month) %>%
  summarise(Rainy_Days_Count = sum(DRain > 1)) %>%   # According to meteo , downpours >1 mm are considered precipitation
  ggplot(aes(x = month, y = Rainy_Days_Count, fill = month)) +
  geom_bar(stat = "identity") +     # To use the real Y values provided instead of doing a count like a default bar chart
  scale_fill_viridis_d() + # Use a nice color palette where _d indicates discrete
  labs(
    title = "Monthly Frequency of Rainy Days (1981-2024)",
    x = "Month",
    y = "Number of Days with Rainfall"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


#Violin plot of Rainy days
plot_intensity <- Rain_df %>%
  filter(DRain > 1) %>% # Only consider days with actual rainfall
  ggplot(aes(x = month, y = DRain, fill = month)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  # Add the y-axis limit here:
  coord_cartesian(ylim = c(0, 10)) + # This limits the y-axis view to 0-10 mm for readability
  scale_fill_viridis_d() +
  labs(
    title = "Monthly Distribution of Daily Rainfall Intensity (on Rainy Days)",
    x = "Month",
    y = "Rainfall (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Combining both rainy days plot
combined_plots <- plot_frequency / plot_intensity # Uses patchwork for side-by-side or stacked plots

combined_plots

#Define the different seasons for sugarcane cycles --> Plantation,Growth and Maturation for daily data as data is yearly in SQL
df$Month <- as.numeric(format(as.Date(df$Date), "%m"))
df$Year<-year(df$Date)
df$Season <- ifelse(df$Month >= 7 & df$Month <= 9, "Plantation",
                    ifelse(df$Month >= 10 | df$Month <= 3, "Growth",
                           ifelse(df$Month >= 4 & df$Month <= 6, "Maturation", NA)))

# Separate data by season
Plantation_data <- df[df$Season == "Plantation", ]
Growth_data <- df[df$Season == "Growth", ]
Maturation_data <- df[df$Season == "Maturation", ]


# --- Step 8: Plotting trends by cycles ---
#1st function for trends by cycle
#2ns function for density plots for temp/rainfall

#First, summarise the required means in a df 
summary_stats <- df %>%
  group_by(Season, Year) %>%
  summarise(
    Temp_Mean = mean(DTemp, na.rm = TRUE),
    Temp_Median = median(DTemp, na.rm = TRUE),
    Temp_Max = max(DTemp, na.rm = TRUE),
    Rain_Mean = mean(DRain, na.rm = TRUE),
    Rain_Median = median(DRain, na.rm = TRUE),
    Rain_Min = min(DRain, na.rm = TRUE),
    Rainy_Days = sum(DRain > 1, na.rm = TRUE),
    Total_precipitation=sum(DRain,na.rm=TRUE),
    Mean_Humidity=mean(Humidity, na.rm = TRUE),
    Mean_solar=mean(Solar, na.rm = TRUE),
    .groups = "drop"    # to ungroup the data (it is like removing the data from the folders, simplifying work for further analysis)
  )

pooled_seasonal_means <- summary_stats %>%
  group_by(Season) %>%
  summarise(
    Pooled_Mean_Temp = mean(Temp_Mean, na.rm = TRUE),
    Pooled_Mean_Rain = mean(Rain_Mean, na.rm = TRUE),
    Pooled_Mean_Humidity = mean(Mean_Humidity, na.rm = TRUE),
    Pooled_Mean_Solar = mean(Mean_solar, na.rm = TRUE),
    .groups = "drop"
  )
head(pooled_seasonal_means)

#1ST FUNCTION FOR TRENDS BY CYCLE

CYCLE_COLORS <- c(                          # color palette for seasons
  "Plantation" = "#d1495b",
  "Growth"     = "#1d4e89",
  "Maturation" = "#397d54"
)

CYCLE_SHAPES <- c(                          # shapes per season
  "Plantation" = 21,
  "Growth"     = 22,
  "Maturation" = 24
)
 #function1
plot_cycle_trend_from_summary <- function(          
  summary_stats,                                    # Data frame that must contain Season, Year, and a numeric y-variable
  yvar,                                             # y-variable 
  ylab,                                             #  y-axis label
  title,                                            # String: plot title
  subtitle,                                         #  subtitle 
  colors = CYCLE_COLORS,                            # Named vector of colors for seasons 
  shapes = CYCLE_SHAPES,                            # Named vector of point shapes for seasons
  y_step = 0.5,                                     # step size for y-axis 
  y_pad  = 0.2,                                     # small padding added to min/max for better visual
  x_by   = 2,                                       # distance of xaxis values
  caption = "Sugarcane cycles: Plantation (Jul–Sep), Growth (Oct–Mar), Maturation (Apr–Jun). LOESS ±95% CI." # Caption text
){
  y_min <- min(summary_stats[[yvar]], na.rm = TRUE) 
  y_max <- max(summary_stats[[yvar]], na.rm = TRUE) 
  y0    <- floor(y_min / y_step) * y_step           # for clean lower bound
  y1    <- ceiling(y_max / y_step) * y_step         # for clean upper bound
  y_br  <- seq(y0, y1, by = y_step)                 #  a sequence of y-axis breaks at y_step increments
  
  ggplot(summary_stats,                              
         aes(x = Year,                               
             y = .data[[yvar]],                      
             color = Season,                         
             group = Season)) +                      # separate paths per season
    geom_smooth(aes(fill = Season),                  # Add LOESS smooth with CI and season-based fill
                method = "loess",                    # Use LOESS smoothing-for non-linear trends
                se = TRUE,                           # Show standard error ribbon
                alpha = 0.2,                         # Make ribbon semi-transparent
                linewidth = 1.2) +                   # Thicker smooth line
    geom_line(linewidth = 0.8, alpha = 0.8) +        # Draw the raw annual mean line per season
    geom_point(aes(shape = Season),                  # Add points for each Year
               size = 2.5,                           # Point size
               stroke = 1.2,                         # Border thickness of the shape
               fill = "white") +                     
    labs(title = title,                              
         subtitle = subtitle,                        
         x = "Year",                                 # X-axis label
         y = ylab,                                   # Y-axis label (passed in)
         caption = caption) +                       
    scale_y_continuous(breaks = y_br,                # Apply computed y breaks
                       limits = c(y0 - y_pad,        # Extend lower limit slightly
                                  y1 + y_pad),       # Extend upper limit slightly
                       expand = c(0, 0)) +           # Remove default expansion
    scale_x_continuous(breaks = seq(min(summary_stats$Year), # Create year steps from min to max year
                                    max(summary_stats$Year),
                                    by = x_by),
                       expand = expansion(add = c(0.5, 3))) + # Add some space on x-axis for labels
    scale_color_manual(values = colors) +            # Apply season colors to lines/points
    scale_fill_manual(values  = colors) +            # Apply same colors to LOESS ribbons
    scale_shape_manual(values = shapes) +            # Apply season-specific shapes
    theme_minimal(base_size = 14) +                  # Use a clean minimal theme
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 18),         # Center/bold title
      plot.subtitle= element_text(hjust = 0.5, size = 12, color = "gray40",
                                  margin = margin(b = 10)),                        # Style subtitle
      plot.caption = element_text(hjust = 0, size = 9, color = "gray50",
                                  margin = margin(t = 10)),                        # Style caption
      axis.title   = element_text(size = 14, face = "bold"),                       # Emphasize axis titles
      axis.text    = element_text(size = 11, color = "gray30"),                    # Axis tick label style
      legend.position = "top",                                                     # Put legend at the top
      panel.grid.minor = element_blank(),                                          # Remove minor grid
      panel.grid.major.x = element_line(color = "grey90", linetype = "dotted",
                                        linewidth = 0.3),                          # Light vertical grid
      panel.grid.major.y = element_line(color = "grey90", linetype = "solid",
                                        linewidth = 0.5),                          # Light horizontal grid
      panel.border       = element_rect(color = "grey70", fill = NA, linewidth = 0.5), # Thin border around panel
      axis.line          = element_line(color = "black", linewidth = 0.6)          # Axis lines for crispness
    )
}



#Calls for each graph
# 1) Temperature
g_temp <- plot_cycle_trend_from_summary(               
  summary_stats = summary_stats,                       
  yvar  = "Temp_Mean",                                 
  ylab  = "Mean Temperature (°C)",                     
  title = "Annual Mean Temperature Trends by Sugarcane Production Cycles (1981–2024)", 
  subtitle = "Consistent warming trends observed across cycles"                     
)
#print(g_temp)

# 2) Rainfall
g_rain <- plot_cycle_trend_from_summary(        
  summary_stats = summary_stats,
  yvar  = "Rain_Mean",
  ylab  = "Mean Rainfall (mm)",
  title = "Annual Mean Rainfall Trends by Season (1981–2024)",
  subtitle = "Inter-annual variability with divergent long-term tendencies by cycle"
)
print(g_rain)

# 3) Humidity
g_hum <- plot_cycle_trend_from_summary(           
  summary_stats = summary_stats,
  yvar  = "Mean_Humidity",
  ylab  = "Mean Humidity (%)",
  title = "Annual Mean Humidity Trends by Sugarcane Production Cycles (1981–2024)",
  subtitle="U-shaped humidity trends reflecting climate forcing and subsequent recovery across sugarcane cycles"
)
#print(g_hum)

# 4) Solar (filter early years with poor data, as in your script)
summary_stats_solar <- summary_stats %>% dplyr::filter(Year >= 1984)
g_solar <- plot_cycle_trend_unified(
  df = summary_stats_solar,
  yvar = "Mean_solar",
  ylab = "Mean Solar Radiation (kWh/m²)",
  title = "Annual Mean Solar Radiation Trends by Sugarcane Production Cycles (1984–2024)",
  subtitle="Consistently high solar radiation during sugarcane growth, contrasting with lower increasing trends in other phases"
)

#print(g_solar)

ggsave(file.path(dir_stage, "trend_cycles_temp.png"),  g_temp,  width = 12, height = 8, dpi = 300)
ggsave(file.path(dir_stage, "trend_cycles_rain.png"),  g_rain,  width = 12, height = 8, dpi = 300)
ggsave(file.path(dir_stage, "trend_cycles_hum.png"),   g_hum,   width = 12, height = 8, dpi = 300)
ggsave(file.path(dir_stage, "trend_cycles_solar.png"), g_solar, width = 12, height = 8, dpi = 300)






#2ND FUNCTION FOR DENSITY PLOTS
plot_cycle_density_with_pooled <- function(            
  df,                                                  # Daily-level data 
  xvar,                                                # column name for x-axis
  title,                                               
  xlab,                                                
  subtitle=NULL,                                            # subtitle
  pooled_df = NULL,                                    # Optional: precomputed pooled means per Season (daily means)
  mean_col  = "Mean",                                  # Name of the column in pooled_df containing the mean
  units = "",                                          
  colors = CYCLE_COLORS,                               
  x_limits = NULL,                                     # Optional x-axis limits c(min, max)-mostly for rainfall
  x_breaks  = NULL                                     # Optional vector of x-axis breaks-ostly for rainfall
){
 
  
  if (is.null(pooled_df)) {                            # If user didn’t supply pooled means…
    pooled_df <- df %>%                                # …compute pooled daily mean per Season from df directly
      dplyr::group_by(Season) %>%
      dplyr::summarise(!!mean_col := mean(.data[[xvar]], na.rm = TRUE),
                       .groups = "drop")
  } else {
    stopifnot(all(c("Season", mean_col) %in% names(pooled_df))) # Ensure provided pooled_df has needed columns
  }
  
  density_peaks <- df %>%                              # Compute density peak height per Season for label placement
    dplyr::group_by(Season) %>%
    dplyr::summarise(
      y_peak = {                                       # For each Season, get max density height (for annotation y)
        v <- .data[[xvar]]                             # Extract the vector for this Season
        v <- v[is.finite(v)]                           # Drop non-finite values (NA, Inf)
        if (length(v) > 1) max(stats::density(v)$y) else NA_real_  # Density only valid with >1 data point
      },
      .groups = "drop"
    )
  
  anno <- pooled_df %>%                                # Build annotation frame combining pooled means and peaks
    dplyr::left_join(density_peaks, by = "Season") %>%
    dplyr::mutate(
      y_label = 0.7 * y_peak,                         # Place text slightly below the peak (95% of peak height)
      label   = sprintf("Mean: %.1f%s", .data[[mean_col]], units) # Text like "Mean: 24.6°C"
    )
  
  if (is.null(x_limits)) {                             # If no x limits passed, compute a padded range from data
    x_min <- floor(min(df[[xvar]], na.rm = TRUE))      # Floor to the nearest integer for a clean min
    x_max <- ceiling(max(df[[xvar]], na.rm = TRUE))    # Ceil to the nearest integer for a clean max
    x_limits <- c(x_min - 0.5, x_max + 0.5)            # Add a half-unit padding on both sides
  }
  if (is.null(x_breaks)) {                             # If no x breaks passed, create 1-unit tick marks
    x_breaks <- seq(floor(x_limits[1]), ceiling(x_limits[2]), by = 1)
  }
  
  ggplot(df, aes(x = .data[[xvar]],                    # Start ggplot with x mapped to the chosen variable
                 color = Season, fill = Season)) +     # Color/fill densities by Season
    geom_density(alpha = 0.4, linewidth = 0.8) +       # Draw density curves with semi-transparent fill
    geom_vline(                                        # Add dashed mean lines per Season
      data = pooled_df,
      aes(xintercept = .data[[mean_col]], color = Season),
      linetype = "dashed", linewidth = 0.7, show.legend = FALSE
    ) +
    geom_text(                                         # Add rotated text near the mean line per Season
      data = anno,
      aes(x = .data[[mean_col]], y = y_label, label = label),
      color = "black", size = 3.5, hjust = -0.1, vjust = 0, angle = 90, show.legend = FALSE
    ) +
    labs(                                              # Titles and labels
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = "Density",
      caption = "Dashed lines indicate pooled mean for each cycle."
    ) +
    scale_color_manual(values = colors) +              # Apply season colors to lines
    scale_fill_manual(values  = colors) +              # Apply season colors to fills
    scale_x_continuous(limits = x_limits, breaks = x_breaks) + # Fix x-axis scale (limits & breaks)
    theme_minimal(base_size = 14) +                    # Minimal theme for clean look
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)), # Title style
      plot.subtitle= element_text(hjust = 0.5, size = 12, color = "gray40", margin = margin(b = 15)), # Subtitle style
      plot.caption = element_text(hjust = 0, size = 9, color = "gray50", margin = margin(t = 10)),    # Caption style
      axis.title   = element_text(size = 14, face = "bold", margin = margin(t = 10, b = 0)),          # Axis title style
      axis.text    = element_text(size = 11, color = "gray30"),                                       # Tick label style
      legend.position = "top",                                                                         # Legend at top
      legend.title    = element_blank(),                                                               # No legend title
      panel.grid.minor = element_blank(),                                                              # Remove minor grid
      panel.grid.major.x = element_line(color = "grey90", linetype = "dotted", linewidth = 0.3),      # Light vertical grid
      panel.grid.major.y = element_line(color = "grey90", linetype = "solid",  linewidth = 0.5),      # Light horizontal grid
      panel.border       = element_rect(color = "grey70", fill = NA, linewidth = 0.5),                # Panel border
      axis.line          = element_line(color = "black", linewidth = 0.6)                              # Axis lines
    )
}



# Temperature density (uses pooled daily means computed internally unless you pass pooled_df)
g_temp_density <- plot_cycle_density_with_pooled(
  df      = df,                                        # Daily data frame with Season and DTemp
  xvar    = "DTemp",                                   # Column to plot on x-axis
  title   = "Distribution of Daily Temperatures by Cycle",   # Title
  subtitle= "Distinct temperature ranges across production phases", # Subtitle
  xlab    = "Daily Temperature (°C)",                  # X-axis label
  units   = "°C"                                       # Units appended to mean labels
)
print(g_temp_density)

# Rainfall density (fix x-axis to 0–5 with 1 mm ticks for clarity)
g_rain_density <- plot_cycle_density_with_pooled(
  df       = df,                                       # Daily data frame with Season and DRain
  xvar     = "DRain",                                  # Column to plot on x-axis
  title    = "Distribution of Daily Rainfall by Season (1981–2024)", # Title
  subtitle = "Right-skewed distributions: many low-rain days with occasional heavy events.", # Subtitle
  xlab     = "Rainfall (mm)",                          # X-axis label
  units    = " mm",                                    # Units appended to mean labels (note space before mm)
  x_limits = c(0, 5),                                  # Fix axis to 0–5 mm
  x_breaks = seq(0, 15, by = 1)                        # Show ticks every 1 mm (even beyond limits for consistency)
)
print(g_rain_density)


ggsave(file.path(dir_stage, "density_temperature.png"), g_temp_density, width = 12, height = 8, dpi = 300)
ggsave(file.path(dir_stage, "density_rainfall.png"),    g_rain_density, width = 12, height = 8, dpi = 300)
print(dir_stage)
cat(paste0("\n📸 Seasonal profiles saved to: ", seasonal_png_path, "\n"))
cat("\n✅ NASA POWER trend and seasonal analysis completed successfully!\n")


# Build seasonal annual means if not already present
seasonal_annual <- df %>%
  group_by(Season, AgroYear) %>%
  summarise(
    Solar = mean(Solar, na.rm = TRUE),
    .groups = "drop"
  )

# Function to fit per-season for a given period range
fit_season_period <- function(seasonal_tbl, y = "Solar", y_start, y_end) {
  seasonal_tbl %>%
    filter(AgroYear >= y_start, AgroYear <= y_end) %>%
    group_by(Season) %>%
    group_modify(~ fit_with_ci(.x, y_col = y)) %>%
    ungroup() %>%
    mutate(Period = paste0(y_start, "–", y_end, " (AgroYear)")) %>%
    select(Period, Season, everything())
}

solar_by_season_81_00 <- fit_season_period(seasonal_annual, y_start = 1981, y_end = 2003)
solar_by_season_01_24 <- fit_season_period(seasonal_annual, y_start = 2004, y_end = 2024)

solar_season_trends_split <- bind_rows(solar_by_season_81_00, solar_by_season_01_24) %>%
  mutate(
    Slope_per_year = round(Slope_per_year, 6),
    CI_low_95      = round(CI_low_95, 6),
    CI_high_95     = round(CI_high_95, 6),
    R2             = round(R2, 3),
    p_value        = signif(p_value, 3)
  ) %>%
  arrange(Season, Period)

view(solar_season_trends_split)


#======================-----------END OF NASA SCRIPT---------======================#

