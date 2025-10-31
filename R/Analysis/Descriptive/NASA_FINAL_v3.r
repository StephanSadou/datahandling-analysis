#=================-----------NASA: SEASONAL & TREND ANALYSIS---------=================#

# --- Libraries ---
library("ggplot2") #plots
library("forecast") #time series forecasting
library("tibble") #restructuring data frames
library("zoo") #for observed observation in irregular time series
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
library("tidyr") #datasets
library("DBI") #SQL database connection
library("RMariaDB") #SQL database connection
library("gridExtra")
library("rprojroot")
# ---- Output directory (ensures files are saved in a known location) -------------------------------
source("get_cwd.R")
cwd_des = get_script_dir()
descriptive_folder = file.path(cwd_des, 'Analysis', "Descriptive")
result_folder = file.path(descriptive_folder, "results")

# Read the environment file to obtain the database credentials 
root <- find_root(has_file(".Renviron"))
readRenviron(file.path(root, ".Renviron"))

# Checks if the folder exists or not - else creates it 
if (!dir.exists(result_folder)) {
  dir.create(result_folder, showWarnings = FALSE)
} 

# Use the credentials to connect to our local database 
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)


final_nasa <- dbReadTable(con, "climate")
# Parse date column
final_nasa$date <- as.Date(final_nasa$date)

# Replace negative solar radiation values with mean
solar_mean <- mean(final_nasa$Solar_radiation_kWh_m2[final_nasa$Solar_radiation_kWh_m2 > 0], na.rm = TRUE)
final_nasa_pow <- final_nasa %>%
  mutate(Solar_radiation_kWh_m2 = ifelse(Solar_radiation_kWh_m2 < 0, solar_mean, Solar_radiation_kWh_m2))

##########################Aggregate to Monthly and Annual Means ---
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

##########################Descriptive Statistics ---
cat("\n=== Descriptive Statistics (Annual Means) ===\n")
print(summary(nasa_annual))

##########################Compute Linear Trends ---
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


print(trend_summary)

##########################Create Trend Plots with 3-Year Moving Average ---
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

##########################Combine and Save Trend Plots ---
trend_combined <- grid.arrange(p_temp, p_rain, p_solar, p_humidity, ncol = 2)
ggsave(
  filename = "nasa_trend_profiles.png",
  path     = result_folder,
  plot     = trend_combined,
  width    = 18, height = 6, dpi = 300
)


##########################Seasonal Profiles ---
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
ggsave(
  filename = "nasa_seasonal_profiles.png",
  path     = result_folder,
  plot     = seasonal_combined,
  width    = 18, height = 6, dpi = 300
)

##########################Dataframe for 4 variables  ---
df <- final_nasa %>% select(
  Date=date,
  DTemp = Temperature_in_degree_celsius,
  DRain = Precipitation_in_mm,
  Humidity=Humidity_percent,
  Solar=Solar_radiation_kWh_m2
)

##########################Bar chart and violin plot for Rainy days ---
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
        plot.title   = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)), # Title style
  )


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
        plot.title   = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)), # Title style
  )

#Combining both rainy days plot
combined_plots <- plot_frequency / plot_intensity # Uses patchwork for stacked plots

combined_plots 

ggsave(
  filename = "Rainy_days_profiles.png",
  path     = result_folder,
  plot     = combined_plots,
  width    = 18, height = 6, dpi = 300
)



#Define the different seasons for sugarcane cycles --> Plantation,Growth and Maturation for daily data as data is yearly in SQL
df$Month <- as.numeric(format(as.Date(df$Date), "%m"))
df$Year<-year(df$Date)
df$Season <- ifelse(df$Month >= 7 & df$Month <= 9, "Plantation",
                    ifelse(df$Month >= 10 | df$Month <= 3, "Growth",
                           ifelse(df$Month >= 4 & df$Month <= 6, "Maturation", NA)))

# Separate data by season
Plantation_data <- df%>%filter(Season == "Plantation")
Growth_data <- df%>%filter(Season == "Growth")
Maturation_data <- df%>%filter(Season == "Maturation")


##########################Plotting trends by cycles ---
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


##########################Trend values by cycles ---
# Fits Value and Year for each Season and Variable (One Season-Year-Variable per line grouped in the same model)


trend_summary_season <- summary_stats %>%
  pivot_longer(                     #reshaping: one row per Season-Year-Variable so that all variables can be fitted in the same model
    cols = c(Temp_Mean,
             Rain_Mean,
             Mean_Humidity,
             Mean_solar),
    names_to  = "Var",          # column to store variable name
    values_to = "Value"         # column to store the numeric value
  ) %>%
  group_by(Season, Var) %>%
  summarise(
    n_years        = n(),                                             # number of years
    Trend_per_year = coef(lm(Value ~ Year))[2],                              #take 2nd element , the slope, as 1st element is intercept
    P_value        = summary(lm(Value ~ Year))$coefficients["Year","Pr(>|t|)"],   #2*4 matrix,indexes row for year and column for P-value with no linear trend
    R2             = summary(lm(Value ~ Year))$r.squared,                    # goodness of fit
    .groups = "drop"
  ) %>%
  mutate(        #make it visually better
    Trend_per_decade = Trend_per_year * 10,                                  # slope per decade
    Trend_per_year   = round(Trend_per_year,   6),
    Trend_per_decade = round(Trend_per_decade, 3),
    R2               = round(R2, 3),
    P_value          = signif(P_value, 3)
  ) %>%
  arrange(Var, Season)


#####Graphs by cycle
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
print(g_temp)


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
print(g_hum)

# 4) Solar (filter early years with poor data, as in your script)
summary_stats_solar <- summary_stats %>%filter(Year >= 1984)
g_solar <- plot_cycle_trend_from_summary(
  summary_stats = summary_stats_solar,
  yvar = "Mean_solar",
  ylab = "Mean Solar Radiation (kWh/m²)",
  title = "Annual Mean Solar Radiation Trends by Sugarcane Production Cycles (1984–2024)",
  subtitle="Consistently high solar radiation during sugarcane growth, contrasting with lower increasing trends in other phases"
)

#print(g_solar)


ggsave(filename = "trend_cycles_temp.png",path=result_folder,plot= g_temp,width = 12, height = 8, dpi = 300)
ggsave(filename = "trend_cycles_rain.png",path=result_folder,plot= g_rain,width = 12, height = 8, dpi = 300)
ggsave(filename = "trend_cycles_humidity.png",path=result_folder,plot= g_hum,width = 12, height = 8, dpi = 300)
ggsave(filename = "trend_cycles_solar.png",path=result_folder,plot= g_solar,width = 12, height = 8, dpi = 300)


#2ND FUNCTION FOR DENSITY PLOTS
plot_cycle_density_with_pooled <- function(            
    df,                                                  # Daily-level data 
    xvar,                                                # column name for x-axis
    title,                                               
    xlab,                                                
    subtitle=NULL,                                            # subtitle
    pooled_df = NULL,                                    # pooled means per Season 
    mean_col  = "Mean",                                  # Name of the column in pooled_df containing the mean
    units = "",                                          
    colors = CYCLE_COLORS,                               
    x_limits = NULL,                                     # Optional x-axis limits c(min, max)-mostly for rainfall
    x_breaks  = NULL                                     # Optional vector of x-axis breaks-ostly for rainfall
){
  
  
  pooled_df <- df %>%                                #compute pooled daily mean per Season from df directly
    dplyr::group_by(Season) %>%
    dplyr::summarise(!!mean_col := mean(.data[[xvar]], na.rm = TRUE),
                     .groups = "drop")
  
  
  density_peaks <- df %>%                              # Compute density peak height per Season for label placement
    dplyr::group_by(Season) %>%
    dplyr::summarise(
      y_peak = {                                       # For each Season, get max density height (for annotation y)
        v <- .data[[xvar]]                             # Extract the vector for this Season
        v <- v[is.finite(v)]                           # Drop non-finite values (NA, Inf)
        if (length(v) > 1) max(stats::density(v)$y) else NA_real_  # Density only valid with >1 data point ; crosschecking whether data is ok or not
      },
      .groups = "drop"
    )
  
  anno <- pooled_df %>%                                # Build annotation frame combining pooled means and peaks
    dplyr::left_join(density_peaks, by = "Season") %>%
    dplyr::mutate(
      y_label = 0.7 * y_peak,                         # Place text slightly below the peak (70% of peak height)
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



# Temperature density 
g_temp_density <- plot_cycle_density_with_pooled(
  df      = df,                                        # Daily data frame with Season and DTemp
  xvar    = "DTemp",                                   # Column to plot on x-axis
  title   = "Distribution of Daily Temperatures by Cycle",   # Title
  subtitle= "Distinct temperature ranges across production phases", # Subtitle
  xlab    = "Daily Temperature (°C)",                  # X-axis label
  units   = "°C"                                       # Unit for mean labels
)
print(g_temp_density)
skewness(Plantation_data$DTemp)
kurtosis(Plantation_data$DTemp)
skewness(Growth_data$DTemp)
kurtosis(Growth_data$DTemp)
skewness(Maturation_data$DTemp)
kurtosis(Maturation_data$DTemp)

# Rainfall density (fix x-axis to 0–5 with 1 mm ticks for clarity)
g_rain_density <- plot_cycle_density_with_pooled(
  df       = df,                                       # Daily data frame with Season and DRain
  xvar     = "DRain",                                  # Column to plot on x-axis
  title    = "Distribution of Daily Rainfall by Season (1981–2024)", # Title
  subtitle = "Right-skewed distributions: many low-rain days with occasional heavy events.", # Subtitle
  xlab     = "Rainfall (mm)",                          # X-axis label
  units    = " mm",                                    # Units appended to mean labels
  x_limits = c(0, 5),                                  # Fix axis to 0–5 mm
  x_breaks = seq(0, 15, by = 1)                        # Show ticks every 1 mm 
)
print(g_rain_density)
skewness(Plantation_data$DRain)
kurtosis(Plantation_data$DRain)
skewness(Growth_data$DRain)
kurtosis(Growth_data$DRain)
skewness(Maturation_data$DRain)
kurtosis(Maturation_data$DRain)

#Humidity Density Plot 

# Humidity density 
g_humidity_density <- plot_cycle_density_with_pooled(
  df      = df,                                        # Daily data frame with Season and Humidity
  xvar    = "Humidity",                                   # Column to plot on x-axis
  title   = "Distribution of Daily Humidity by Cycle",   # Title
  subtitle= "Distinct humidity ranges across sugarcane phases", # Subtitle
  xlab    = "Daily Humidity (%)",                  # X-axis label
  units   = "%",                                       # Unit for mean labels
  x_limits = c(55, 94)   #for readability 
)
print(g_humidity_density)
skewness(Plantation_data$Humidity)
kurtosis(Plantation_data$Humidity)
skewness(Growth_data$Humidity)
kurtosis(Growth_data$Humidity)
skewness(Maturation_data$Humidity)
kurtosis(Maturation_data$Humidity)

# Solar rdiation
# Filter from 1984+ 
df_solar84 <- df %>%filter(Year >= 1984)

# Solar radiation density (1984–2024 only)
g_solar_density <- plot_cycle_density_with_pooled(
  df      = df_solar84,                                  
  xvar    = "Solar",
  title   = "Daily Solar Radiation Distribution by Cycle (1984–2024)",
  subtitle= "High-Intensity Solar Profiles During Growth, Contrasting with Near-Normal and Flatter Distributions in Other Phases.",
  xlab    = "Daily Solar Radiation (kWh/m²)",
  units   = " kWh/m²"
)
print(g_solar_density)
#skewness and kurtosis >=1984 year
Plantation84 <- Plantation_data %>% filter(Year >= 1984)
Growth84     <- Growth_data %>% filter(Year >= 1984)
Maturation84 <- Maturation_data %>% filter(Year >= 1984)
skewness(Plantation84$Solar)
kurtosis(Plantation84$Solar)
skewness(Growth84$Solar)
kurtosis(Growth84$Solar)
skewness(Maturation84$Solar)
kurtosis(Maturation84$Solar)


ggsave(filename = "density_temperature.png",path=result_folder,plot= g_temp_density,width = 12, height = 8, dpi = 300)
ggsave(filename = "density_rainfall.png",path=result_folder,plot= g_rain_density,width = 12, height = 8, dpi = 300)
ggsave(filename = "density_humidity.png",path=result_folder,plot= g_humidity_density,width = 12, height = 8, dpi = 300)
ggsave(filename = "density_solar.png",path=result_folder,plot= g_solar_density,width = 12, height = 8, dpi = 300)




#Calculating trends per cycles for the 1981-1998,1999-2010 and 2011-2024

# ===================== SPLIT-PERIOD TRENDS BY CYCLE FUNCTION =====================

# ---- 1) Fit OLS slope + 95% CI for a given Season × period ---------------------------------------
# This function takes a data frame with columns Year and a numeric column (named by y_col)
# and returns the slope per year, standard error, t-stat, p-value, 95% CI, and R2.
fit_with_ci <- function(df_in, y_col) {
  # Keep only the rows where Year and the response column are finite numbers (drop NA/Inf)
  df <- df_in %>%
    filter(is.finite(.data[[y_col]]), is.finite(Year))
  
  # If there are fewer than 3 observations, regression isn’t meaningful
  # NOTE: `3L` is just the integer literal 3 (same as 3); the L suffix marks it as integer in R;had to add otherwise code crashes
  if (nrow(df) < 3L) {
    # Return a single-row tibble filled with NAs (but keep period bounds and sample size)
    return(tibble(
      n = nrow(df),                                    # number of observations used
      Year_start = min(df$Year, na.rm = TRUE),         # first year seen
      Year_end   = max(df$Year, na.rm = TRUE),         # last year seen
      Slope_per_year = NA_real_,                       # slope unavailable
      SE = NA_real_,                                   # standard error unavailable
      t_stat = NA_real_,                               # t-stat unavailable
      p_value = NA_real_,                              # p-value unavailable
      CI_low_95 = NA_real_,                            # 95% CI lower bound unavailable
      CI_high_95 = NA_real_,                           # 95% CI upper bound unavailable
      R2 = NA_real_                                    # R-squared unavailable
    ))
  }
  
  # Fit an ordinary least squares (OLS) regression: response and Year
  mod <- lm(df[[y_col]] ~ Year, data = df)
  
  # Extract the slope row (coefficient of Year) as a tidy data frame
  slope_row <- tidy(mod) %>% filter(term == "Year")
  
  # Compute the 95% confidence interval for the Year coefficient
  ci <- confint(mod, parm = "Year", level = 0.95)
  
  # Return a tidy one-row summary tibble with all key statistics
  tibble(
    n            = nrow(df),                           # sample size used in regression
    Year_start   = min(df$Year, na.rm = TRUE),         # earliest year in the period
    Year_end     = max(df$Year, na.rm = TRUE),         # latest year in the period
    Slope_per_year = slope_row$estimate,               # slope per calendar year
    SE           = slope_row$std.error,                # standard error of slope
    t_stat       = slope_row$statistic,                # t-statistic for the slope
    p_value      = slope_row$p.value,                  # p-value for the slope
    CI_low_95    = as.numeric(ci[1]),                  # lower bound of 95% CI
    CI_high_95   = as.numeric(ci[2]),                  # upper bound of 95% CI
    R2           = summary(mod)$r.squared              # coefficient of determination
  )
}

# ---- 2) Build annual means per Season for any variable in df -------------------------------
# This function aggregates a daily variable (e.g., "Solar", "DTemp") by Season and Year using mean().

build_seasonal_annual <- function(df, value_col, agg_fun = mean) {
  # Group by Season and Year to form annual aggregates per cycle
  df %>%
    group_by(Season, Year) %>%                         # define season-year groups
    summarise(
      !!value_col := agg_fun(.data[[value_col]], na.rm = TRUE),  # compute annual mean 
      .groups = "drop"                                  # return an ungrouped data frame
    )
}

# ---- 3) Fit trends per Season for a given period (start–end) -------------------------------------
# This function filters to a year range, splits by Season, and applies fit_with_ci().
fit_season_period <- function(seasonal_tbl, value_col, y_start, y_end) {
  # Filter to the requested period window (inclusive)
  seasonal_tbl %>%
    filter(Year >= y_start, Year <= y_end) %>%         # keep years in [y_start, y_end]
    group_by(Season) %>%                               # analyse each cycle separately
    group_modify(~ fit_with_ci(.x, y_col = value_col)) %>%  # run regression within each cycle
    ungroup() %>%                                      # drop grouping for clean output
    mutate(
      Variable = value_col,                            # record which variable we modeled
      Period   = sprintf("%d–%d (Year)", y_start, y_end)  # carry period label for clarity
    ) %>%
    select(Variable, Period, Season, everything())     # put key columns first
}

# ---- 4) Compute and combine multiple periods ----------------------------------
# Pass a list of periods like list(c(1981, 1998),c(1999,2010)).

compute_trends_split <- function(seasonal_tbl, value_col, periods) {
  # Apply fit_season_period() to each (start, end) pair in periods
  
  results <- lapply(      #passing each period elemnets to the function
    periods,
    function(pe) fit_season_period(seasonal_tbl, value_col, y_start = pe[1], y_end = pe[2])
  )
  
  # Bind results and format numbers for neat reporting
  bind_rows(results) %>%                               # stack all period results
    mutate(
      Slope_per_year = round(Slope_per_year, 6),       # round slopes to 6 decimals
      CI_low_95      = round(CI_low_95, 6),            # round CI lower bound
      CI_high_95     = round(CI_high_95, 6),           # round CI upper bound
      R2             = round(R2, 3),                   # round R-squared
      p_value        = signif(p_value, 3)              # p-value in better format
    ) %>%
    arrange(Variable, Season, Period)                  # order rows for easy reading
}

# ============================= EXAMPLES (using your objects) ======================================
# Your daily df already has columns: Season, Year, Solar, DTemp, etc.

# Define the two split periods you want to compare
periods_split <- list(c(1981, 1998),c(1999,2010), c(2011, 2024))    

# ---- A) SOLAR trends per cycle across the periods --------------------------------------------
seasonal_solar <- build_seasonal_annual(df, value_col = "Solar")              # annual Solar by Season
solar_trends_split <- compute_trends_split(seasonal_solar, "Solar", periods_split)  # regressions per period                                                     
print(solar_trends_split)   

# ---- B) TEMPERATURE trends per cycle across the periods --------------------------------------
seasonal_temp <- build_seasonal_annual(df, value_col = "DTemp")               # annual Temp by Season
temp_trends_split <- compute_trends_split(seasonal_temp, "DTemp", periods_split)    # regressions per period                                                      
print(temp_trends_split)    

# ---- C) Rainfall trends per cycle across the periods --------------------------------------
seasonal_rain <- build_seasonal_annual(df, value_col = "DRain")               # annual rain by Season
rain_trends_split <- compute_trends_split(seasonal_rain, "DRain", periods_split)    # regressions per period                                                      
print(rain_trends_split)  

# ---- D) Humidity trends per cycle across the periods --------------------------------------
seasonal_hum <- build_seasonal_annual(df, value_col = "Humidity")               # humidity by Season
humid_trends_split <- compute_trends_split(seasonal_hum, "Humidity", periods_split)    # regressions per period                                                      
print(humid_trends_split)  



cat("\n NASA POWER trend and seasonal analysis completed successfully!\n")
#======================-----------END OF NASA SCRIPT---------======================#
