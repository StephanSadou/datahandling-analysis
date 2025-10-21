#----------CREATE VIEW, JOIN TABLES & CREATE NEW FIELDS TO BE USED FOR ANALYSIS PURPOSES-----------
USE data_handling;

#--------CREATING VIEW 1 with same harvest and climate year---------
DROP VIEW IF EXISTS  current_compiled_data; #remove already existing view
CREATE VIEW current_compiled_data AS
SELECT 
    ag.Year as Harvest_Year,
    cl.*,
    Area_harvested,
    Production,
	Yield,
    ei.value as GDP_Value
FROM agriculture ag

JOIN (
    SELECT
      YEAR(date) as Climate_Year ,  

      # Temperature Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Plantation,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Growth,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Maturation,

      # Humidity Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Humidity_percent END) AS Avg_Humidity_Plantation,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Humidity_percent END) AS Avg_Humidity_Growth,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Humidity_percent END) AS Avg_Humidity_Maturation,

      # Solar Radiation Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Plantation,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Growth,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Maturation,

      # Total Rainfall Grouping
      SUM(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Precipitation_in_mm END) AS Total_Rainfall_Plantation,
      SUM(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Precipitation_in_mm END) AS Total_Rainfall_Growth,
      SUM(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Precipitation_in_mm END) AS Total_Rainfall_Maturation

    FROM climate  
    GROUP BY Climate_Year
) cl
ON ag.Year = cl.Climate_Year  # Align climate from current year with current harvest year
JOIN economic_indicator ei 
ON ei.Year = ag.Year;

select count(*) From current_compiled_data ;#:- Crosscheck linecount:- should be same as FAOSTAT Data

#View 2. For harvest year data with prior year climate data only
CREATE VIEW lagged_data AS
SELECT 
ag.Year as Harvest_Year,
    cl.*,
    Area_harvested,
    Production,
	Yield,
    ei.value as Agri_GDP_Share

FROM agriculture ag

JOIN (
    SELECT
      YEAR(date) as Climate_Year ,  

      # Temperature Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Plantation_lag1,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Growth_lag1,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Maturation_lag1,

      # Humidity Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Humidity_percent END) AS Avg_Humidity_Plantation_lag1,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Humidity_percent END) AS Avg_Humidity_Growth_lag1,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Humidity_percent END) AS Avg_Humidity_Maturation_lag1,

      # Solar Radiation Grouping
      AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Plantation_lag1,
      AVG(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Growth_lag1,
      AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Maturation_lag1,

      # Total Rainfall Grouping
      SUM(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Precipitation_in_mm END) AS Total_Rainfall_Plantation_lag1,
      SUM(CASE WHEN (MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) THEN Precipitation_in_mm END) AS Total_Rainfall_Growth_lag1,
      SUM(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Precipitation_in_mm END) AS Total_Rainfall_Maturation_lag1

    FROM climate  
    GROUP BY Climate_Year
) cl
ON ag.Year = cl.Climate_Year + 1   # Align climate from previous year with harvest year
JOIN economic_indicator ei 
ON ei.Year = ag.Year;

select count(*) From lagged_data ;#:- Crosscheck linecount:- should be 1 less than linecount for FAOSTAT Data

#--------CREATING VIEW 2 to combine lagged climate data and current year data together with gdp and yield information---------
DROP VIEW IF EXISTS  lagged_compiled_data;
CREATE VIEW lagged_compiled_data AS
SELECT 
 x.*,
 y.Avg_Temperature_Plantation,
 y.Avg_Temperature_Growth,
 y.Avg_Temperature_Maturation,
 y.Avg_Humidity_Plantation,
 y.Avg_Humidity_Growth,
 y.Avg_Humidity_Maturation,
 y.Solar_Radiation_Plantation,
 y.Solar_Radiation_Maturation,
 y.Solar_Radiation_Growth,
 y.Total_Rainfall_Plantation,
 y.Total_Rainfall_Maturation,
 y.Total_Rainfall_Growth
FROM lagged_data x
JOIN current_compiled_data y ON x.Harvest_Year=y.Harvest_Year
