#----------CREATE VIEW, JOIN TABLES & CREATE NEW FIELDS TO BE USED FOR ANALYSIS PURPOSES-----------
USE data_handling;
DROP VIEW compiled_data;
CREATE VIEW compiled_data AS
SELECT 
    cl.*,ag.Year as Harvest_Year,
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
      SUM(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Precipitation_in_mm END) AS Total_Rainfall_Maturation,

      # Rainy Days Grouping
      SUM(CASE WHEN (MONTH(date) BETWEEN 7 AND 9 AND Precipitation_in_mm > 1) THEN 1 ELSE 0 END) AS Total_rainy_days_Plantation,
      SUM(CASE WHEN ((MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3) AND Precipitation_in_mm > 1) THEN 1 ELSE 0 END) AS Total_rainy_days_Growth,
      SUM(CASE WHEN (MONTH(date) BETWEEN 4 AND 6 AND Precipitation_in_mm > 1) THEN 1 ELSE 0 END) AS Total_rainy_days_Maturation

    FROM climate  
    GROUP BY Climate_Year
) cl
ON ag.Year = cl.Climate_Year + 1   # Align climate from previous year with harvest year
JOIN economic_indicator ei 
ON ei.Year = ag.Year;