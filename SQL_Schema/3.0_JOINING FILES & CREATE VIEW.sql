#----------CREATE VIEW, JOIN TABLES & CREATE NEW FIELDS TO BE USED FOR ANALYSIS PURPOSES-----------
USE data_handling;
CREATE VIEW compiled_data AS
SELECT 
    cl.*,
    Area_harvested,
    Production,
	Yield,
    ei.value as GDP_Value
FROM agriculture ag
JOIN
(SELECT
  YEAR(date) AS Year,

 #Grouping Temperature (°C) in Plantation,Growth and Maturation Phase
  AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Plantation,
  AVG(CASE WHEN MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Growth,
  AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Temperature_in_degree_celsius END) AS Avg_Temperature_Maturation,

  #Grouping Humidity (%) in Plantation,Growth and Maturation Phase
  AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Humidity_percent END) AS Avg_Humidity_Plantation,
  AVG(CASE WHEN MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3 THEN Humidity_percent END) AS Avg_Humidity_Growth,
  AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Humidity_percent END) AS Avg_Humidity_Maturation,

 #Grouping Solar Radiation (kWh/m²) in Plantation,Growth and Maturation Phase
  AVG(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Plantation,
  AVG(CASE WHEN MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Growth,
  AVG(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Solar_radiation_kWh_m2 END) AS Solar_Radiation_Maturation,

 #Grouping total Rainfall (mm) in Plantation,Growth and Maturation Phase
  SUM(CASE WHEN MONTH(date) BETWEEN 7 AND 9 THEN Precipitation_in_mm END) AS Total_Rainfall_Plantation,
  SUM(CASE WHEN MONTH(date) BETWEEN 10 AND 12 OR MONTH(date) BETWEEN 1 AND 3 THEN Precipitation_in_mm END) AS Total_Rainfall_Growth,
  SUM(CASE WHEN MONTH(date) BETWEEN 4 AND 6 THEN Precipitation_in_mm END) AS Total_Rainfall_Maturation,

#Rainy Days computation
  COUNT(CASE WHEN Precipitation_in_mm > 0 THEN 1 END) AS rainy_days

FROM climate   
GROUP BY YEAR(date)
ORDER BY Year) cl
ON cl.Year = ag.year
JOIN economic_indicator ei ON ei.Year=cl.year;


#Visualise fields in VIEW
select *
from compiled_data