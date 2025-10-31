DROP DATABASE IF EXISTS Data_Handling;
CREATE DATABASE Data_Handling;
USE Data_Handling;

CREATE TABLE agriculture(
    prod_id SERIAL PRIMARY KEY, 
    Item VARCHAR(100) NOT NULL,
    year Year NOT NULL ,
    Area_harvested DECIMAL(12,2) CHECK(Area_harvested>=0),
    Production DECIMAL(12,2) CHECK(Production>=0),
    Yield DECIMAL(12,2) CHECK(Yield>=0)
);

CREATE TABLE climate (
    date DATE PRIMARY KEY,
    Temperature_in_degree_celsius DECIMAL(5,2) NOT NULL,
    Precipitation_in_mm DECIMAL(6,2) DEFAULT 0 CHECK (Precipitation_in_mm >= 0),
	Humidity_percent DECIMAL(5,2) NOT NULL CHECK (Humidity_percent BETWEEN 0 AND 100),
    Solar_radiation_kWh_m2 DECIMAL(6,2) DEFAULT 0 CHECK (Solar_radiation_kWh_m2 >= 0)
);

CREATE TABLE economic_indicator(
    Year YEAR NOT NULL,    
    Countryiso3code VARCHAR(10) NOT NULL,
    value DECIMAL(12,4) NOT NULL CHECK (value >= 0), 
	PRIMARY KEY (Year, Countryiso3code)
);

#select count(*) from climate
