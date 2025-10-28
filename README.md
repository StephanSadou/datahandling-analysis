# 🌾 STAT5129 – Data Handling & Analysis  
### Automated ELT Pipeline in R for Mauritius Climate–Agriculture–Economy Case Study

---

## 📘 Introduction

This repository was developed as part of **STAT5129: Data Handling & Analysis (MSc Data Analytics, University of Mauritius)** — the capstone coursework assessing the ability to **collect, clean, store, analyse, and present data using R and SQL**, following good data governance and reproducibility practices.

The project focuses on the **Mauritius Case Study**, which investigates how **climate conditions affect electricity generation and/or agricultural production in Mauritius**, optionally supported by **socio-economic indicators** such as GDP trends.

The repository implements a fully automated **ELT (Extract → Load → Transform)** pipeline that:
- Retrieves data programmatically from open-data APIs (NASA POWER, FAOSTAT, World Bank WDI).
- Loads cleaned datasets into structured CSV files and MySQL relational tables.
- Transforms and analyses the data through SQL views and R scripts to generate descriptive, associative, and predictive insights.

A separate **NYC Taxi Skills Demo** (Part B) using the `arrow` package demonstrates handling of Parquet files.

---

## ⚙️ Prerequisites

Before running the pipeline, please ensure the following software is installed.

### Required
- [R (latest version)](https://cran.r-project.org/)
- [RStudio (latest version)](https://posit.co/download/rstudio/)
- [Rtools](https://cran.r-project.org/bin/windows/Rtools/) *(for Windows users)*
- R GUI *(optional if using RStudio)*
- [MySQL Server (latest version)](https://dev.mysql.com/downloads/mysql/)
- [MySQL Workbench](https://dev.mysql.com/downloads/workbench/)

### Optional
To clone and version-control this repository:
- [Git](https://git-scm.com/downloads)
- [GitHub Desktop](https://desktop.github.com/)

Alternatively, download the repository as a ZIP file and extract it locally.

---

## 🗄️ Database Setup (Important)

### 1. Database Prerequisite

This project integrates directly with a **MySQL database** for structured data storage and analysis.  
Users must ensure that **MySQL Server is properly installed, configured, and running** prior to executing any R scripts.  
Without an active MySQL service, database connection attempts will fail, interrupting the pipeline execution.

### 2. Configure Environment Variables

To allow R to connect automatically to your database, credentials are securely stored in an **`.Renviron`** file located in your project root or R home directory.  
If it doesn’t exist, create it and paste the following content (update as needed):

```bash
DB_HOST = <Database Host> 
DB_USER = <Database User>
DB_PASSWORD = <Database Password>
DB_NAME= "data_handling"

```
| Variable | Description |
|-----------|-------------|
| **DB_HOST** | The address of your database server (e.g., `127.0.0.1` for local). |
| **DB_USER** | The username used to connect to the MySQL database (e.g., `root`). |
| **DB_PASSWORD** | The password for the specified database user account. |
| **DB_NAME** | `"data_handling"` is the name of the database that script will create on MySQL to load the extracted data. **Kindly do not modify this line** |


## 🔁 Automated ELT Pipeline Overview

This repository automates the **Extract–Load–Transform** workflow for Mauritius data analysis.

### 🟢 1. Extract  
Data are automatically requested and downloaded from public APIs:
- **NASA POWER (JSON)** – daily weather metrics (temperature, precipitation).  
- **FAOSTAT (CSV)** – agricultural production and yield data.  
- **World Bank WDI (JSON)** – socio-economic indicators such as GDP per capita.  

Each source is stored in `/data_raw` for full traceability.

### 🟡 2. Load  
Cleaned and standardised datasets are:
- Exported to `/data_stage` as CSV files.  
- Imported into SQL tables using the scripts in the `SQL_Schema` folder. 

This ensures structured data storage ready for querying and integration.

### 🔵 3. Transform  
Transformation and analysis occur in R:
- SQL views combine datasets for descriptive summaries and associative analyses.  
In this phase, the project runs **three analysis scripts** using the data loaded into the MySQL database.  
Each script performs a different type of analysis aligned with the coursework requirements:

- **Descriptive Analysis:** Summarises and visualises the data to reveal key trends and distributions (e.g., monthly or seasonal patterns, averages, standard deviations, boxplots).  
- **Associative / Explanatory Analysis:** Examines relationships between variables, such as correlations or regression models, to explain how climate factors affect electricity generation or crop yields.  
- **Predictive Analysis:** (Optional but rewarded) Builds simple forecasting or regression-based models to predict recent or future values using past observations.

These analyses collectively transform raw data into actionable insights that address the project’s main research question.  
The **final outputs and insights** for each analysis type — including tables, plots, and model summaries — are stored in their corresponding subfolders within the **`/results`** directory.

---

## ▶️ How to Run the Project

Follow these steps to execute the full pipeline.

1. **Open the R Project**  
   - Launch RStudio.  
   - Go to **File → Open Project...**  
   - Open the file **`R.project`** located in the `R/` folder of the cloned or downloaded repository.

2. **Run the Master Script**  
   - Open the script **`00_all.R`**, which orchestrates all pipeline steps.  
   - Select all code (`Ctrl + A`) and run (`Ctrl + Enter`) or click **Run**.

3. **Observe Execution Flow**  
   The script will automatically:  
   - **Extract** → download datasets from APIs.  
   - **Load** → save CSVs and populate SQL tables.  
   - **Transform** → generate views, run analyses, and export results.

4. **Check Outputs**  
   - Processed data and models appear in the `/results` folder.  
   - SQL views and intermediate tables reside in the SQL database.  
   - Diagnostic plots and summaries are reproducible at every run.

---