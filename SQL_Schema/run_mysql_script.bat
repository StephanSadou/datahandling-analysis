@echo off
REM ====== EDIT THESE 4 LINES ======
set "MYSQL_BIN=C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe"
set "DB_HOST=127.0.0.1"
set "DB_PORT=3306"
set "DB_NAME=data_handling"
REM Pass full path to your SQL file as the first argument to the .bat
set "SQL_FILE=%~1"
REM ================================

if "%SQL_FILE%"=="" (
  echo Usage: %~nx0 "C:\path\to\script.sql"
  exit /b 1
)

if not exist "%MYSQL_BIN%" (
  echo mysql.exe not found at: %MYSQL_BIN%
  echo Tip: it may be here: C:\xampp\mysql\bin\mysql.exe  or  C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe
  exit /b 1
)

if not exist "%SQL_FILE%" (
  echo SQL file not found: %SQL_FILE%
  exit /b 1
)

echo Running %SQL_FILE% on %DB_NAME%...
"%MYSQL_BIN%" -h "%DB_HOST%" -P %DB_PORT% -u root -p ^
  --default-character-set=utf8mb4 ^
  "%DB_NAME%" < "%SQL_FILE%"
if errorlevel 1 (
  echo ❌ Execution failed.
  exit /b 1
) else (
  echo ✅ Done.
)
