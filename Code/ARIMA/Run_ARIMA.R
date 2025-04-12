# Load libraries for ARIMA modeling
library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(urca)
library(ggplot2)
library(cluster)
library(lubridate)
library(here)
library(tibble)

# Run baseline models
source(here("Code/ARIMA/unoptimized ARIMA.R"))
source(here("Code/ARIMA/optimized ARIMA.R"))
source(here("Code/ARIMA/Optimized ARIMA with RMSE.R"))
source(here("Code/ARIMA/Rolling window and RMSE.R"))
source(here("Code/ARIMA/Changi imputed and restructured.R"))