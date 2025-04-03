library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(ggplot2)
library(cluster)
library(lubridate)
library(tibble)
library(sf)

set.seed(5188)

df <- read_csv("../../Data/Final/Imputation/changi_imp_final.csv")

# Function to convert date string to date object
convert_period_to_date <- function(period_str) {
  parts <- unlist(strsplit(period_str, " "))
  period <- parts[1]
  year_val <- parts[2]
  months <- unlist(strsplit(period, "-"))
  month_num <- match(months[1], month.abb)
  date_str <- paste0(year_val, "-", sprintf("%02d", month_num), "-01")
  as.Date(date_str)
}

df$Date <- as.Date(sapply(df$Date, convert_period_to_date))
head(df)

### Helper Functions ###

# Get date index for data (MUST APPLY THIS BEFORE ANYTHING ELSE)
get_date_index <- function(df) {
  # Extract Year and Month directly from Date
  df <- df %>%
    mutate(Year = year(Date),
           Month = month(Date)) %>%
    mutate(period = paste(Month, Year, sep = "-"))  # "Month-Year" format

  # Generate the date index for each period (Month-Year combinations)
  years <- 2000:2024
  months <- 1:12
  date_periods <- expand.grid(Month = months, Year = years)
  date_periods <- date_periods %>%
    mutate(period = paste(Month, Year, sep = "-")) %>%
    arrange(Year, Month)  # Ensure periods are sorted correctly
  
  # Create a look-up table mapping period to date_index (starting at 1)
  period_numbers <- seq_along(date_periods$period)
  lookup_table <- data.frame(period = date_periods$period, date_index = period_numbers)
  
  # Left join to add the date index to the dataframe
  df <- df %>%
    left_join(lookup_table, by = "period") %>%
    select(-Year, -Month)  # Drop Year and Month columns as they're no longer needed
  
  return(df)
}

# Function to create rolling windows with a fixed window size (specified by user)
get_data_by_window <- function(df, window_size) {
  min_max <- get_date_range(df)
  start_values <- seq(min_max[1], min_max[2] - window_size, 1)  # Start from the minimum index, and ensure a window of fixed size
  end_values <- start_values + window_size
  data_lst <- list()
  
  for (i in 1:length(start_values)) {
    data_lst[[i]] <- df %>%
      filter(date_index >= start_values[i], date_index <= end_values[i])  # Get the data for the current window
  }
  
  return(data_lst)
}

# Function to get the date range from the dataset
get_date_range <- function(df) {
  return(c(min(df$date_index), max(df$date_index)))
}

# Apply the get_date_index function to assign the date index
df <- get_date_index(df)

# Now apply the rolling window function with a specified window size (e.g., 9 years)
window_size <- 10  # Specify the window size (can be any value between 9-13 or whatever you prefer)
rolling_windows <- get_data_by_window(df, window_size)

# Optionally, inspect the first few windows
head(rolling_windows)