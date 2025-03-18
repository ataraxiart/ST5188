#Changi
library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(urca)
library(ggplot2)
library(cluster)
library(lubridate)

train_C <- read_csv("../../Data/Final/TT Split/changi_train_long.csv")
test_C <- read_csv("../../Data/Final/TT Split/changi_test_long.csv")


# Create a Unique Point Identifier
train_Changi <- train_C |>
  mutate(point_id = paste(x, y, sep = "_"))

test_Changi <- test_C |>
  mutate(point_id = paste(x, y, sep = "_"))

# Sample Discrete Points from the Training Data
# Get the unique point IDs for the training data
unique_points <- train_Changi |>
  select(point_id, x, y) |>
  distinct()

# Decide how many points to sample
n_points <- 10

# Perform K-means clustering to get well-distributed points
set.seed(5188) 
clusters <- kmeans(unique_points[, c("x", "y")], centers = n_points)

# Assign cluster labels to unique points
unique_points$cluster <- clusters$cluster

# Select one random point from each cluster
sampled_points <- unique_points |>
  group_by(cluster) |>
  sample_n(1) |>
  pull(point_id)

cat("Sampled point_id values for Changi:", sampled_points, "\n")


# Function to convert date string to date object
convert_period_to_date <- function(period_str) {
  # Expected format: "Jan-Feb 2020"
  # Split the string by space to separate the period and the year:
  parts <- unlist(strsplit(period_str, " "))
  if (length(parts) < 2) {
    stop("Unrecognized period format: ", period_str)
  }
  period <- parts[1]   # e.g., "Jan-Feb"
  year_val <- parts[2] # e.g., "2020"
  
  # Split the period into two month parts and choose the first one
  months <- unlist(strsplit(period, "-"))
  month_abbr <- months[1]  # "Jan"
  
  # Convert the month abbreviation to its numeric value:
  month_num <- match(month_abbr, month.abb)
  
  if(is.na(month_num)) {
    stop("Invalid month abbreviation in: ", period_str)
  }
  
  # Construct a date string using the first day of that month.
  # For "Jan-Feb 2020", we create "2020-01-01"
  date_str <- paste0(year_val, "-", sprintf("%02d", month_num), "-01")
  
  # Convert to Date object
  as.Date(date_str)
}

# Test the function on your sample:
convert_period_to_date("Jan-Feb 2020")
# Should return: "2020-01-01"

# Convert the Date column for the training dataset:
train_Changi$Date <- sapply(train_Changi$Date, convert_period_to_date)

# Similarly, convert the Date column for the test dataset:
test_Changi$Date <- sapply(test_Changi$Date, convert_period_to_date)

# Verify the conversion:
head(train_Changi$Date)
head(test_Changi$Date)

write.csv(train_Changi, file = "latest_train_Changi.csv", row.names = FALSE)
write.csv(test_Changi, file = "latest_test_Changi.csv", row.names = FALSE)


# Build unoptimized ARIMA Models for Each Sampled Point
# 1. Training and storing the ARIMA models for each sampled point.
arima_models <- list()

for (point in sampled_points) {
  
  # Filter the dataset for the current sampled point
  point_data <- train_Changi |> 
    filter(point_id == point) |> 
    arrange(Date)  # Ensure data is ordered by Date
  
  # Create a time series object
  ts_data <- ts(ts_train_data$Value,
                             start = c(year(as.Date(min(ts_train_data$Date), origin = "1970-01-01")),
                                       month(as.Date(min(ts_train_data$Date), origin = "1970-01-01"))),
                             frequency = 6)
  
  # Fit an unoptimized ARIMA model (using default settings)
  model <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE, seasonal = TRUE)
  
  # Store the model in the list
  arima_models[[point]] <- model
  
  # Print model summary
  cat("\nARIMA Model for Point:", point, "\n")
  print(summary(model))
}

# 2. Forecast and Compare with Test Data 
forecast_results <- list()

for (point in sampled_points) {
  
  # Filter the test data for the current sampled points
  test_point_data <- test_Changi |> 
    filter(point_id == point) |> 
    arrange(Date)  
  
  # Create a time series object for the test data
  ts_test_data <- ts(test_point_data$Value,
                     start = c(year(as.Date(min(test_point_data$Date), origin = "1970-01-01")),
                               month(as.Date(min(test_point_data$Date), origin = "1970-01-01"))),
                     frequency = 6)  
  
  # Get the corresponding ARIMA model from the list
  model <- arima_models[[point]]
  
  # Forecast using the ARIMA model (set the number of periods to forecast)
  forecast_horizon <- length(ts_test_data)  # This is the length of the test data
  forecast_values <- forecast(model, h = forecast_horizon)
  
  # Store the forecast results in the list
  forecast_results[[point]] <- forecast_values
  
  # Print the forecast for the current point
  cat("\nForecast for Point:", point, "\n")
  print(forecast_values)
  
  # Compare forecast with actual test data
  comparison <- data.frame(
    Date = test_point_data$Date,
    Actual = test_point_data$Value,
    Forecast = forecast_values$mean,
    Lower = forecast_values$lower[,2],  # 95% confidence interval lower bound
    Upper = forecast_values$upper[,2]   # 95% confidence interval upper bound
  )
  
  # Print the comparison for each point
  cat("\nComparison for Point:", point, "\n")
  print(comparison)
}






