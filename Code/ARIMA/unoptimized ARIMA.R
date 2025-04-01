process_neighborhood <- function(neighborhood, train_file, test_file, n_points = 30, seed = 5188) {
  library(dplyr)
  library(readr)
  library(tidyr)
  library(forecast)
  library(urca)
  library(ggplot2)
  library(cluster)
  library(lubridate)
  
  cat("\nProcessing Neighborhood:", neighborhood, "\n")
  
  # Load the training and test datasets
  train_data <- read_csv(train_file)
  test_data <- read_csv(test_file)
  
  # Create a Unique Point Identifier
  train_data <- train_data |> 
    mutate(point_id = paste(x, y, sep = "_"))
  test_data <- test_data |> 
    mutate(point_id = paste(x, y, sep = "_"))
  
  # Sample Discrete Points from the Training Data
  unique_points <- train_data |> 
    select(point_id, x, y) |> 
    distinct()
  
  # Perform K-means clustering to get well-distributed points
  set.seed(seed)
  clusters <- kmeans(unique_points[, c("x", "y")], centers = n_points)
  unique_points$cluster <- clusters$cluster
  sampled_points <- unique_points |> 
    group_by(cluster) |> 
    sample_n(1) |> 
    pull(point_id)
  
  cat("\nSampled point_id values for", neighborhood, ":", sampled_points, "\n")
  
  # Convert Period Strings to Date
  convert_period_to_date <- function(period_str) {
    parts <- unlist(strsplit(period_str, " "))
    if (length(parts) < 2) stop("Unrecognized period format: ", period_str)
    period <- parts[1]
    year_val <- parts[2]
    months <- unlist(strsplit(period, "-"))
    month_abbr <- months[1]
    month_num <- match(month_abbr, month.abb)
    if (is.na(month_num)) stop("Invalid month abbreviation in: ", period_str)
    date_str <- paste0(year_val, "-", sprintf("%02d", month_num), "-01")
    as.Date(date_str)
  }
  
  train_data$Date <- as.Date(sapply(train_data$Date, convert_period_to_date))
  test_data$Date <- as.Date(sapply(test_data$Date, convert_period_to_date))
  
  # Build ARIMA Models for Sampled Points
  arima_models <- list()
  for (point in sampled_points) {
    point_data <- train_data |> 
      filter(point_id == point) |> 
      arrange(Date)
    ts_data <- ts(point_data$Value,
                  start = c(year(min(point_data$Date)),
                            month(min(point_data$Date))),
                  frequency = 6)
    model <- arima(ts_data, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 1), period = 6))
    arima_models[[point]] <- model
    cat("\nARIMA Model for Point:", point, "\n")
    print(summary(model))
  }
  
  # Forecast and Compare with Test Data
  forecast_results <- list()
  sampled_test_data <- test_data |> 
    filter(point_id %in% sampled_points)
  for (point in sampled_points) {
    test_point_data <- sampled_test_data |> 
      filter(point_id == point) |> 
      arrange(Date)
    ts_test_data <- ts(test_point_data$Value,
                       start = c(year(min(test_point_data$Date)),
                                 month(min(test_point_data$Date))),
                       frequency = 6)
    model <- arima_models[[point]]
    forecast_horizon <- length(ts_test_data)
    forecast_values <- forecast(model, h = forecast_horizon)
    forecast_results[[point]] <- forecast_values
    comparison <- data.frame(
      Date = test_point_data$Date,
      Actual = test_point_data$Value,
      Forecast = forecast_values$mean,
      Lower = forecast_values$lower[,2],
      Upper = forecast_values$upper[,2]
    )
    cat("\nForecast for Point:", point, "\n")
    print(comparison)
  }
  
  return(forecast_results)
}

# Changi
forecast_results_changi <- process_neighborhood(
  neighborhood = "Changi",
  train_file = "../../Data/Final/Imputation/imp_train_set.csv",
  test_file = "../../Data/Final/Imputation/imp_test_set.csv"
)

