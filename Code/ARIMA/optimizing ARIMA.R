#Jurong West

library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(urca)
library(ggplot2)
library(cluster)
library(lubridate)

train_JW <- read_csv("../../Data/Final/JW Split/train_set.csv")
test_JW <- read_csv("../../Data/Final/JW Split/test_set.csv")

# Create a Unique Point Identifier
train_JurongWEST <- train_JW %>%
  mutate(point_id = paste(x, y, sep = "_"))

test_JurongWest <- test_JW %>%
  mutate(point_id = paste(x, y, sep = "_"))

# Sample Discrete Points from the Training Data
unique_points <- train_JurongWEST %>%
  select(point_id, x, y) %>%
  distinct()

n_points <- 30
set.seed(5188) 
clusters <- kmeans(unique_points[, c("x", "y")], centers = n_points)
unique_points$cluster <- clusters$cluster

sampled_points <- unique_points %>%
  group_by(cluster) %>%
  sample_n(1) %>%
  pull(point_id)

cat("Sampled point_id values for Jurong West:", sampled_points, "\n")

# Function to convert date string to date object
convert_period_to_date <- function(period_str) {
  parts <- unlist(strsplit(period_str, " "))
  if (length(parts) < 2) {
    stop("Unrecognized period format: ", period_str)
  }
  period <- parts[1]
  year_val <- parts[2]
  
  months <- unlist(strsplit(period, "-"))
  month_abbr <- months[1]
  
  month_num <- match(month_abbr, month.abb)
  
  if (is.na(month_num)) {
    stop("Invalid month abbreviation in: ", period_str)
  }
  
  date_str <- paste0(year_val, "-", sprintf("%02d", month_num), "-01")
  
  as.Date(date_str)
}

train_JurongWEST$Date <- as.Date(sapply(train_JurongWEST$Date, convert_period_to_date))
test_JurongWest$Date <- as.Date(sapply(test_JurongWest$Date, convert_period_to_date))

# Grid search for ARIMA parameters and calculating RMSE
arima_models <- list()
rmse_values <- data.frame(Point = character(), RMSE = numeric())

# Define a function to calculate RMSE
calculate_rmse <- function(actual, forecasted) {
  sqrt(mean((actual - forecasted)^2))
}

# Iterate over each point to optimize ARIMA parameters using grid search
for (point in sampled_points) {
  
  point_data <- train_JurongWEST %>%
    filter(point_id == point) %>%
    arrange(Date)
  
  ts_data <- ts(point_data$Value,
                start = c(year(min(point_data$Date)), month(min(point_data$Date))),
                frequency = 6)
   
  best_rmse <- Inf
  best_order <- c(0, 1, 1)  # Default ARIMA order
  
  # Grid Search for ARIMA Parameters
  p_values <- 0:2
  d_values <- 0:1
  q_values <- 0:2
  seasonal_p_values <- 0:1
  seasonal_d_values <- 0:1
  seasonal_q_values <- 0:1
  
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        for (sp in seasonal_p_values) {
          for (sd in seasonal_d_values) {
            for (sq in seasonal_q_values) {
              # Fit the ARIMA model with the grid parameters
              model <- tryCatch({
                arima(ts_data, order = c(p, d, q), seasonal = list(order = c(sp, sd, sq), period = 6))
              }, error = function(e) NULL)
              
              if (!is.null(model)) {
                # Forecast using the model
                forecast_horizon <- length(ts_data)
                forecast_values <- forecast(model, h = forecast_horizon)
                
                # Calculate RMSE for the model
                rmse <- calculate_rmse(point_data$Value, forecast_values$mean)
                
                if (rmse < best_rmse) {
                  best_rmse <- rmse
                  best_order <- c(p, d, q, sp, sd, sq)
                  arima_models[[point]] <- model
                }
              }
            }
          }
        }
      }
    }
  }
  
  rmse_values <- rbind(rmse_values, data.frame(Point = point, RMSE = best_rmse))
  cat("Best ARIMA Order for Point", point, ": ", best_order, "\n")
}

# Print RMSE values
print(rmse_values)

# Forecast and Compare with Test Data for the best ARIMA models
all_comparisons <- list()
for (point in sampled_points) {
  
  test_point_data <- test_JurongWest %>%
    filter(point_id == point) %>%
    arrange(Date)
  
  ts_test_data <- ts(test_point_data$Value,
                     start = c(year(min(test_point_data$Date)), month(min(test_point_data$Date))),
                     frequency = 6)
  
  model <- arima_models[[point]]
  
  forecast_horizon <- length(ts_test_data)
  forecast_values <- forecast(model, h = forecast_horizon)
  
  # Compare forecast with actual test data
  comparison <- data.frame(
    Date = test_point_data$Date,
    Actual = test_point_data$Value,
    Forecast = forecast_values$mean,
    Lower = forecast_values$lower[,2],  
    Upper = forecast_values$upper[,2]
  )
  
  cat("\nComparison for Point:", point, "\n")
  print(comparison)
  
  # Add comparison data to the list for facet wrap plotting
  comparison$Point <- point
  all_comparisons[[point]] <- comparison
}

# Combine all comparison data into a single data frame
comparison_data <- bind_rows(all_comparisons)

# Define a data frame to store RMSE for test data
test_rmse_values <- data.frame(Point = character(), RMSE = numeric())

# Loop for comparing forecast with test data
for (point in sampled_points) {
  
  test_point_data <- test_JurongWest %>%
    filter(point_id == point) %>%
    arrange(Date)
  
  ts_test_data <- ts(test_point_data$Value,
                     start = c(year(min(test_point_data$Date)), month(min(test_point_data$Date))),
                     frequency = 6)
  
  model <- arima_models[[point]]
  
  forecast_horizon <- length(ts_test_data)
  forecast_values <- forecast(model, h = forecast_horizon)
  
  # Calculate RMSE for test data
  test_rmse <- calculate_rmse(test_point_data$Value, forecast_values$mean)
  
  # Store the RMSE value in the data frame
  test_rmse_values <- rbind(test_rmse_values, data.frame(Point = point, RMSE = test_rmse))
  
  cat("\nTest RMSE for Point:", point, ":", test_rmse, "\n")
}

# Print Test RMSE values
print(test_rmse_values)


