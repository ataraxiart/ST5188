#Changi

library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(urca)
library(ggplot2)
library(cluster)
library(lubridate)

train_C <- read_csv("../../Data/Final/Imputation/imp_train_set.csv")
test_C <- read_csv("../../Data/Final/Imputation/imp_test_set.csv")

# Create a Unique Point Identifier
train_Changi <- train_C %>%
  mutate(point_id = paste(x, y, sep = "_"))

test_Changi <- test_C %>%
  mutate(point_id = paste(x, y, sep = "_"))

# Sample Discrete Points from the Training Data
unique_points <- train_Changi %>%
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

cat("Sampled point_id values for Changi:", sampled_points, "\n")

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

train_Changi$Date <- as.Date(sapply(train_Changi$Date, convert_period_to_date))
test_Changi$Date <- as.Date(sapply(test_Changi$Date, convert_period_to_date))

# Grid search for ARIMA parameters and calculating RMSE
arima_models <- list()
rmse_values <- data.frame(Point = character(), RMSE = numeric())  # Stores best RMSE for training

# Horizon-specific RMSE matrix
rmse_matrix <- matrix(NA, nrow = 6, ncol = length(sampled_points))  # Rows: horizons (1, 3, ..., Overall), Columns: points
colnames(rmse_matrix) <- paste0("X", 1:length(sampled_points))  # Columns: X1, X2, ..., X30
rownames(rmse_matrix) <- c("1", "3", "6", "9", "12", "Overall")  # Rows for horizons

# Define a function to calculate RMSE
calculate_rmse <- function(actual, forecasted) {
  valid_indices <- !is.na(actual) & !is.na(forecasted)  # Ignore NA values
  sqrt(mean((actual[valid_indices] - forecasted[valid_indices])^2))
}

# Iterate over each point to optimize ARIMA parameters using grid search
for (point in sampled_points) {
  
  point_data <- train_Changi %>%
    filter(point_id == point) %>%
    arrange(Date)
  
  test_point_data <- test_Changi %>%
    filter(point_id == point) %>%
    arrange(Date)
  
  ts_train_data <- ts(point_data$Value,
                      start = c(year(min(point_data$Date)), month(min(point_data$Date))),
                      frequency = 6)
  
  ts_test_data <- ts(test_point_data$Value,
                     start = c(year(min(test_point_data$Date)), month(min(test_point_data$Date))),
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
                arima(ts_train_data, order = c(p, d, q), seasonal = list(order = c(sp, sd, sq), period = 6))
              }, error = function(e) NULL)
              
              if (!is.null(model)) {
                # Forecast using the model
                forecast_horizon <- length(ts_train_data)
                forecast_values <- forecast(model, h = forecast_horizon)
                
                # Calculate RMSE for the model
                rmse <- calculate_rmse(ts_train_data, forecast_values$mean)
                
                if (!is.na(rmse) && rmse < best_rmse) {  # Ensure RMSE is valid
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
  
  # Forecast for the 2-year test horizon (12 bimonthly periods)
  forecast_horizon <- length(ts_test_data)
  forecast_values <- forecast(arima_models[[point]], h = forecast_horizon)
  
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
  
  # Calculate RMSE for specific horizons
  rmse_horizons <- sapply(c(1, 3, 6, 9, 12), function(h) {
    if (h <= length(ts_test_data)) {
      calculate_rmse(ts_test_data[h], forecast_values$mean[h])
    } else {
      NA  # Handle cases where h exceeds the test data length
    }
  })
  
  # Calculate Overall RMSE for all 12 forecast steps
  rmse_overall <- calculate_rmse(ts_test_data, forecast_values$mean)
  
  # Store RMSE values in the matrix
  rmse_matrix[1:5, sampled_points == point] <- rmse_horizons  # Store horizon-specific RMSEs
  rmse_matrix[6, sampled_points == point] <- rmse_overall  # Store Overall RMSE
}

# Combine all comparison data into a single data frame
comparison_data <- bind_rows(all_comparisons)

# Convert matrix to data frame for cleaner output
rmse_dataframe <- as.data.frame(rmse_matrix)

# Print the RMSE table
print(rmse_dataframe)

write.csv(rmse_dataframe, "RMSE_23-2.csv", row.names = TRUE)
