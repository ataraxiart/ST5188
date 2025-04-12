#Changi

set.seed(5188)

# Load Data
train_C <- read_csv(here("Data/Final/Imputation/imp_train_set.csv"))
test_C <- read_csv(here("Data/Final/Imputation/imp_test_set.csv"))

# Create Unique Point Identifier
train_Changi <- train_C %>% 
  mutate(point_id = paste(x, y, sep = "_"))

test_Changi <- test_C %>% 
  mutate(point_id = paste(x, y, sep = "_"))

# Sample 30 Discrete Points
unique_points <- train_Changi %>% 
  select(point_id, x, y) %>% 
  distinct()
n_points <- 30
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
  period <- parts[1]
  year_val <- parts[2]
  months <- unlist(strsplit(period, "-"))
  month_num <- match(months[1], month.abb)
  date_str <- paste0(year_val, "-", sprintf("%02d", month_num), "-01")
  as.Date(date_str)
}

train_Changi$Date <- as.Date(sapply(train_Changi$Date, convert_period_to_date))
test_Changi$Date <- as.Date(sapply(test_Changi$Date, convert_period_to_date))

# RMSE Calculation
calculate_rmse <- function(actual, forecasted) {
  sqrt(mean((actual - forecasted)^2))
}

# Rolling Window ARIMA Training and Forecasting
train_sizes <- 9:13  # Years of training data
forecast_horizon <- 12  # Bimonthly periods
num_samples <- 10  # Training-validation pairs

final_results <- list()

for (train_size in train_sizes) {
  # Table for the current window (adjust the number of columns dynamically)
  train_window_results <- matrix(NA, nrow = 6, ncol = length(sampled_points))  # Use all 30 points
  colnames(train_window_results) <- paste0("X", 1:length(sampled_points))  # Columns for 30 points
  rownames(train_window_results) <- c("1", "3", "6", "9", "12", "Overall")  # Forecast horizons
  
  for (point_idx in 1:length(sampled_points)) {  # Process all 30 points
    point <- sampled_points[point_idx]
    point_data <- train_Changi %>% filter(point_id == point) %>% arrange(Date)
    ts_data <- ts(point_data$Value, start = c(year(min(point_data$Date)), month(min(point_data$Date))), frequency = 6)
    
    sample_rmse <- matrix(NA, nrow = num_samples, ncol = 6)  # Store RMSE for each sample and forecast horizon
    
    for (i in 1:num_samples) {  # Generate 10 samples
      # Random sampling logic for training-validation split
      end_train <- length(ts_data) - forecast_horizon - (train_size * 6) + i
      start_train <- max(1, end_train - train_size * 6 + 1)
      
      train_series <- window(ts_data, start = c(year(point_data$Date[start_train]), month(point_data$Date[start_train])),
                             end = c(year(point_data$Date[end_train]), month(point_data$Date[end_train])))
      val_series <- window(ts_data, start = c(year(point_data$Date[end_train + 1]), month(point_data$Date[end_train + 1])))
      
      if (length(val_series) < forecast_horizon || any(is.na(val_series))) next  # Skip invalid validation sets
      
      model <- auto.arima(train_series)
      forecast_values <- forecast(model, h = forecast_horizon)
      
      rmse_f <- sapply(c(1, 3, 6, 9, 12), function(h) calculate_rmse(val_series[h], forecast_values$mean[h]))
      sample_rmse[i, ] <- c(calculate_rmse(val_series, forecast_values$mean), rmse_f)
    }
    
    # Average RMSE over samples
    avg_rmse <- colMeans(sample_rmse, na.rm = TRUE)
    train_window_results[, point_idx] <- avg_rmse  # Assign RMSE values to the table
  }
  
  # Store the table for the current window
  final_results[[paste0("Window_", train_size)]] <- train_window_results
}

# Print results
for (window in names(final_results)) {
  cat("\nResults for", window, ":\n")
  print(final_results[[window]])
}

# Combine all results into a single data frame
all_results <- data.frame()

for (window in names(final_results)) {
  # Extract the current window's results
  window_results <- as.data.frame(final_results[[window]])
  
  # Add a column to indicate the training window size
  window_results$Window <- window
  
  # Combine with overall results
  all_results <- rbind(all_results, cbind(Window = window, rownames_to_column(window_results, "Horizon")))
}

# Save combined results to a single CSV file
write.csv(all_results, "Combined_RMSE_Results.csv", row.names = FALSE)
