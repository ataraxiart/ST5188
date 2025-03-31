# Load required libraries
library(tidyverse)
library(stringr)
library(zoo)

# Load the dataset
data <- read.csv("../../Data/Final/final_JURONG WEST_long.csv")

# Extract first month and year from the bi-monthly period
# Example: "Mar-Apr 2000" -> Extract "Mar" and "2000"
data$first_month <- str_extract(data$period, "^[A-Za-z]+")  # First month
data$year <- str_extract(data$period, "\\d{4}")             # Year

# Convert first month and year into proper Date format
data$Date <- as.Date(paste("1", data$first_month, data$year), format="%d %b %Y")

# Create a full sequence of bi-monthly periods
full_dates <- seq(from = min(data$Date, na.rm = TRUE), 
                  to = max(data$Date, na.rm = TRUE), 
                  by = "2 months")

# Expand the dataset to include all missing periods (create placeholder rows for missing periods)
expanded_data <- expand.grid(Date = full_dates, x = unique(data$x), y = unique(data$y))

# Merge with original data
merged_data <- left_join(expanded_data, data, by = c("Date", "x", "y"))

# Check missing data percentage
missing_percentage <- sum(is.na(merged_data$avg_LST)) / nrow(merged_data)
print(paste("Missing Data Percentage:", round(missing_percentage * 100, 2), "%"))

# --------------------------
# 1. TEST SPLINE ON A SMALL SAMPLE FIRST
# --------------------------

# Select a small sample of x values (adjust the number if needed)
test_x_values <- sample(unique(merged_data$x), 10)  # Select 10 unique x values

# Filter the dataset for testing
test_data <- merged_data %>% filter(x %in% test_x_values)

# Apply Spline Interpolation to the test dataset
test_spline_fit <- test_data %>%
  group_by(x, y) %>%
  mutate(avg_LST = ifelse(
    is.na(avg_LST),
    {
      # Remove rows where avg_LST is NA before fitting the spline
      clean_data <- .[!is.na(.$avg_LST), ]  # Only remove rows where avg_LST is NA
      clean_data <- clean_data[is.finite(clean_data$avg_LST) & is.finite(as.numeric(clean_data$Date)), ]
      
      if (nrow(clean_data) > 1) {  # Ensure enough data points for spline fitting
        spline_fit <- smooth.spline(as.numeric(clean_data$Date), clean_data$avg_LST, all.knots = TRUE)
        predict(spline_fit, as.numeric(Date))$y
      } else {
        NA  # Return NA if not enough data to fit spline
      }
    },
    avg_LST  # Keep original values for non-missing rows
  ))

# Check if Spline successfully filled missing values in the test dataset
print(paste("Remaining missing values in test data:", sum(is.na(test_spline_fit$avg_LST))))

# --------------------------
# 2. APPLY SPLINE TO FULL DATA IF TEST IS SUCCESSFUL
# --------------------------

if (sum(is.na(test_spline_fit$avg_LST)) < sum(is.na(test_data$avg_LST))) {
  print("Spline test successful! Running on full dataset...")
  
  # Apply Spline Interpolation to the full dataset
  spline_fit_full <- merged_data %>%
    group_by(x, y) %>%
    mutate(avg_LST = ifelse(
      is.na(avg_LST),
      {
        # Remove rows where avg_LST is NA before fitting the spline
        clean_data <- .[!is.na(.$avg_LST), ]  # Only remove rows where avg_LST is NA
        clean_data <- clean_data[is.finite(clean_data$avg_LST) & is.finite(as.numeric(clean_data$Date)), ]
        
        if (nrow(clean_data) > 1) {  # Ensure enough data points for spline fitting
          spline_fit <- smooth.spline(as.numeric(clean_data$Date), clean_data$avg_LST, all.knots = TRUE)
          predict(spline_fit, as.numeric(Date))$y
        } else {
          NA  # Return NA if not enough data to fit spline
        }
      },
      avg_LST  # Keep original values for non-missing rows
    ))
  
  # Convert Date back to original bi-monthly format
  spline_fit_full$period <- paste0(format(spline_fit_full$Date, "%b"), "-", 
                                   format(spline_fit_full$Date + months(1), "%b"), " ",
                                   format(spline_fit_full$Date, "%Y"))
  
  final_imputed_data <- spline_fit_full %>% select(x, y, period, avg_LST) %>% rename(Date = period, Value = avg_LST)
  
  # Save the imputed dataset
  write.csv(final_imputed_data, "../../Data/Final/final_imputed_JURONG_WEST.csv", row.names = FALSE)
  
  print("Spline imputation completed and file saved successfully!")
} else {
  print("Spline test failed. Please check data consistency.")
}




