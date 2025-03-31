# Load required libraries
library(tidyverse)
library(stringr)
library(zoo)

# Load the dataset
data <- read.csv("../../Data/Final/final_JURONG WEST_long.csv")

# Extract first month and year from the bi-monthly period
data$first_month <- str_extract(data$period, "^[A-Za-z]+")  # First month
data$year <- str_extract(data$period, "\\d{4}")             # Year

# Convert first month and year into proper Date format
data$Date <- as.Date(paste("1", data$first_month, data$year), format="%d %b %Y")

# Create a full sequence of bi-monthly periods
full_dates <- seq(from = min(data$Date, na.rm = TRUE), 
                  to = max(data$Date, na.rm = TRUE), 
                  by = "2 months")

# Expand the dataset to include all missing periods (ONLY for missing dates)
existing_dates <- unique(data$Date)
missing_dates <- setdiff(full_dates, existing_dates)

expanded_data <- expand.grid(Date = missing_dates, x = unique(data$x), y = unique(data$y))
expanded_data$Date <- as.Date(expanded_data$Date, origin = "1970-01-01")

# Merge only missing rows with original data
merged_data <- bind_rows(data, expanded_data)

# Apply Spline Interpolation only to missing rows
spline_fit_full <- merged_data %>%
  group_by(x, y) %>%
  mutate(avg_LST = ifelse(
    is.na(avg_LST),
    {
      clean_data <- .[!is.na(.$avg_LST), ]  # Keep only rows with actual data
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
write.csv(final_imputed_data, "final_imputed_JURONG_WEST.csv", row.names = FALSE)


