library(tidyverse)
library(stringr)

# Load the dataset
data <- read.csv("../../Data/Final/final_JURONG EAST_long.csv")

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

# Expand the dataset to include all missing periods
expanded_data <- expand.grid(Date = full_dates, x = unique(data$x), y = unique(data$y))

# Merge with original data
merged_data <- left_join(expanded_data, data, by = c("Date", "x", "y"))

# Perform LOESS imputation
loess_fit <- merged_data %>%
  group_by(x, y) %>%
  mutate(avg_LST = ifelse(is.na(avg_LST),
                          predict(loess(avg_LST ~ as.numeric(Date), span = 0.5, data = ., na.action = na.exclude)),
                          avg_LST))

# Convert Date back to original bi-monthly format
loess_fit$period <- paste0(format(loess_fit$Date, "%b"), "-", 
                           format(loess_fit$Date + months(1), "%b"), " ",
                           format(loess_fit$Date, "%Y"))

final_imputed_data <- loess_fit %>% select(x, y, period, avg_LST) %>% rename(Date = period, Value = avg_LST)

# Save the imputed dataset
write.csv(final_imputed_data, "final_imputed_JURONG_EAST.csv", row.names = FALSE)
