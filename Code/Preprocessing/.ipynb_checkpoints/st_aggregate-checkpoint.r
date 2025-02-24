library(dplyr)
library(lubridate)
library(tidyr)

head(data$date)

# Aggregation function
ST_aggregation<- function(file_path) {
  
  data <- read.csv(file_path)
  
  data$date <- as.Date(data$date, format="%Y-%m-%d") # Convert date into a 'date' format
  
  data$period <- paste0(
    case_when(
      month(data$date) %in% c(1, 2) ~ "Jan-Feb ",
      month(data$date) %in% c(3, 4) ~ "Mar-Apr ",
      month(data$date) %in% c(5, 6) ~ "May-Jun ",
      month(data$date) %in% c(7, 8) ~ "Jul-Aug ",
      month(data$date) %in% c(9, 10) ~ "Sep-Oct ",
      month(data$date) %in% c(11, 12) ~ "Nov-Dec "
    ),
    year(data$date) # Add the year
  )
  
  # Avg LST per year
  aggregated_data <- data |>
    group_by(x, y, period) |>
    summarise(avg_LST = mean(LST, na.rm = TRUE), .groups = "drop")

  final_data <- aggregated_data |>
    pivot_wider(names_from = period, values_from = avg_LST)

  return(final_data)
}

# Call upon function
file_path <- "../../Data/Final/CHANGI.csv"
final_data <- ST_aggregation(file_path)

print(final_data)

write.csv(final_data, "../../Data/Final/aggregated_CHANGI.csv", row.names = FALSE)
