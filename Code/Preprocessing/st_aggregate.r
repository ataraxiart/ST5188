# script to aggregation function

st_aggregation <- function(data, subzone) {
  
  data$period <- paste0(
    case_when(
      month(data$date) %in% c(1, 2) ~ "Jan-Feb ",
      month(data$date) %in% c(3, 4) ~ "Mar-Apr ",
      month(data$date) %in% c(5, 6) ~ "May-Jun ",
      month(data$date) %in% c(7, 8) ~ "Jul-Aug ",
      month(data$date) %in% c(9, 10) ~ "Sep-Oct ",
      month(data$date) %in% c(11, 12) ~ "Nov-Dec "
    ),
    year(data$date) # add the year
  )
  
  # average LST per period per coordinate (x, y) pair
  aggregated_data <- data |>
    group_by(x, y, period) |>
    summarise(avg_LST = mean(LST, na.rm = TRUE), .groups = "drop") |>
    mutate( # to arrange in order
      temp_month = str_extract(period, "^[A-Za-z]+"),   # extracts the first month
      temp_year = as.integer(str_extract(period, "\\d{4}")),
      temp_date = as.Date(paste("01", temp_month, temp_year), format = "%d %b %Y")) |>
    arrange(temp_date) |>
    select(- c("temp_month", "temp_year", "temp_date"))

  # update dataframe to wide format
  final_data <- aggregated_data |>
    pivot_wider(names_from = period, values_from = avg_LST)

  write.csv(final_data, paste0("../../Data/Final/", subzone, ".csv"), row.names = FALSE)
}