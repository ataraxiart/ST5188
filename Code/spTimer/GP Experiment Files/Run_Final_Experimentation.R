# Necessary Functions for GP model

library(dplyr)
library(lubridate)
library(sf)
library(spTimer)
library(here)


### Helper Functions ###

### get date index for data (MUST APPLY THIS BEFORE ANYTHING ELSE)
get_date_index <- function(df){
  years <- 2000:2024
  months <- c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", "Sep-Oct", "Nov-Dec")
  date_periods <- expand.grid(Year = years, Month = months) %>%
    arrange(Year, Month) %>%
    mutate(period = paste(Month, Year)) %>%
    pull(period)
  period_numbers <- seq_along(date_periods)
  lookup_table <- data.frame(period = date_periods, date_index = period_numbers)
  df <- df %>%
    left_join(lookup_table, by = "period")
  return(df)
}

### get lags for data
get_lags <- function(df,n){
  for (i in 1:n){
    df<-df %>%
      arrange(x, y, date_index) %>%  #sorting by location then time
      group_by(x, y) %>%       #grouping by unique location
      mutate(!!paste0("lag_", i) := lag(avg_LST, i)) #creating i-period lag for each location
  }
  return(df)
}

### eliminate rows with NA values
get_valid_rows <- function(df,n){
  df<-df %>%
    tidyr::drop_na(sapply(1:n, function(i )paste0("lag_", i)))
  return(df)
}

### filter by date index in data
filter_by_date_index <- function(df,date){
  years <- 2000:2024
  months <- c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", "Sep-Oct", "Nov-Dec")
  date_periods <- expand.grid(Year = years, Month = months) %>%
    arrange(Year, Month) %>%
    mutate(period = paste(Month, Year)) %>%
    pull(period)
  period_numbers <- seq_along(date_periods)
  lookup_table <- data.frame(period = date_periods, date_index = period_numbers)
  df <- df %>%
    filter(date_index <= which(lookup_table$period == date))
  return(df)
}

### get range of date index in data
get_date_range <- function(df){
  return(c(min(df$date_index),max(df$date_index)))
}

### return list of all data in rolling window approach
get_data_by_window <- function(df,window){
  min_max <- get_date_range(df)
  start_values <- seq(min_max[1], min_max[2]-window-2*6 - 1,1)
  end_values <- start_values + window - 1
  data_lst <- list()
  for (i in 1:length(start_values)){
    data_lst[[i]] <- df %>%
      filter( `date_index` >= start_values[i], `date_index` <= end_values[i])
  }
  
  return(data_lst)
}


### return data from list to be used in prediction
filter_lst_by_index<- function(lst,index){
  lst_length <- length(lst)
  return(lst[index])
}

### get resolution of data
get_resolution <- function(df){
  coord_matrix <- as.matrix(df %>% dplyr::select(x, y) %>% distinct())
  unique_x_coordinates <- unique(coord_matrix[,1])
  unique_y_coordinates <- unique(coord_matrix[,2])
  return(unique_x_coordinates[2] - unique_x_coordinates[1])
}

### get specified bounding box
get_bounding_box <- function(coordinates){
  x_min <- coordinates[1]
  x_max <- coordinates[2]
  y_min <- coordinates[3]
  y_max <- coordinates[4]
  bbox <- st_sfc(
    st_polygon(list(matrix(c(
      x_min, y_min,
      x_max, y_min,
      x_max, y_max,
      x_min, y_max,
      x_min, y_min
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  return(bbox)
}


### filter by geography
filter_by_geography <- function(df,bbox=NULL,random = FALSE,seed = 5188, n = NULL){
  if (!random){
    df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)
    filtered_df_sf <- df_sf[st_within(df_sf, bbox, sparse = FALSE), ]
    filtered_df <- st_drop_geometry(filtered_df_sf)  
    filtered_df$x<- st_coordinates(filtered_df_sf)[, 1]  
    filtered_df$y <- st_coordinates(filtered_df_sf)[, 2] 
    return(filtered_df)
  } else{
    set.seed(seed)
    n <- n
    sampled_coords <- df %>%
      dplyr::select(x, y) %>%
      distinct() %>%  
      sample_n(n)  
    filtered_df <- df %>%
      filter(paste(x, y) %in% paste(sampled_coords$x, sampled_coords$y))
    return(filtered_df)
  }
}

### Get month and year columns
get_bimonth_year_columns <- function(df){
  month_df <- data.frame(
    month_name = c('Jan','Mar','May','Jul','Sep','Nov')
  )
  
  df <- df %>%
    dplyr::mutate(bimonth = match(paste(substr(df$period, 1, 3)),month_df$month_name), 
                  year = as.numeric(substr(period, 8, 12)))
  return(df)
  
}



#########################
#########################
#Sourcing the RScripts for the GP Experiment

source(here("Code/spTimer/GP Experiment Files/lag_experiment.R"))
source(here("Code/spTimer/GP Experiment Files/covariance_experiment.R"))
source(here("Code/spTimer/GP Experiment Files/grid_search_changi_experiment.R"))
source(here("Code/spTimer/GP Experiment Files/hyperparameter_changi_experiment.R"))
source(here("Code/spTimer/GP Experiment Files/final_gp_rolling_window_experiment.R"))
source(here("Code/spTimer/GP Experiment Files/final_gp_23_2_experiment.R"))


