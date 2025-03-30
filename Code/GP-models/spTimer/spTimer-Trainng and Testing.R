#Formatting Data
library(dplyr)
library(lubridate)
library(sf)
library(spTimer)

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
    drop_na(sapply(1:n, function(i )paste0("lag_", i)))
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
  start_values <- seq(min_max[1], min_max[2]-window,1)
  end_values <- start_values + window
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
                  year = as.numeric(substr(period, 8, 12)
                  ))
  return(df)
  
}

############################
############################

#Data used:

data<-read.csv("/Users/varshap/Downloads/ST5188/Data/Final/final_CHANGI_long.csv")
data <- get_bimonth_year_columns(get_date_index(data))

###########
#Training data

#Setting training parameters time points -10 years
training_start<-"Jan-Feb 2012"
training_end<-"Nov-Dec 2022"

Training_data_full<-filter_by_date_index(data,training_end) #2000 to 2022 dataset

Training_10yr<-Training_data_full |>
  filter(year>=2013 & year<=2023) #filters and is left with just 2013 -2022 data

Testing_2yr <- test_data_full |>  # Start from full dataset
  filter(year >= 2023 & year <= 2024) |>  # Explicit year filter
  filter(date_index > max(Training_10yr$date_index))  # Ensure no overlap

max_lags <- 12 # Set maximum lags to calculate
training_data <- get_valid_rows(get_lags(Training_10yr, 12), 12)

resolution <- get_resolution(training_data)

x_min <- 103.9832 
x_max <- 103.9832 + 10*resolution   
y_min <- 1.348838  
y_max <- 1.348838 + 10*resolution   


coordinates<-c(x_min, x_max, y_min,y_max)

training_data <- filter_by_geography(training_data,bbox=get_bounding_box(coordinates))

data_lst <- get_data_by_window(training_data,window = 5)
len_data_lst <- length(data_lst)

# randomly sample 10 
set.seed(5188)
random_sample <- sample(1:len_data_lst,10)
traindata_lst_sample <- filter_lst_by_index(data_lst, random_sample)
train_df <- traindata_lst_sample[[10]]

get_validation_df <- function(df,horizon,ref_df){
  date_indices <- unique(ref_df$date_index)
  current_index <- max(df$date_index)
  indices <- seq(current_index + 1, current_index+horizon, 1)
  len_indexes <- length(indices)
  result_indices <- c()
  for (i in 1:length(indices)){
    if (indices[i] %in% date_indices){
      result_indices <- c(result_indices, indices[i])
    }
  }
  
  validation_df <- ref_df %>%
    filter(ref_df$date_index %in% indices)
  forecast_steps <- indices - current_index 
  return(list(validation_df,forecast_steps))
}

validation_df <- get_validation_df(train_df,3,ref_df = training_data)[[1]]
foreStep <- max(get_validation_df(train_df,3,training_data)[[2]])
coords <- as.matrix(validation_df %>% dplyr::select(x,y) %>% distinct())
set.seed(5188)
post.gp <- spT.Gibbs(formula = avg_LST ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5, 
                     data = train_df, model = "GP", 
                     coords = ~ x + y, scale.transform = "SQRT", 
                     spatial.decay = spT.decay(distribution = Gamm(2, 1), tuning = 0.1))
#spatial.decay = spT.decay(distribution = Unif(0.001, 5), tuning = 0.1))
set.seed(5188)
pred.gp <- predict.spT(post.gp, newdata = validation_df ,
                       type="temporal",newcoords= coords,
                       foreStep = foreStep,tol.dist = 0.0005) 
#tol.dist was causing all the problems

spT.validation(validation_df$avg_LST, c(pred.gp$Median))


############
##Testing

#Setting testing parameter time points-10 years
testing_start<-"Jan-Feb 2023"
testing_end<-"Nov-Dec 2024"


# Setting up testing data
test_data_full <- filter_by_date_index(data, testing_end) # Get all data from 2000 to test end date

Testing_2yr<-test_data_full |>
  filter(year>=2023 & year<=2024) #filters and is left with just 2023 -2024 data

#testing with 4 lags
testing_data <- get_valid_rows(get_lags(Testing_2yr, 4), 4) 

# if this is 12 itself it ends up as NA as seen here # A tibble: 0 × 19
# Groups:   x, y [0]
# ℹ 19 variables: x <dbl>, y <dbl>, period <chr>, avg_LST <dbl>, date_index <int>,
#   bimonth <int>, year <dbl>, lag_1 <dbl>, lag_2 <dbl>, lag_3 <dbl>, lag_4 <dbl>,
#   lag_5 <dbl>, lag_6 <dbl>, lag_7 <dbl>, lag_8 <dbl>, lag_9 <dbl>, lag_10 <dbl>,
#   lag_11 <dbl>, lag_12 <dbl>
# hence taken another value for lags

#resolution doesnt change so considering the same as in training above

testing_data <- filter_by_geography(testing_data,bbox=get_bounding_box(coordinates))

#as 2 years is a small time frame, not performing sampling here 
#but directly taking the test_df as testing data
test_df<-testing_data


test_df <- test_df |>
  group_by(x, y) |>
  arrange(date_index)|>
  mutate(
    lag_1 = lag(avg_LST, 1),  # 1 period (~2 months) lag
    lag_2 = lag(avg_LST, 2),   # 2 periods (~4 months)
    lag_3 = lag(avg_LST, 3)    # 3 periods (~6 months)
  )|>
  ungroup()|>
  filter(complete.cases(lag_1, lag_2, lag_3))


#Create validation set +foreStep and Coordinates
validation_df <- test_df |>
  group_by(x, y) |>
  filter(date_index > max(date_index) - 3) |>  # Last 3 periods
  ungroup()

foreStep<-max(get_validation_df(test_df,3,testing_data)[[2]])

coords <- as.matrix(validation_df %>% select(x, y) %>% distinct())

# Fitting the Model
set.seed(5188)
post.gp <- spT.Gibbs(
  formula = avg_LST ~ lag_1 + lag_2 + lag_3,  # Matches test_df
  data = test_df,
  coords = ~x + y,
  scale.transform = "SQRT",
  spatial.decay = spT.decay(distribution = Gamm(2, 1), tuning = 0.1)
)

#Prediction
set.seed(5188)
pred.gp <- predict.spT(post.gp, newdata = validation_df ,
                       type="temporal",newcoords= coords,
                       foreStep = foreStep,tol.dist = 0.0005) 

spT.validation(validation_df$avg_LST, c(pred.gp$Median))

