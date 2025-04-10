# Formatting Data
library(dplyr)
library(lubridate)
library(sf)
library(spTimer)
library(tidyr)
library(purrr)

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
                  year = as.numeric(substr(period, 8, 12)
                  ))
  return(df)
  
}



############################
############################


# Process Data

# Original Data used:
dir <- "C:/Users/samue/Documents/Masters/Sem 2/ST5188/st5188/Data/Final/final_CHANGI_long.csv"
data <- read.csv(dir)
# Define Bounding Box to reduce computational complexity 
resolution <- get_resolution(data)
x_min <- 103.9832 
x_max <- 103.9832 + 10*resolution   
y_min <- 1.348838  
y_max <- 1.348838 + 10*resolution   
coordinates<-c(x_min, x_max, y_min,y_max)
data <- filter_by_geography(data,bbox=get_bounding_box(coordinates))

get_processed_data <- function(data) {
  # Function to apply spline interpolation per (x, y) group
  impute_spline <- function(df) {
    full_df <- data.frame(date_index = full_dates)
    merged_df <- merge(full_df, df, by = "date_index", all.x = TRUE)
    if (sum(!is.na(merged_df$avg_LST)) >= 3) {
      spline_model <- splinefun(merged_df$date_index[!is.na(merged_df$avg_LST)], 
                                merged_df$avg_LST[!is.na(merged_df$avg_LST)], 
                                method = "natural")
      merged_df$avg_LST[is.na(merged_df$avg_LST)] <- spline_model(merged_df$date_index[is.na(merged_df$avg_LST)])
    }
    
    return(merged_df)
  }
  data<- get_bimonth_year_columns(get_date_index(data))
  full_dates <- seq(min(data$date_index), max(data$date_index), by = 1)
  imputed_data <- data %>% group_by(x, y) %>% group_modify(~ impute_spline(.x)) %>% ungroup()
  max_lags <- 3 # Set maximum lags to calculate
  imputed_data <- get_valid_rows(get_lags(imputed_data, 3), 3)
  return(imputed_data)
}

data <- get_processed_data(data)



# Functions to obtain optimal phi
### Function 1 to use Bayesian to obtain phi list (called upon in Function 2)
run_bayesian_phi <- function(cov_fn, n_samples, random_sample, traindata_lst_sample, new_formula, window_size) {
  phi_df <- list()
  for (i in 1:n_samples) {
    print(paste0("Sample no.: ", i, " (window size: ", window_size, ")"))
    train_df <- traindata_lst_sample[[i]]
    set.seed(5188)
    post.gp <- spT.Gibbs(formula = new_formula, 
                         data = train_df, model = "GP", 
                         coords = ~ x + y, scale.transform = "SQRT", 
                         cov.fnc = cov_fn, nItr=5000,
                         spatial.decay = spT.decay(distribution = Gamm(0.01, 0.01), tuning = 6))
    
    df <- data.frame(sample_no = i, phi_val = coef(post.gp)["phi"])
    phi_df[[i]] <- df
    
    print(paste0("Sample ", i, " (window size: ", window_size, ") done!"))
  }
  phi_df_combined <- do.call(rbind, phi_df)
  return(phi_df_combined)
}


### Function 2 to run Grid Search to obtain optimal phi
run_grid_search_experiment <- function(cov_fn, n_samples, len_data_lst, data_lst, window_size) {
  
  set.seed(5188)
  random_sample <- sample(1:len_data_lst, n_samples)
  traindata_lst_sample <- filter_lst_by_index(data_lst, random_sample)
  
  get_formula <- function(response, num_lags) {
    change_formula <- function(response, num_lags) {
      predictors <- paste0("lag_", 1:num_lags)
      formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
      return(formula)
    }
    new_formula <- change_formula(response, num_lags)
    return(new_formula)
  }
  new_formula <- get_formula('avg_LST', 3)
  
  # obtain phi list
  phi_df_combined <- run_bayesian_phi(cov_fn, n_samples, random_sample, traindata_lst_sample, new_formula, window_size)
  min_phi <- floor(min(phi_df_combined$phi_val) * 10)/10 # 1dp
  max_phi <- ceiling(max(phi_df_combined$phi_val) * 10)/10 # 1dp
  phi_list <- seq(from = ifelse(min_phi - 0.4 <= 0, 0.1, min_phi - 0.4), to = max_phi + 0.1, by = 0.1)
  
  # find the optimal phi
  output_df <- data.frame(ID = c("1","3","6","9","12","overall_RMSE"))
  
  get_RMSE_by_phi <- function(phi, cov_fn, n_samples) {
    iteration_list <- list()
    
    for (i in 1:n_samples) {
      print(paste0("We are on sample ", i, " (window size: ", window_size, ")"))
      
      train_df <- traindata_lst_sample[[i]]
      
      get_validation_df <- function(df,horizon,ref_df) {
        current_index <- max(df$date_index)
        indices <- seq(current_index + 1, current_index+horizon, 1)
        validation_df <- ref_df %>% filter(ref_df$date_index %in% indices)
        return(validation_df)
      }
      
      validation_df <- get_validation_df(train_df, 12, ref_df = data %>% ungroup)
      validation_df$forecast_step <- validation_df$date_index - min(validation_df$date_index) + 1
      
      set.seed(5188)
      post.gp <- spT.Gibbs(formula = new_formula, 
                           data = train_df, model = "GP", 
                           coords = ~ x + y, scale.transform = "SQRT", 
                           cov.fnc = cov_fn, nItr=5000,
                           spatial.decay = spT.decay(distribution="FIXED", value = phi))
      
      set.seed(5188)
      pred.gp <- predict.spT(post.gp, newdata = validation_df ,
                             type="temporal",newcoords= ~ x + y,
                             foreStep = 12, tol.dist = 0.0005) 
      
      get_RMSE_for_specific_time_step <- function(timestep = NULL, df1=NULL, df2=NULL) {
        df1_values <- df1 %>%
          filter(forecast_step == timestep) %>%
          pull(avg_LST)
        df2_values <- df2[timestep,]
        
        return(sqrt(mean((df1_values - df2_values)^2)))
      }
      
      individual_RMSE <- sapply(c(1,3,6,9,12), 
                                function(x) get_RMSE_for_specific_time_step(x, validation_df, pred.gp$Median))
      
      overall_RMSE <- unname(spT.validation(validation_df$avg_LST, c(pred.gp$Median))[2])
      
      selected_ids <- c(c(1,3,6,9,12), "overall_RMSE")
      df <- data.frame(ID = selected_ids, LST = c(individual_RMSE, overall_RMSE))
      iteration_list[[i]] <- df
    }
    
    all_data <- do.call(rbind, iteration_list)
    
    # Compute the average LST for each ID
    average_LST <- aggregate(LST ~ ID, data = all_data, FUN = mean)
    return(average_LST)
  }
  
  for (phi in phi_list) {
    print(paste0("We are using phi = ", phi))
    
    average_LST <- get_RMSE_by_phi(phi, cov_fn, n_samples)
    colnames(average_LST) <- c("ID", phi)
    
    output_df <- output_df %>% left_join(average_LST, by="ID")
    write.csv(output_df, paste0("phi_RMSE_window", window_size, ".csv"))
  }
  optimal_phi <- colnames(output_df)[which.min(as.numeric(output_df[6, 2:ncol(output_df)])) + 1]
  return(optimal_phi)
}

# test
# run_grid_search_experiment(cov_fn = "spherical", n_samples = 2, len_data_lst = 64, data_lst, window_size = 10)


# Function to run for window size 25
run_final_gp <- function(window_sizes, cov_fn, n_samples) {
  all_windows_output <- list() 
  
  for (window_size in window_sizes) {
    print(paste0("Running window size = ", window_size))
    
    data_lst <- get_data_by_window(data, window = window_size*6)
    len_data_lst <- length(data_lst)
    
    optimal_phi <- run_grid_search_experiment(cov_fn, n_samples, len_data_lst, data_lst, window_size)
    
    set.seed(5188)
    random_sample <- sample(1:len_data_lst, n_samples)
    traindata_lst_sample <- filter_lst_by_index(data_lst, random_sample)
    
    get_formula <- function(response, num_lags) {
      change_formula <- function(response, num_lags) {
        predictors <- paste0("lag_", 1:num_lags)
        formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
        return(formula)
      }
      new_formula <- change_formula(response, num_lags)
      return(new_formula)
    }
    new_formula <- get_formula('avg_LST', 3)
    
    samples_iter_list <- list()
    for (i in 1:n_samples) {
      print(paste0("  Sample ", i, "..."))
      
      train_df <- traindata_lst_sample[[i]]
      
      get_validation_df <- function(df,horizon,ref_df) {
        current_index <- max(df$date_index)
        indices <- seq(current_index + 1, current_index+horizon, 1)
        validation_df <- ref_df %>%
          filter(ref_df$date_index %in% indices)
        return(validation_df)
      }
      
      validation_df <- get_validation_df(train_df, 12, ref_df = data %>% ungroup)
      validation_df$forecast_step <- validation_df$date_index - min(validation_df$date_index) + 1
      
      set.seed(5188)
      post.gp <- spT.Gibbs(formula = new_formula, 
                           data = train_df, model = "GP", 
                           coords = ~ x + y, scale.transform = "SQRT", 
                           cov.fnc = cov_fn, nItr=5000,
                           spatial.decay = spT.decay(distribution="FIXED", value = optimal_phi))
      
      set.seed(5188)
      pred.gp <- predict.spT(post.gp, newdata = validation_df ,
                             type="temporal",newcoords= ~ x + y,
                             foreStep = 12, tol.dist = 0.0005) 
      
      get_RMSE_for_specific_time_step <- function(timestep = NULL, df1=NULL, df2=NULL) {
        df1_values <- df1%>%
          filter(forecast_step == timestep) %>%
          pull(avg_LST)
        df2_values <- df2[timestep,]
        return(sqrt(mean((df1_values - df2_values)^2)))
      }
      
      individual_RMSE <- sapply(c(1,3,6,9,12), 
                                function(x) get_RMSE_for_specific_time_step(x,validation_df,pred.gp$Median))
      
      overall_RMSE <- unname(spT.validation(validation_df$avg_LST, c(pred.gp$Median))[2])
      selected_ids <- c(c(1,3,6,9,12), "overall_RMSE")
      
      df <- data.frame(ID = selected_ids, RMSE = c(individual_RMSE, overall_RMSE))
      samples_iter_list[[i]] <- df
    }
    
    all_data <- do.call(rbind, samples_iter_list)
    avg_RMSE <- aggregate(RMSE ~ ID, data = all_data, FUN = mean)
    colnames(avg_RMSE)[2] <- paste0("Window_", window_size)
    
    all_windows_output[[as.character(window_size)]] <- avg_RMSE
  }
  return(all_windows_output)
}

windows <- c(22)
final_results <- run_final_gp(window_sizes = windows, cov_fn = "spherical", n_samples = 1)
print(final_results)


