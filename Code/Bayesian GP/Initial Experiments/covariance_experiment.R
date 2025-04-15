############################

#Process Data


#Original Data used:
dir <- "Data/Final/final_CHANGI_long.csv"
data <- read.csv(here(dir))
#Define Bounding Box to reduce computational complexity 
resolution <- get_resolution(data)
x_min <- 103.9832 
x_max <- 103.9832 + 10*resolution   
y_min <- 1.348838  
y_max <- 1.348838 + 10*resolution   
coordinates<-c(x_min, x_max, y_min,y_max)
data <- filter_by_geography(data,bbox=get_bounding_box(coordinates))


get_processed_data <- function(data){
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
  max_lags <- 12 # Set maximum lags to calculate
  imputed_data <- get_valid_rows(get_lags(imputed_data, 12), 12)
  return(imputed_data)
}

data <- get_processed_data(data)
data_lst <- get_data_by_window(data,window = 10*6)
len_data_lst <- length(data_lst)




#Randomly sample 10
#We test for lags


covariance_fns <- c("exponential", "gaussian", "spherical", "matern")

?spT.Gibbs





run_cov_experiment <- function(cov_lst,n_samples){
  output_df <- data.frame(ID = c("1","3","6","9","12","overall_RMSE"))
  get_RMSE_by_cov <- function(fn,n_samples = 10){
    set.seed(5188)
    random_sample <- sample(1:len_data_lst,n_samples)
    traindata_lst_sample <- filter_lst_by_index(data_lst, random_sample)
    get_formula <- function(response,num_lags){
      change_formula <- function(response, num_lags) {
        predictors <- paste0("lag_", 1:num_lags)
        formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
        return(formula)
      }
      new_formula <- change_formula(response, num_lags)
      return(new_formula)
    }
    new_formula <- get_formula('avg_LST',3)
    iteration_list <- list()
    
    
    for (i in 1:n_samples){
      
      train_df <- traindata_lst_sample[[i]]
      get_validation_df <- function(df,horizon,ref_df){
        current_index <- max(df$date_index)
        indices <- seq(current_index + 1, current_index+horizon, 1)
        validation_df <- ref_df %>%
          filter(ref_df$date_index %in% indices)
        return(validation_df)
      }
      
      validation_df <- get_validation_df(train_df,12,ref_df = data %>% ungroup)
      validation_df$forecast_step <- validation_df$date_index - min(validation_df$date_index) + 1
      
      set.seed(5188)
      post.gp <- spT.Gibbs(formula = new_formula, 
                           data = train_df, model = "GP", 
                           coords = ~ x + y, scale.transform = "SQRT", 
                           cov.fnc = fn,
                           spatial.decay = spT.decay(distribution = Gamm(4, 2), tuning = 6))
      
      set.seed(5188)2
      pred.gp <- predict.spT(post.gp, newdata = validation_df ,
                             type="temporal",newcoords= ~ x + y,
                             foreStep = 12,tol.dist = 0.0005) 
      
      get_RMSE_for_specific_time_step <- function(timestep = NULL,df1=NULL,df2=NULL){
        df1_values <- df1%>%
          filter(forecast_step == timestep) %>%
          pull(avg_LST)
        df2_values <- df2[timestep,]
        return(sqrt(mean((df1_values - df2_values)^2)))
      }
      
      individual_RMSE <- sapply(c(1,3,6,9,12),function(x) get_RMSE_for_specific_time_step(x,validation_df,pred.gp$Median))
      overall_RMSE <- unname(spT.validation(validation_df$avg_LST, c(pred.gp$Median))[2])
      selected_ids <- c(c(1,3,6,9,12), "overall_RMSE")
      df <- data.frame(ID = selected_ids, LST = c(individual_RMSE,overall_RMSE))
      iteration_list[[i]] <- df
    }
    all_data <- do.call(rbind, iteration_list)
    # Compute the average LST for each ID
    average_LST <- aggregate(LST ~ ID, data = all_data, FUN = mean)
    return(average_LST)
  }
  
  for (fn in cov_lst){
    average_LST <- get_RMSE_by_cov(fn,n_samples = n_samples)
    output_df <- output_df %>% left_join(average_LST,by="ID")
  }
  return(output_df)
}



output_df <- run_cov_experiment(c("exponential","spherical"),n_samples = 10) 
colnames(output_df) <- c("ID", "exponential","spherical")
write.csv(output_df, "cov_experiment.csv")

