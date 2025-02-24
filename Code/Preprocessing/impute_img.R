# script for imputation of NA values

impute_img <- function(df_boundary) {
  
  # convert to spatial object
  coordinates(df_boundary) <- ~ x + y
  
  df_train <- df_boundary[!is.na(df_boundary$LST), ]
  df_pred <- df_boundary[is.na(df_boundary$LST), ]
  
  knn.out <- gstat(formula = LST ~ 1, 
                   locations = df_train, 
                   nmax = 10, # number of nearest neighbours
                   set = list(idp = 0)) # uniform weights
  
  knn.pred <- predict(knn.out, newdata = df_pred)
  
  df_pred$LST <- knn.pred$var1.pred
  
  df_imputed <- rbind(df_train, df_pred)

  return(as.data.frame(df_imputed))
}