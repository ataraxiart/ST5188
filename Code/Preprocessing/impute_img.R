# script for imputation of NA values

impute_img <- function(df_boundary) {

  # extract known data points
  known_points <- df_boundary[!is.na(df_boundary$LST), ]
  x_known <- known_points$x
  y_known <- known_points$y
  z_known <- known_points$LST
  
  # extract locations where LST is missing
  missing_points <- df_boundary[is.na(df_boundary$LST), ]
  x_missing <- missing_points$x
  y_missing <- missing_points$y
  
  # converting data to spatial format
  coordinates(known_points) <- ~ x + y
  coordinates(missing_points) <- ~ x + y
  
  # fit the Kriging model and perform interpolation
  vgm_model <- variogram(LST ~ 1, known_points)
  fit_model <- fit.variogram(vgm_model, vgm("Sph"))
  
  if (nrow(vgm_model) == 0) {
    warning("Variogram calculation failed - not enough data.")
    return(NULL)
  }
  
  # use default variogram if fitting fails
  if (inherits(fit_model, "try-error")) {
    warning("Variogram fitting failed. Using default parameters.")
    fit_model <- vgm(psill = 1, model = "Sph", range = 500, nugget = 0)
  }
  
  # ensure variogram range is positive
  if (fit_model$range[2] < 0) {
    warning("Negative variogram range detected. Setting default positive range.")
    fit_model$range[2] <- max(dist(coordinates(known_points))) / 3
  }
  
  kriging_result <- krige(LST ~ 1, known_points, missing_points, model = fit_model)
  
  # update dataframe with imputed values
  df_boundary$LST[is.na(df_boundary$LST)] <- kriging_result$var1.pred
  
  return(df_boundary)
}