# script to combine values from each image into one big dataframe

combine_images <- function(subzone) {
  
  extract_impute <- function(img) {
  
    # import subzone boundary 
    boundary <- st_read("../../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = T) |>
      select(PLN_AREA_N, geometry) |>
      filter(PLN_AREA_N == subzone) |>
      st_union() |>
      st_transform("EPSG:4326") |>
      st_sf()
    
    # import image
    r <- rast(img) |>
      project("EPSG:4326") 
    date <- as.Date(str_extract(img, "\\d{4}-\\d{2}-\\d{2}"))
    
    # save the min. temp of region
    min_temp <- global(terra::mask(r, vect(boundary)), fun = "min", na.rm = TRUE)[1,1]
    
    # set NA values in image to be 0
    r[is.na(r)] <- 0
   
    # mask & crop to subzone region
    temp_boundary <- terra::mask(r, vect(boundary))
    ext <- ext(boundary)
    temp_boundary <- terra::crop(temp_boundary, ext) 
    
    # aggregate spatially to reduce the number of points
    aggregated_temp_boundary <- temp_boundary |>
      aggregate(fact = 3, fun = "mean", na.rm = T) 
    
    # temperature lower than min_temp -> set to 0 i.e. take it as missing data
    aggregated_temp_boundary <- ifel(aggregated_temp_boundary$LST < min_temp, 0, aggregated_temp_boundary$LST)
    
    # extract as dataframe & set 0 values to be NA
    df_boundary <- terra::as.data.frame(aggregated_temp_boundary, xy = TRUE, na.rm = TRUE) |>
      mutate(LST = ifelse(LST == 0, NA, LST),
             date = date)
    
    # call upon imputation function - swa
    
    
    # save imputed df as RDS file
    saveRDS(df, file = paste0("../../Data/Misc/SavedRDS/", img, ".RDS"))
    print(paste0("Successfully imputed", img))
  }
  
  # apply extract_impute function on all images
  ls_img <- list.files("../../Data/Landsat/GEE_landsat8")
  
  
  # combine all RDS into one big dataframe
  
  
  write.csv(df, paste0("../../Data/Final/", subzone, ".csv"), row.names = FALSE)
  print(paste0(subzone, ".csv is ready!"))
}
