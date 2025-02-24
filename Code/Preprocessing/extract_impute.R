# script to extract LST data from images & impute NA values

extract_impute <- function(img, subzone) {
  
  date <- as.Date(str_extract(img, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
  
  # import subzone boundary 
  boundary <- st_read("../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = T) |>
    select(PLN_AREA_N, geometry) |>
    filter(PLN_AREA_N == subzone) |>
    st_union() |>
    st_transform("EPSG:4326") |>
    st_sf()
  
  # import image
  r <- rast(img) |>
    project("EPSG:4326") 

  # align to a common grid (using LST_Singapore_2013-04-24.tif as template)
  template <- rast("../Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |> 
    project("EPSG:4326")
  r_aligned <- resample(r, template)
  
  # save the min. temp of region
  min_temp <- global(terra::mask(r_aligned, vect(boundary)), fun = "min", na.rm = TRUE)[1,1] 
  min_temp <- ifelse(min_temp <= 18 | is.na(min_temp), 19, min_temp) # to account for cases where temperatures are negative or extremely low
  
  # set NA values in image to be 0
  r_aligned[is.na(r_aligned)] <- 0
 
  # mask & crop to subzone region
  temp_boundary <- terra::mask(r_aligned, vect(boundary))
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
  
  # if entire df_boundary is NA, skip
  if (all(is.na(df_boundary$LST))) {
    print(paste0("Skipping ", basename(img), " — all LST are missing."))
    assign("skipped_files", c(get("skipped_files", envir = .GlobalEnv), basename(img)), envir = .GlobalEnv)
    return(NULL)
    }
  
  # if there's missing values, impute
  if (!(all(is.na(df_boundary$LST))) & any(is.na(df_boundary$LST))) {
    df_boundary <- impute_img(df_boundary)
    }

  saveRDS(df_boundary, file = paste0("../Data/Misc/SavedRDS/", gsub("\\.tif$", "", basename(img)), ".RDS"))
  print(paste0("Successfully extracted and imputed: ", basename(img)))
}
