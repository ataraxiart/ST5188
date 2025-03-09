count_NA <- function(img, subzone) {
  
  date <- as.Date(str_extract(img, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
  
  boundary <- st_read("../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = TRUE) |>
    select(PLN_AREA_N, geometry) |>
    filter(PLN_AREA_N == subzone) |>
    st_union() |>
    st_transform("EPSG:4326") |>
    st_sf()
  
  centroid <- st_centroid(boundary)
  x_mid <- st_coordinates(centroid)[1, 1]
  y_mid <- st_coordinates(centroid)[1, 2]
  
  r <- rast(img) |>
    project("EPSG:4326")
  
  template <- rast("../Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |>
    project("EPSG:4326")
  r_aligned <- resample(r, template)
  
  min_temp <- global(terra::mask(r_aligned, vect(boundary)), fun = "min", na.rm = TRUE)[1, 1]
  min_temp <- ifelse(min_temp <= 18 | is.na(min_temp), 19, min_temp)
  r_aligned[is.na(r_aligned)] <- 0
  
  temp_boundary <- terra::mask(r_aligned, vect(boundary))
  temp_boundary <- terra::crop(temp_boundary, ext(boundary))
  
  aggregated_temp_boundary <- temp_boundary |>
    aggregate(fact = 10/3, fun = "mean", na.rm = TRUE)
  
  aggregated_temp_boundary <- ifel(aggregated_temp_boundary$LST < min_temp, 0, aggregated_temp_boundary$LST)
  
  df_boundary <- terra::as.data.frame(aggregated_temp_boundary, xy = TRUE, na.rm = TRUE) |>
    mutate(LST = ifelse(LST == 0, NA, LST),
           date = date)
  
  na_regions <- df_boundary |>
    filter(is.na(LST)) |>
    mutate(region = case_when(
      x < x_mid & abs(y - y_mid) <= (max(y) - min(y)) * 0.25 ~ "West",
      x > x_mid & abs(y - y_mid) <= (max(y) - min(y)) * 0.25 ~ "East",
      y > y_mid & abs(x - x_mid) <= (max(x) - min(x)) * 0.25 ~ "North",
      y < y_mid & abs(x - x_mid) <= (max(x) - min(x)) * 0.25 ~ "South",
      TRUE ~ "Central"
    ))
  
  na_counts <- na_regions |>
    group_by(region) |>
    summarise(na_count = n()) |>
    arrange(desc(na_count))
  
  max_region <- na_counts$region[which.max(na_counts$na_count)]
  
  results <- data.frame(subzone, date, image_name = basename(img), max_region, na_counts)
  saveRDS(results, file = paste0("../Data/Misc/SavedRDS/", gsub("\\.tif$", "", basename(img)), ".RDS"))
  
  cat(sprintf("The maximum number of NA values is in the %s region.\n", max_region))
}
