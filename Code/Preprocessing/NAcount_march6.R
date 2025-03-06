# Function to calculate the number of NA values in a Landsat image
countNA <- function(img, subzones, landsat_no) {
  
  results <- data.frame()
  
  for (subzone in subzones) {
    date <- as.Date(str_extract(img, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
    
    # Import subzone boundary 
    boundary <- st_read("../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = TRUE) |>
      select(PLN_AREA_N, geometry) |>
      filter(PLN_AREA_N == subzone) |>
      st_union() |>
      st_transform("EPSG:4326") |>
      st_sf()
    
    # Import image
    r <- rast(img) |>
      project("EPSG:4326")
    
    # Align to a common grid (using LST_Singapore_2013-04-24.tif as template)
    template <- rast("../Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |>
      project("EPSG:4326")
    r_aligned <- resample(r, template)
    
    # Save the min. temp of region
    min_temp <- global(terra::mask(r_aligned, vect(boundary)), fun = "min", na.rm = TRUE)[1, 1]
    min_temp <- ifelse(min_temp <= 18 | is.na(min_temp), 19, min_temp) # To account for cases where temperatures are negative or extremely low
    
    # Set NA values in image to be 0
    r_aligned[is.na(r_aligned)] <- 0
    
    # Mask & crop to subzone region
    temp_boundary <- terra::mask(r_aligned, vect(boundary))
    temp_boundary <- terra::crop(temp_boundary, ext(boundary))
    
    # Aggregate spatially to reduce the number of points
    aggregated_temp_boundary <- temp_boundary |>
      aggregate(fact = 10/3, fun = "mean", na.rm = TRUE)
    
    # Temperature lower than min_temp -> set to 0 i.e. take it as missing data
    aggregated_temp_boundary <- ifel(aggregated_temp_boundary$LST < min_temp, 0, aggregated_temp_boundary$LST)
    
    # Extract as dataframe & set 0 values to be NA
    df_boundary <- terra::as.data.frame(aggregated_temp_boundary, xy = TRUE, na.rm = TRUE) |>
      mutate(LST = ifelse(LST == 0, NA, LST),
             date = date)
    
    # Compute total pixels and NA percentage
    total_pixels <- terra::ncell(aggregated_temp_boundary)
    na_count <- sum(is.na(df_boundary$LST))
    na_percentage <- (na_count / total_pixels) * 100
    
    # Store results in data frame
    results <- rbind(results, data.frame(subzone, na_count, total_pixels, na_percentage, date))
  }
  
  return(results)
}

# Function to create plot for a specific subzone for NA counts
create_subzone_plot <- function(data, subzone_name) {
  subzone_data <- data |>
    filter(subzone == subzone_name)
  
  ggplot(subzone_data, aes(x = formatted_date, y = na_count, fill = landsat_number)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
      title = paste("NA Count for", subzone_name),
      x = "Date",
      y = "NA Count",
      fill = "Landsat Number"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "top"
    )
}

# Function to create summary statistics based on NA counts
create_subzone_summary <- function(data, subzone_name) {
  subzone_data <- data |>
    filter(subzone == subzone_name)
  
  summary_stats <- subzone_data |>
    group_by(landsat_number) |>
    summarize(
      Mean_NA = mean(na_count),
      Median_NA = median(na_count),
      Min_NA = min(na_count),
      Max_NA= max(na_count),
      Average_NA_Percentage = mean(na_percentage)
    )
  
  return(summary_stats)
}