countNA <- function(img, subzones, landsat_no) {
  results <- data.frame()
  
  for (subzone in subzones) {
    date <- as.Date(str_extract(img, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
    
    boundary <- st_read("../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = TRUE) |>
      select(PLN_AREA_N, geometry) |>
      filter(PLN_AREA_N == subzone) |>
      st_union() |>
      st_transform("EPSG:4326") |>
      st_sf()
    
    r <- rast(img) |>
      project("EPSG:4326")
    
    template <- rast("../Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |>
      project("EPSG:4326")
    r_aligned <- resample(r, template)
    
    # set a minimum temperature of 19 degrees
    min_temp <- global(terra::mask(r_aligned, vect(boundary)), fun = "min", na.rm = TRUE)[1, 1]
    min_temp <- ifelse(min_temp <= 18 | is.na(min_temp), 19, min_temp)
    r_aligned[is.na(r_aligned)] <- 0
    
    # mask and crop by subzone boundary
    temp_boundary <- terra::mask(r_aligned, vect(boundary))
    temp_boundary <- terra::crop(temp_boundary, ext(boundary))
    
    # aggregate spatially
    aggregated_temp_boundary <- temp_boundary |>
      aggregate(fact = 10/3, fun = "mean", na.rm = TRUE)
    
    # temperature lower than min_temp -> set to 0 i.e. take it as missing data
    aggregated_temp_boundary <- ifel(aggregated_temp_boundary$LST < min_temp, 0, aggregated_temp_boundary$LST)
    
    # extract as dataframe & set 0 values to be NA
    df_boundary <- terra::as.data.frame(aggregated_temp_boundary, xy = TRUE, na.rm = TRUE) |>
      mutate(LST = ifelse(LST == 0, NA, LST),
             date = date)
    
    # compute total pixels and relative % of missing values
    total_pixels <- terra::ncell(aggregated_temp_boundary)
    na_count <- sum(is.na(df_boundary$LST))
    na_percentage <- (na_count / total_pixels) * 100
    
    # store results in data frame
    results <- rbind(results, data.frame(subzone, na_count, total_pixels, na_percentage, date, image_name = basename(img)))
  }
  
  return(results)
}

# summary statistics by NA values
create_subzone_summary <- function(data, subzone_name) {
  subzone_data <- data |>
    filter(subzone == subzone_name)
  
  summary_stats <- subzone_data |>
    summarize(
      Mean_NA = mean(na_count),
      Min_NA = min(na_count),
      Max_NA = max(na_count),
      Average_NA_Percentage = mean(na_percentage)
    )
  
  return(summary_stats)
}

# probability distribution by NA percentage
na_probdist <- function(data) {
  data |>
    group_by(subzone) |>
    mutate(probability = na_count / sum(na_count, na.rm = TRUE)) |>
    ungroup()
}

# plots for probability distribution
plot_na_probdist <- function(data, subzone_name) {
  subzone_data <- data |>
    filter(subzone == subzone_name)
  
  ggplot(subzone_data, aes(x = na_percentage, y = probability, fill = subzone)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Probability Distribution of NA Values for", subzone_name),
         x = "NA Percentage",
         y = "Probability") +
    theme_minimal() +
    scale_fill_brewer(palette = "Dark2") +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}