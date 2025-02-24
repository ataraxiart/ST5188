# script to apply extract_impute function on all images

combine_img <- function(subzone) {
  # initialise skipped files list
  assign("skipped_files", character(0), envir = .GlobalEnv)
  
  # retrieve all tif files
  ls_img <- list.files("../Data/Landsat/GEE_landsat8", pattern = "\\.tif$", full.names = TRUE)
  
  # apply extract_impute function on all images
  lapply(ls_img, function(img) extract_impute(img, subzone))
  
  # retrieve all imputed data files
  rds_img <- list.files("../Data/Misc/SavedRDS", pattern = "\\.RDS$", full.names = TRUE)
  rds_df_combined <- bind_rows(lapply(rds_img, readRDS))
  
  if (length(skipped_files) > 0) {
    skipped_df <- data.frame(skipped_files = skipped_files)
    write.csv(skipped_df, paste0("../Data/Final/", subzone, "_skipped_files.csv"), row.names = FALSE)
    print(paste0("Skipped files list saved as: ", subzone, "_skipped_files.csv"))
  }
  
  print(paste0("Data is combined and ready to be aggregated temporally!"))
  return(rds_df_combined)
}
