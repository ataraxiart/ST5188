# script to apply extract_impute function on all images

combine_img <- function(subzone, landsat_no) {
  # list all pre-existing RDS files in the target directory
  existing_rds <- list.files("../Data/Misc/SavedRDS", pattern = "\\.RDS$", full.names = TRUE)
  
  # remove pre-existing RDS files if any exist
  if(length(existing_rds) > 0) {
    file.remove(existing_rds)
  }
  
  # initialise skipped files list
  assign("skipped_files", character(0), envir = .GlobalEnv)
  
  # retrieve all tif files
  ls_img <- list.files(paste0("../Data/Landsat/GEE_landsat", landsat_no), pattern = "\\.tif$", full.names = TRUE)
  
  # apply extract_impute function on all images
  lapply(ls_img, function(img) extract_impute(img, subzone))
  
  # retrieve all imputed data files
  rds_img <- list.files("../Data/Misc/SavedRDS", pattern = "\\.RDS$", full.names = TRUE)
  rds_df_combined <- bind_rows(lapply(rds_img, readRDS))
  
  if (length(skipped_files) > 0) {
    skipped_df <- data.frame(skipped_files = skipped_files)
    write.csv(skipped_df, paste0("../Data/Final/landsat", landsat_no, "/", subzone, "_skipped_files.csv"), row.names = FALSE)
    print(paste0("Skipped files list saved as: ", subzone, "_skipped_files.csv"))
  }
  
  print(paste0("Data is combined! Ready for merging and temporal aggregating!"))
  return(rds_df_combined)
}
