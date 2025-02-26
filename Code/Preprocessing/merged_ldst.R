# script to merge two preprocessed landsat dataframes

merge_ldst <- function(df1, df2) {
  
  merged_df <- bind_rows(df1, df2) |>
    group_by(x, y, date) |>
    summarise(LST = mean(LST, na.rm = TRUE), .groups = "drop")
  
  return(merged_df)
}