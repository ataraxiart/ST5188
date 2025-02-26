eda <- function(df) {
  
  # view basic info
  print(str(df))
  print(summary(df))  
  
  # histogram of avg_LST
  p1 <- ggplot(df, aes(x = avg_LST)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    labs(title = "Distribution of Land Surface Temperature", x = "avg_LST", y = "Count")
  
  print(p1)
  
  # boxplot of avg_LST by period
  p2 <- ggplot(df, aes(x = period, y = avg_LST)) +
    geom_boxplot(fill = "blue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "LST Variation Over Different Periods", x = "Period", y = "avg_LST")
  
  print(p2)
  
  # summary statistics by period
  summary_stats <- df |>
    group_by(period) |>
    summarise(mean_LST = mean(avg_LST, na.rm = TRUE),
              sd_LST = sd(avg_LST, na.rm = TRUE),
              min_LST = min(avg_LST, na.rm = TRUE),
              max_LST = max(avg_LST, na.rm = TRUE))
  
  print(summary_stats)
  
  # time series plot of average LST
  df <- df %>%
    mutate(period_clean = sub("-.*", "", period),  # Keep only first month (for plotting)
           period_date = as.Date(paste0("01-", period_clean, "-", sub(".* ", "", period)), 
                                 format="%d-%b-%Y"))
  
  p3 <- ggplot(df, aes(x = period_date, y = avg_LST)) +
    geom_line(stat = "summary", fun = mean, color = "blue") +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Average LST Over Time", x = "Time Period", y = "Average LST") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(p3)
  
  # spatial distribution of LST using a heatmap
  p4 <- ggplot(df, aes(x = x, y = y, color = avg_LST)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "yellow", high = "red") +
    labs(title = "Spatial Distribution of LST", x = "Longitude", y = "Latitude", color = "LST")
  
  print(p4)
  
  # autocorrelation plot
  acf(df$avg_LST, na.action = na.pass)
}