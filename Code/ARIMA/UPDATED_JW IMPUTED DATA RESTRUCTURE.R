# RESTRUCTURE

# Load required libraries
library(dplyr)
library(tidyr)

imputed_df <- read.csv("../../Data/Final/JW Split/final_imputed_JURONG_WEST.csv")
# Extract unique values for x, y, and date
unique_x <- unique(imputed_df$x)
unique_y <- unique(imputed_df$y)
years <- 2000:2024
months <- c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", "Sep-Oct", "Nov-Dec")
date_levels <- as.vector(outer(months, years, paste, sep = " "))
date_levels <- gsub("^ ", "", date_levels) # Clean up any leading spaces

# Filter to only keep relevant dates (Mar-Apr 2000 to Nov-Dec 2024)
date_levels <- date_levels[which(date_levels == "Mar-Apr 2000"):which(date_levels == "Nov-Dec 2024")]

# Step 2: Convert the Date column to a factor with the correct levels
imputed_df <- imputed_df %>%
  mutate(Date = factor(Date, levels = date_levels))

# Generate all possible combinations of x, y, and period
expanded_df <- expand.grid(
  x = unique_x,
  y = unique_y,
  Date = date_levels
)

final_df <- imputed_df %>%
  arrange(Date, x) %>% 
  group_by(Date)

# Check the result
print(head(final_df))

# Save the restructured dataset to a file (if necessary)
write.csv(final_df, "jw_imp_final.csv", row.names = FALSE)


