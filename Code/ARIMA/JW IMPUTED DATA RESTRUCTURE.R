# RESTRUCTURE

# Load required libraries
library(dplyr)
library(tidyr)

imputed_df <- read.csv("../../Data/Final/final_imputed_JURONG_WEST.csv")
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
write.csv(final_df, "../../Data/Final/Imputed_JW_Restructured.csv", row.names = FALSE)


#removing unwanted rows (duplicate values)

# Load the dataset
file_path <- "../../Data/Final/Imputed_JW_Restructured.csv"  
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Remove duplicate "Value" entries but keep the first occurrence
df_unique_values <- df[!duplicated(df$Value), ]

# Save the modified dataset to a new CSV file
output_file <- "../../Data/Final/FINAL_RESTRUCTURED_IMPUTED_JW.csv"
write.csv(df_unique_values, output_file, row.names = FALSE)


#adding the missing values which got removed accidentally
# Load both datasets
original_file <- "../../Data/Final/final_JURONG WEST_long.csv"  
imputed_file <- "../../Data/Final/FINAL_RESTRUCTURED_IMPUTED_JW.csv"  

original <- read.csv(original_file, stringsAsFactors = FALSE)
imputed <- read.csv(imputed_file, stringsAsFactors = FALSE)

# Rename columns in original_df to match imputed_df
colnames(original) <- c("x", "y", "Date", "Value")

# Identify missing rows (rows in original_df but not in imputed_df)
missing_rows <- original[!paste(original$x, original$y, original$Date, original$Value) %in%
                              paste(imputed$x, imputed$y, imputed$Date, imputed$Value), ]

# Append missing rows back to the imputed dataset
final <- rbind(imputed, missing_rows)

# Save the final dataset
final_file <- "../../Data/Final/final_dataset.csv"
write.csv(final, final_file, row.names = FALSE)



