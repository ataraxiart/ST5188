library(dplyr)
library(spate)
library(sf)
library(terra)
library(gridExtra)
library(reshape2)

# importing the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_train_long.csv")
df <- df |>
  mutate(coord_key = paste0(round(x, 6), "_", round(y, 6))) # for identification

# obtaining grid of subzones
## import in a .tiff image
r <- rast("/Users/amiraazad/Documents/GitHub/ST5188/Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |>
  project("EPSG:4326")

## import in the boundary of each subzone - we will focus on changi's grid first
changi <- st_read("/Users/amiraazad/Documents/GitHub/ST5188/Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp", quiet = T) |>
  dplyr::select(PLN_AREA_N, geometry) |>
  filter(PLN_AREA_N == "CHANGI") |>
  st_union() |>
  st_transform("EPSG:4326") |>
  st_sf()

# get the end points of the subzone boundary
ext <- ext(changi)  

# find the width and height of the subzone
width <- ext$xmax - ext$xmin
height <- ext$ymax - ext$ymin

# find the max length of the sides of the boundary and round down!
diff_length <- ceiling(max(width, height) * 1000) / 1000 - floor(min(width, height) * 1000) / 1000

# define a square boundary around the subzone
square_ext <- ext(ext$xmin, ext$xmax + diff_length, 
                  ext$ymin, ext$ymax + (diff_length - (height - width)))
plot(square_ext)

# mask & crop accordingly
temp_boundary <- terra::mask(r , vect(changi))
temp_boundary <- terra::crop(temp_boundary, ext) 

# aggregate spatially to reduce the number of points
aggregated_temp_boundary <- temp_boundary |>
  aggregate(fact = 10/3, fun = "mean", na.rm = T) 
plot(aggregated_temp_boundary)

# extend the image so that it will have a square boundary (following square_ext)
extend_img <- extend(aggregated_temp_boundary, square_ext)
plot(extend_img)

# extract full grid (including NA values)
full_coord <- as.data.frame(extend_img, xy = TRUE, na.rm = FALSE)
colnames(full_coord) <- c("x", "y", "LST")

length(unique(full_coord$x)) == length(unique(full_coord$y))  # should be TRUE -> we now have a grid of 104 x 104

# now, we need to map our non-NA LST values to the 104 x 104 grid 
# to attain its index for the Sind argument in spate.mcmc
full_coord <- full_coord |>
  mutate(coord_key = paste0(round(x, 6), "_", round(y, 6)),
         index = row_number())

# check if the index given are right
check_coord <- full_coord |>
  mutate(row = floor((index - 1) / 104) + 1,
         col = ((index - 1) %% 104) + 1)

ggplot(check_coord, aes(col, row, fill = LST)) +
  geom_tile() +
  scale_fill_gradientn(colors = terrain.colors(100)) +
  theme_minimal() +
  labs(title = "Matrix Index Order Check", x = "Column", y = "Row") +
  scale_y_reverse()  # flip Y-axis to make ggplot match standard matrix format


indexed_df <- left_join(df, full_coord[, c("coord_key", "index")], by = "coord_key")
# any(is.na(indexed_df$index))
Sind <- unique(indexed_df$index) # length(Sind) should be 5290 = no. of observed coordinates

# prep into matrix format
df_wide <- indexed_df |>
  dplyr::select(-c(1, 2, 5)) |>
  tidyr::pivot_wider(names_from = index, values_from = Value)

spate_matrix <- as.matrix(df_wide[, -1])  # remove Date column
colnames(spate_matrix) <- paste0("V", seq(1, ncol(spate_matrix))) # change colnames into V1, V2, ..., V5290

m <- mean(spate_matrix)
sd <- sd(spate_matrix)
spate_matrix.scaled <- scale(spate_matrix, center = TRUE, scale = TRUE) # scale because ...

# HBM with Bayesian Inference with MCMC
spateMCMC <- spate.mcmc(y = spate_matrix.scaled,
                        n = 104, # our grid size is 104 x 104
                        Sind = Sind, # a vector of length 104 indicating the grid cells in which our observations lie
                        Nmc = 1000, BurnIn = 500, # to be updated later
                        trace = FALSE, # setting trace = TRUE results in error
                        seed = 5188)
spateMCMC

# Forecasting with HBM
MCMC.predict <- spate.predict(y = spate_matrix.scaled, 
                         Sind = Sind, # have to specify bc spate_matrix is not n x n
                         n = 104, 
                         tPred = (108:136), # from Jan-Feb 2020 to Nov-Dec 2024
                         spateMCMC = spateMCMC, 
                         Nsim = 1000, BurnIn = 500, 
                         DataModel = "Normal", 
                         seed = 5188)

# rescale forecast values
Pmean <- apply(predict,c(1,2), mean) # take the mean because we have 100 samples
Pmean <- Pmean[, Sind] # cut out the other points of the grid that spate.predict also interpolates at
Pmean_unscaled <- (Pmean * sd) + m

plot_heatmap <- function(Pmean_row, Sind, row_index, dates) {
  
  df_matrix <- melt(Pmean_row) |>
    mutate(index = Sind) |>
    rename(LST = value)
  df_matrix$index <- Sind
  
  check_Pmean <- df_matrix |>
    mutate(row = floor((index - 1) / 104) + 1,
           col = ((index - 1) %% 104) + 1)
  
  heatmap_plot <- ggplot(check_Pmean, aes(col, row, fill = LST)) +
    geom_tile() +
    scale_fill_gradientn(colors = terrain.colors(100)) +
    theme_minimal() +
    theme(
      text = element_text(size = 5),       # Reduce all text size
      axis.text = element_text(size = 5),  # Reduce axis text size
      plot.title = element_text(size = 5) # Reduce title size
    ) +
    labs(title = paste0("Predicted Field at t =", dates[row_index]),
         x = "Column", y = "Row") +
    scale_y_reverse() 
  
  return(heatmap_plot)
}

dates <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/final_CHANGI_long.csv") |>
  dplyr::select(period)
dates <- unique(dates)[108:nrow(dates), ]

time_steps <- 1:nrow(Pmean_unscaled)  
heatmaps <- lapply(time_steps, function(i) plot_heatmap(Pmean_unscaled[i, ], Sind, i, dates))
grid.arrange(grobs = heatmaps, ncol = 5)

# GLMM with MLE
