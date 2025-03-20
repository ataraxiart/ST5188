# understanding spate

library(dplyr)
library(spate)
library(sf)
library(terra)

# importing the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_train_long.csv")

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

# obtain the center of the subzone
center_x <- (ext$xmax + ext$xmin) / 2
center_y <- (ext$ymax + ext$ymin) / 2

# find the width and height of the subzone
width <- ext$xmax - ext$xmin
height <- ext$ymax - ext$ymin

# find the bigger side of the subzone
side_length <- max(width, height)

# define a square boundary around the subzone
square_ext <- ext(center_x - side_length / 2, center_x + side_length / 2, 
                  ext$ymin, ext$ymax)

# mask & crop accordingly
temp_boundary <- terra::mask(r , vect(changi))
temp_boundary <- terra::crop(temp_boundary, square_ext) 

# aggregate spatially to reduce the number of points
aggregated_temp_boundary <- temp_boundary |>
  aggregate(fact = 10/3, fun = "mean", na.rm = T) 

# extract full grid (including NA values)
full_coord <- as.data.frame(aggregated_temp_boundary, xy = TRUE, na.rm = FALSE)
colnames(full_coord) <- c("x", "y", "LST")

length(unique(full_coord$x)) == length(unique(full_coord$y))  # should be TRUE -> we now have a grid of 103 x 103

# now, we need to map our non-NA LST values to the 103 x 103 grid 
# to attain its index for the Sind argument in spate.mcmc

library(FNN)  # Fast nearest neighbor search

# Find the closest grid point for each observation
Sind <- get.knnx(grid_points, df[, c("x", "y")], k = 1)$nn.index


# prep into matrix format
df <- df |>
  mutate(Coordinates = paste0("(", x, ", ", y, ")")) |>
  select(-c(1,2))

df_wide <- df |>
  tidyr::pivot_wider(names_from = Coordinates, values_from = Value)

spate_matrix <- as.matrix(df_wide[, -1])  # remove Date column
rownames(spate_matrix) <- df_wide$Date    # set time steps as row names

# test_matrix <- spate_matrix[, 1:100]
# test_coords <- coord_matrix[1:100, ]

# HBM with Bayesian Inference with MCMC

## 74 is obtained by taking sqrt(no. of coords) + 1 (bc n has to be even)
## trace = TRUE results in an error
spateMCMC <- spate.mcmc(y = spate_matrix, Sind = 1:5290, n = 74, 
                        seed = 5188, trace = FALSE)

# Forecasting
predict <- spate.predict(y=spate_matrix, tPred=(108:120), coord = coord_matrix, n = 74,
                         spateMCMC=spateMCMC, Nsim = 100,
                         BurnIn = 10, DataModel = "Normal", seed=4)

Pmean <- apply(predict,c(1,2),mean)
Psd <- apply(predict,c(1,2),sd)

colnames(Pmean) <- colnames(spate_matrix)
# GLMM with MLE



