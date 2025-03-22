# call in packages needed
library(dplyr)
library(spate)
library(sf)
library(terra)
library(gridExtra)
library(reshape2)
library(ggplot2)

# hello! this code is to get a sense of how spate works. 
# we will work with CHANGI first before generalising/applying it to JE and JW later on.

# import the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_train_long.csv")

# create a unique key for each row for merging later on
df <- df |>
  mutate(coord_key = paste0(round(x, 6), "_", round(y, 6)))

# spate works with grid i.e. n x n 
# so we need to extend our CHANGI subzone boundary such that it'll be a grid
# right now, its 103 x 86 -> our aim is to get it to 104 x 104 (n has to be even for spate to work)

# import in one .tiff image
r <- rast("/Users/amiraazad/Documents/GitHub/ST5188/Data/Landsat/GEE_landsat8/LST_Singapore_2013-04-24.tif") |>
  project("EPSG:4326")

# import in the boundary of CHANGI's subzone
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

# find the max length of the sides of the boundary and round up
diff_length <- ceiling((max(width, height) - min(width, height)) * 1000) / 1000

# attain a square boundary around the subzone
square_ext <- ext(ext$xmin, ext$xmax + diff_length, 
                  ext$ymin, ext$ymax + (diff_length - (height - width)))
plot(square_ext)

# mask, crop and aggregate CHANGI according to the pre-processing steps (to ensure the coordinates remain the same)
temp_boundary <- terra::mask(r , vect(changi))
temp_boundary <- terra::crop(temp_boundary, ext) 
aggregated_temp_boundary <- temp_boundary |> aggregate(fact = 10/3, fun = "mean", na.rm = T) 
plot(aggregated_temp_boundary)

# extend `aggregated_temp_boundary` so that it will have a square boundary (following square_ext)
extend_img <- extend(aggregated_temp_boundary, square_ext)
plot(extend_img) # we now have CHANGI as a grid!

# extract all coordinates from square CHANGI (including NA values) to get the coordinates of the grid
full_coord <- as.data.frame(extend_img, xy = TRUE, na.rm = FALSE)
colnames(full_coord) <- c("x", "y", "LST") # can ignore LST values as the main thing we want is the coordinates

# check!
length(unique(full_coord$x)) == length(unique(full_coord$y)) # should be TRUE -> we now have a grid of 104 x 104

# now, we need to map our non-NA LST values to the 104 x 104 grid to attain its grid index
# i.e. which part of the grid does the LST values belong to?
# note that each image can be represented by a matrix because they're just pixels

# by observing full_coord, we can see that the values are recorded top to bottom, left to right, which is how a matrix works!
# so we can use row_number() to assign their grid index
# for example, let's say `extend_img` has the following image:
# e.g [1  2 3   4  5
#      6  7 8   9  10
#      11 12 13 14 15
#      16 17 18 19 20
#      21 22 23 24 25]
# the numbers in the matrix above represent the grid index i.e. their position in the n x n grid matrix!

full_coord <- full_coord |>
  mutate(coord_key = paste0(round(x, 6), "_", round(y, 6)), # create a key column for merging with df
         grid_index = row_number()) # assign grid index

# check if the grid index given are right - it seems legit
check_coord <- full_coord |>
  mutate(row = floor((grid_index - 1) / 104) + 1,
         col = ((grid_index - 1) %% 104) + 1)

ggplot(check_coord, aes(col, row, fill = LST)) +
  geom_tile() +
  scale_fill_gradientn(colors = terrain.colors(100)) +
  theme_minimal() +
  labs(title = "Matrix Index Order Check", x = "Column", y = "Row") +
  scale_y_reverse()  # flip y-axis to make ggplot match standard matrix format

# merge full_coord together with df to assign grid index to all coordinates in df
# so we know exactly where each LST value belongs to in the n x n CHANGI
indexed_df <- left_join(df, full_coord[, c("coord_key", "grid_index")], by = "coord_key")
# any(is.na(indexed_df$index)) # FALSE

# now, we can tell spate which grid index has an observed value!
Sind <- unique(indexed_df$grid_index) # length(Sind) should be 5290 = no. of observed coordinates

# change indexed_df into a matrix of dim T x no. of obs. coordinates (124 x 5290)
df_wide <- indexed_df |>
  dplyr::select(-c(1, 2, 5)) |>
  tidyr::pivot_wider(names_from = grid_index, values_from = Value)

# remove Date column
spate_matrix <- as.matrix(df_wide[, -1])

# change colnames into V1, V2, ..., V5290 just because (i was following the sample code's output)
colnames(spate_matrix) <- paste0("V", seq(1, ncol(spate_matrix))) 

# apparently spate takes in scaled values so we need to scale our matrix
m <- mean(spate_matrix)
sd <- sd(spate_matrix)
spate_matrix.scaled <- scale(spate_matrix, center = TRUE, scale = TRUE)

# now, we try to estimate our parameters using HBM with Bayesian Inference with MCMC
spateMCMC <- spate.mcmc(y = spate_matrix.scaled,
                        n = 104, # our grid size is 104 x 104
                        Sind = Sind, # a vector of length 104 indicating the grid cells in which our observations lie
                        Nmc = 1000, BurnIn = 200, # to be updated later
                        trace = FALSE, # setting trace = TRUE results in error
                        seed = 5188)
spateMCMC
# thoughts: from the plot plotted by running spate.mcmc, you can see the parameters arent converging enough :(
# we might need to set initial values later on

# let's try forecasting with HBM
MCMC.predict <- spate.predict(y = spate_matrix.scaled, 
                         Sind = Sind, # still have to specify because spate_matrix is not n x n
                         n = 104, 
                         tPred = (125:136), # 12 time points: from Jan-Feb 2023 to Nov-Dec 2024
                         spateMCMC = spateMCMC, # take in the parameters estimated from spate.mcmc
                         Nsim = 1000, BurnIn = 200, # to be updated later 
                         DataModel = "Normal", 
                         seed = 5188)

# rescale forecast values
Pmean <- apply(MCMC.predict, c(1,2), mean) # take the mean because we have >1 samples
Pmean <- Pmean[, Sind] # cut out the other cells of the grid that spate.predict also interpolates at
Pmean_unscaled <- (Pmean * sd) + m

# visualising results
plot_heatmap <- function(Pmean_row, Sind, row_index, dates) {
  
  df_matrix <- melt(Pmean_row) |>
    mutate(grid_index = Sind) |>
    rename(LST = value)
  df_matrix$grid_index <- Sind
  
  check_Pmean <- df_matrix |>
    mutate(row = floor((grid_index - 1) / 104) + 1,
           col = ((grid_index - 1) %% 104) + 1)
  
  heatmap_plot <- ggplot(check_Pmean, aes(col, row, fill = LST)) +
    geom_tile() +
    scale_fill_gradientn(colors = terrain.colors(100)) +
    theme_minimal() +
    theme(
      text = element_text(size = 5),
      axis.text = element_text(size = 5),
      plot.title = element_text(size = 5)
    ) +
    labs(title = paste0("Predicted LST at t = ", dates[row_index]),
         x = "Column", y = "Row") +
    scale_y_reverse() 
  
  return(heatmap_plot)
}

test <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_test_long.csv")
test_wide <- test |>
  mutate(coord_key = paste0(round(x, 6), "_", round(y, 6))) |>
  dplyr::select(-c(1, 2)) |>
  tidyr::pivot_wider(names_from = coord_key, values_from = Value)

dates <- test_wide$Date

time_steps <- 1:nrow(Pmean_unscaled)  
heatmaps <- lapply(time_steps, function(i) plot_heatmap(Pmean_unscaled[i, ], Sind, i, dates))
grid.arrange(grobs = heatmaps, ncol = 6)

# sd - weird
# Psd <- apply(predict,c(1,2), sd)
# Psd <- Psd[, Sind]
# Psd_unscaled <- (Psd * sd) + m

# calculate RMSE of predictions
test_wide <- as.matrix(test_wide[-1]) # remove Date column and make it into a matrix

rmse_matrix <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))  # compute RMSE over the entire matrix
}

rmse_value <- rmse_matrix(test_wide, Pmean_unscaled)
print(paste("Overall RMSE:", rmse_value)) # 4.05

########################################################################################

# having major problems with this
# estimating with GLMM with MLE
# real.fft.TS requires the data to be in T x n*n and no NA values (problematic!)

time <- data.frame(Date = unique(indexed_df$Date),
                   time_index = 1:124)

indexed_df <- indexed_df |>
  left_join(time, by = "Date")

# create the full matrix
n_time <- length(unique(indexed_df$time))
n_spate <- nrow(full_coord)

## initialize full matrix with NA
grid_matrix <- matrix(NA, nrow = n_time, ncol = n_spate)

for (i in 1:nrow(indexed_df)) {
  t <- indexed_df$time_index[i]
  idx <- indexed_df$grid_index[i]
  grid_matrix[t, idx] <- indexed_df$Value[i]
}

# initial values for optim - used the values given in the sample
parI <- c(rho0 = 0.2, sigma2 = 0.1, zeta = 0.25, rho1 = 0.01, gamma = 1,
          alpha = 0.3, muX = 0, muY = 0, tau2 = 0.005)

# transform to log-scale
logInd = c(1, 2, 3, 4, 5, 9)
parI[logInd] <- log(parI[logInd])

grid_matrix[is.na(grid_matrix)] <- 0 # no NA values allowed :(

wFT <- spate::real.fft.TS(grid_matrix, n = 104, T = 124)

# estimate with GLMM/MLE
spateMLE <- optim(par = parI,
                  loglike,
                  control = list(trace = TRUE, maxit = 1000),
                  wFT = wFT, method = "L-BFGS-B",
                  lower = c(-10, -10, -10, -10, -10, 0, -0.5, -0.5, -10),
                  upper = c(10, 10, 10, 10, 10, pi/2, 0.5, 0.5, 10),
                  negative = TRUE,
                  logScale = TRUE,
                  logInd = c(1,2,3,4,5,9),
                  hessian = TRUE,
                  n = 104, T = 124)

# this part i followed the sample code
mle <- spateMLE$par
mle[logInd] <- exp(mle[logInd])
sd = sqrt(diag(solve(spateMLE$hessian)))

# calculate confidence intervals
MleConfInt <- data.frame(array(0,c(4,9)))
colnames(MleConfInt) <- names(par)
rownames(MleConfInt) <- c("True","Estimate","Lower","Upper")
MleConfInt[1,] <- par
MleConfInt[2,] <- mle
MleConfInt[3,] <- spateMLE$par-2*sd
MleConfInt[4,] <- spateMLE$par+2*sd
MleConfInt[c(3,4),logInd] <- exp(MleConfInt[c(3,4),logInd])
round(MleConfInt,digits=3)

# iirc, one of the parameters didnt get estimated
# also, not sure if changing NA values to 0 is a good idea

