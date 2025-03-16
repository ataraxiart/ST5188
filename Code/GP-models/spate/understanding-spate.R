# understanding spate

library(dplyr)
library(spate)

# importing the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_train_long.csv")
# extract unique coordinates in the same order as they appear in the matrix
coord_matrix <- as.matrix(df %>% select(x, y) %>% distinct())

# prep into matrix format
df <- df |>
  mutate(Coordinates = paste0("(", x, ", ", y, ")")) |>
  select(-c(1,2))

df_wide <- df |>
  tidyr::pivot_wider(names_from = Coordinates, values_from = Value)

spate_matrix <- as.matrix(df_wide[, -1])  # remove Date column
rownames(spate_matrix) <- df_wide$Date    # set time steps as row names

# HBM with Bayesian Inference with MCMC
spateMCMC <- spate.mcmc(y = spate_matrix, coord = coord_matrix, n = 5290, Nmc = 100, BurnIn = 20, seed = 5188, trace = TRUE)

# GLMM with MLE


