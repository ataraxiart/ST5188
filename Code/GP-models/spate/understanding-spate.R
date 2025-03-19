# understanding spate

library(dplyr)
library(spate)

# importing the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/TT Split/changi_train_long.csv")
coord_matrix <- as.matrix(df %>% select(x, y) %>% distinct())

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
spateMCMC <- spate.mcmc(y = spate_matrix, coord = coord_matrix, n = 74, 
                        seed = 5188, trace = FALSE)

# Forecasting
predict <- spate.predict(y=spate_matrix, tPred=(108:120), coord = coord_matrix, n = 74,
                         spateMCMC=spateMCMC, Nsim = 100,
                         BurnIn = 10, DataModel = "Normal",seed=4)

Pmean <- apply(predict,c(1,2),mean)
Psd <- apply(predict,c(1,2),sd)

# GLMM with MLE


