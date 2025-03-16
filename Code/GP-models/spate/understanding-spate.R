# understanding spate

library(dplyr)
library(spate)

# importing the data
df <- read.csv("/Users/amiraazad/Documents/GitHub/ST5188/Data/Final/final_CHANGI_long.csv")
head(df)

df <- df |>
  group_by(period) |>
  mutate(time_index = as.integer(factor(period, levels = unique(period))))

y <- df$avg_LST; x <- model.matrix(~1, data = df)
num_space <- length(unique(df$x)) # number of spatial points
num_time <- length(unique(df$time_index)) # number of time steps

# chose the MCMC method
mcmc_settings <- list(nIter = 5000,  # number of MCMC iterations
                      burnIn = 1000, # burn-in period
                      adapt = TRUE)  # adaptive MCMC


set.seed(5188)
# run the spatio-temporal model - estimates the parameters using the MCMC method
mcmc_results <- spate.mcmc(y = y, X = X, N = N, T = time,
                           nIter = 5000, burnIn = 1000, adapt = TRUE)

summary(mcmc_results)
plot(mcmc_results)
par(mfrow = c(3, 3))  # Arrange plots in a grid
for (i in 1:9) {
  plot(mcmc_results$theta[, i], type = "l", main = paste("Param", i))
}

post_means <- colMeans(mcmc_results$theta)
print(post_means)

image(mcmc_results$theta, main = "Spatial Posterior Distribution")
plot(1:T, apply(mcmc_results$theta, 2, mean), type = "l", main = "Time Series of Parameters")
