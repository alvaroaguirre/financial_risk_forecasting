library(animation)

mtx <- matrix(NA, nrow = 101, ncol = 1000)
mtx[1,] <- 500
for (i in 1:1000) { 
  for (j in 2:101) {
      mtx[j,i] <- mtx[j-1,i] + rnorm(1, mean = 0, sd = 2.5)
  }
}

random_sims <- mtx[,sample(1:1000,250)]

saveGIF(
  {
    for (i in 1:200) {
      matplot(random_sims[,1:i], type = "l", lwd = 1, xlim = c(0, 100), ylim = c(400, 600),
              main = "Random Walk of 250 simulations", col = 1:i, lty = 1, ylab = "Price", xlab = "Time")
    }
  },
  loop=TRUE,interval=0.05, movie.name = "random_walk.gif",
  clean = TRUE
)

