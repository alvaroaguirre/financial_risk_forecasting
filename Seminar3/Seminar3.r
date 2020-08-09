library(tseries)
library(car)
library(lubridate)

# CDF of 0 under a Standard Normal
pnorm(0)

# CDF of 8 under a Normal with mean 10 and standard deviation 2
pnorm(8, mean = 10, sd = 2)

# Rounding it
round(pnorm(8, mean = 10, sd = 2),3)

# Sequence from -2 to 2, with increments of 0.5
sequence <- seq(-2, 2, 0.5)
round(pnorm(sequence),3)

# Inverse CDF of 0.95 under a Student-t with 3 degrees of freedom
qt(0.95, df = 3)

# Creating a sequence
x <- seq(-3,3,0.1)

# Vector with the CDF
cdf <- pnorm(x)

# Plotting it
plot(x, cdf, type = "l", main = "CDF of a Standard Normal",
    lwd = 3, las = 1, col = "blue")

# Find 5% quantile using qnorm
q5 <- qnorm(0.05)

# Add lines using the segments() function
segments(x0 = q5, y0 = -1, y1 = 0.05, lty = "dashed", lwd = 2, col ="red")
segments(x0 = -4, y0 = 0.05, x1 = q5, lty = "dashed", lwd = 2, col ="red")

# Add tick marks in the plot
axis(1, at = q5, label = round(q5,2))
axis(2, at = 0.05, label = 0.05, las = 1)

# Start by creating a sequence
x <- seq(-3, 3, 0.1)

# Normal density
normal <- dnorm(x)

# Student-t with 2 df
st2 <- dt(x, df = 2)

# Student-t with 3 df
st3 <- dt(x, df = 3)

# Student-t with 10 df
st10 <- dt(x, df = 10)

plot(x, normal, type = "l", main = "Comparing distributions", col = 1, xlab = "x", ylab = "f(x)")
lines(x, st2, col = 2)
lines(x, st3, col = 3)
lines(x, st10, col = 4)

legend("topright", legend = c("Normal", "T - 2 df", "T - 3 df", "T - 10 df"), col = c(1:4), fill = c(1:4))

# One random number from a Standard Normal
rnorm(1)

# Ten random numbers from a Normal(10, 0.5)
rnorm(10, mean = 10, sd = 0.5)

# One random number from a Student-t with 5 degrees of freedom
rt(1, df = 5)

set.seed(442)
rnorm(5)

set.seed(442)
rnorm(5)

# Normal distribution 
rnd_normal <- rnorm(1000)

# Student-t
rnd_t <- rt(1000, df = 3)

# Plotting
plot(rnd_t, col = "red", pch = 16, main = "Random points from a Normal and Student-t")
points(rnd_normal, col = "blue",pch = 16)

# Adding a legend
legend("bottomright", legend = c("Student-t", "Normal"), pch = 16, col = c("red", "blue"))

sample(1:100, 1)
sample(1:100, 3)
sample(1:100, 5)

# Getting the grades
grades <- sample(1:100, 60)

# Creating a histogram
hist(grades, col = "lightgray")

# We can use a condition in the square brackets to subset the vector:
length(grades[grades >= 70])

# Sample size: 100
norm1 <- rnorm(100)

# Sample size: 1000
norm2 <- rnorm(1000)

# Sample size: 100000
norm3 <- rnorm(100000)

# Create a sequence
x <- seq(-3,3,0.1)

# Plot the histograms and overlay a normal distribution
hist(norm1, freq = FALSE, breaks = 20, main = "Sample size 100", col = "lightgrey", ylim = c(0, 0.5))
lines(x,dnorm(x), lwd = 3, col = "red")
hist(norm2, freq = FALSE, breaks = 20, main = "Sample size 1000", col = "lightgrey", ylim = c(0, 0.5))
lines(x,dnorm(x), lwd = 3, col = "red")
hist(norm3, freq = FALSE, breaks = 20, main = "Sample size 100000", col = "lightgrey", ylim = c(0, 0.5))
lines(x,dnorm(x), lwd = 3, col = "red")

# Initial price
X_0 <- 100

# Volatility
sigma <- 0.01

# Interest rate
r <- 0.05

# Initialize a vector that will contain all realizations
X_1 <- numeric()

# Perform one million realizations
for (i in 1:10^6) {
    X_1[i] <- X_0*exp(r/365)*exp(rnorm(1,0,sigma)-sigma^2/2)
}

# Plot a histogram
hist(X_1, col = "lightgray", main = "Distribution of expected prices",
    xlab = "Price")

quantile(X_1, 0.05)

hist(X_1, col = "lightgray", main = "Distribution of expected prices",
    xlab = "Price")

abline(v = quantile(X_1, 0.05), lwd = 2, col = "red")

# Create a matrix to hold the paths. One column for every simulation and one row for each day, starting from 0
mtx <- matrix(NA, nrow = 101, ncol = 1000)

# Since at time 0 the price is 500, we fill the first row with this value
mtx[1,] <- 500

# Use a loop to draw the path of the price for each simulation

# i represents the simulation, from 1 to 1000
for (i in 1:1000) { 
    
    # j represents the row in our matrix, one for each day
    for (j in 2:101) {
        
        # We proceed to fill the matrix following the random walk
        mtx[j,i] <- mtx[j-1,i] + rnorm(1, mean = 0, sd = 2.5)
    }
}

head(mtx)

matplot(mtx, main = "Random Walk", xlab = "Time", ylab = "Price", type = "l", lty = 1)

# We want to see the last row of mtx
hist(mtx[101,], main = "Distribution of the price after 100 days")

# Average price:
mean(mtx[101,])

load("PRC.RData")

head(PRC)

# Subset the PRC data frame
GE <- subset(PRC, select = "GE")

# Rename the column as price
names(GE) <- "price"

# Plot
plot(GE$price, type = "l", main = "Price of GE")

GE$date <- PRC$date
plot(GE$date, GE$price, type = "l", main = "Price of GE")

# Finding the highest price
max(GE$price)

# We can find the date when this happened in two different but equivalent ways
# 1. Filtering the dates when the stock price was at its maximum
GE$date[GE$price == max(GE$price)] 
# 2. Find the index where the maximum value is reached, and use it as a filter
GE[which.max(GE$price),]

# First we plot the data
plot(GE$date, GE$price, type = "l", main = "Price of GE")

# We add a vertical line using abline and the function ymd to turn a number into a Date
abline(v = ymd(20010907), lwd = 2, col = "blue")

# First we create the limits
x_left = ymd(20080101)
x_right = ymd(20120101)

# Use xlim to incorporate them
plot(GE$date, GE$price, type = "l", main = "Price of GE during the crisis",
     xlab = "Time", ylab = "Price", xlim = c(x_left, x_right))

# Create the plot again
plot(GE$date, GE$price, type = "l", main = "Price of GE during the crisis",
     xlab = "Time", ylab = "Price", xlim = c(x_left, x_right), xaxt = "na")

# Use axis.Date to edit 
axis.Date(1, at=seq(x_left, x_right, by="6 mon"), format="%m/%Y")

# Loading the data
load("Y.RData")

# Adding the returns to our GE data frame
GE$returns <- Y$GE

# Plot the returns
plot(GE$date, GE$returns, type = "l", main = "Returns of GE")

x_left = ymd(20170101)
x_right = ymd(20191231)

plot(GE$date, GE$returns, type = "l", main = "Returns of GE between 2017 and 2019", 
         xlab = "Time", ylab = "Returns", xlim = c(x_left, x_right), xaxt = "na")

axis.Date(1, at=seq(x_left, x_right, by="1 mon"), format="%m/%Y")

# Get the mean and sd of returns
ge_mean <- mean(GE$returns)
ge_sd <- sd(GE$returns)
paste0("Mean: ", round(ge_mean,3))
paste0("Standard Deviation: ", round(ge_sd,3))

# Create the histogram
hist(GE$returns, freq = FALSE, main = "Returns of GE", col = "lightgrey", breaks = 50)

# Add the normal distribution
x <- seq(-3,3,0.001)
lines(x, dnorm(x, mean = ge_mean, sd = ge_sd), lwd = 3, col = "red")

jarque.bera.test(GE$returns)

# qqPlot of the normal distribution
qqPlot(GE$returns, distribution = "norm", envelope = FALSE)

# 2 degrees of freedom
qqPlot(GE$returns, distribution = "t", df = 2, envelope = FALSE,
      main = "2 Degrees of Freedom")

# 3 degrees of freedom
qqPlot(GE$returns, distribution = "t", df = 3, envelope = FALSE,
      main = "3 Degrees of Freedom")

# 4 degrees of freedom
qqPlot(GE$returns, distribution = "t", df = 4, envelope = FALSE,
      main = "4 Degrees of Freedom")

# Autocorrelation of returns
acf(GE$returns, main = "Autocorrelation of returns")

# Autocorrelation of returns squared
acf(GE$returns^2, main = "Autocorrelation of returns squared")

# Autocorrelation of returns squared, 100 lags
acf(GE$returns^2, main = "Autocorrelation of returns squared - 100 lags", lag.max = 100)

# Importing the acf_fm442 function
source("acf_fm442.R")

# Now we can plot again the objects using acf_fm442:
acf_fm442(GE$returns, main = "Autocorrelation of returns")
acf_fm442(GE$returns^2, main = "Autocorrelation of returns squared")
acf_fm442(GE$returns^2, main = "Autocorrelation of returns squared - 100 lags", lag.max = 100)

Box.test(GE$returns, type = "Ljung-Box")
Box.test(GE$returns^2, type = "Ljung-Box")
