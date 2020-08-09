# Implementing HS to estimate VaR and ES

# Load the data
load("Y.RData")

# Extract the returns for GE
y <- Y$GE

# View them
head(y)

# Plot the returns
plot(y, type = "l", main = "Returns for GE")

# Specify the probability p
p <- 0.05

# Assume we have a portfolio value of 1000 USD
portfolio <- 1000

# Sort the values in y using sort()
ys <- sort(y)

# Plot them
plot(ys, type = "l", main = "Sorted returns for GE", las = 1)

# Number of observations
n <- length(y)

# Get the 5% quantile by multiplying the length times p
n * p

# Round up
quant <- ceiling(n*p)
quant

# Find the 378th element in ys
ys[quant]

# Visually finding the 5% quantile
plot(ys, type = "l", main = "Sorted returns for GE", las = 1)

# Adding the segments
segments(x0 = quant, y0 = -0.5, y1 = ys[quant], lty = "dashed", lwd = 2, col ="red")
segments(x0 = -1000, y0 = ys[quant], x1 = quant, lty = "dashed", lwd = 2, col ="red")

# Adding ticks
axis(1, at = quant, label = quant)
axis(2, at = ys[quant], label = round(ys[quant],3), las = 1)

# Use it to calculate VaR, scaling for the portfolio value
VaR <- -ys[quant] * portfolio
VaR

# For ES, we get the mean of observations up to the 5th quantile
ES <- -mean(ys[1:quant]) * portfolio
ES

# Report our findings:
cat("We have", length(y), "observations on GE", "\n",
 p , "VaR is", VaR, "and ES is", ES, "on portfolio worth $", portfolio, "\n")

# Doing HS with the quantile() function
quant2 <- quantile(y,0.05)
quant2

# VaR
VaR <- -quant2*portfolio

cat("5% VaR is:", VaR)

# HS for GE and C

# Get the returns from GE and C and transform into a matrix
y <- as.matrix(Y[,c("GE", "C")])

# Create a vector of portfolio weights, here we will split the stocks halfway
w <- c(0.5, 0.5)

# Multiply the matrix y (dimension Tx2) by the vector w (dimension 2x1)
# Output yp is portfolio returns, dimension (Tx1)
yp <- y %*% w
head(yp)

# Plot the portfolio returns
plot(Y$date, yp, main = "Portfolio returns for GE and C", type = "l", las = 1, lwd = 2, col = "red")

# Get VaR and ES

# Sort the vector
ys <- sort(yp)

# Get the length
n <- length(ys)

# Get the position of 5% quantile
quant <- ceiling(n*p)

# VaR
VaR <- -ys[quant]*portfolio

# ES 
ES <- -mean(ys[1:quant]) * portfolio

cat("We have", length(ys), "observations on the portfolio", "\n",
 p , "VaR is", VaR, "and ES is", ES, "on portfolio worth $", portfolio, "\n")

# Historical simulation with fixed estimation window
y <- Y$GE
window <- 1000
p <- 0.05
n <- length(y)
portfolio_value <- 1000

# Initialize and empty vector to hold VaR forecasts
VaR <- vector(length = (n - window))

# Loop to calculate one VaR per day
for (i in 1:(n-window)) {
    ys <- sort(y[i:(i+window)])
    quant <- ceiling(p*length(ys))
    VaR[i] <- -ys[quant]*portfolio_value
}

# Plot it
plot(VaR, type = "l", main = "VaR HS with Estimation Window 1000",
    col = "red", las = 1, ylab = "USD")

# We should add the dates in the x-axis
# Notice we have to exclude the first 1000 days
dates_var <- Y$date[-1:-1000]

# Plot again
plot(dates_var, VaR, type = "l", main = "VaR HS with Estimation Window 1000",
    col = "red", las = 1, ylab = "USD", xlab = "Date")

windows <- c(100, 500, 1000, 5000)
which(windows == 100)

# Using four different estimation window sizes
windows <- c(100, 500, 1000, 5000)

# Create an empty data frame to fill with the forecasts
HS <- data.frame(HS100 = numeric(),
                HS500 = numeric(),
                HS1000 = numeric(),
                HS5000 = numeric())

# Do a loop for every element of windows
for (window in windows) {
    
    # Perform a daily HS VaR
    for (i in 1:(n-window)) {
        
        # Sort the returns for the respective estimation window
        ys <- sort(y[i:(i+window)])
        
        # Get the quantile
        quant <- ceiling(p*length(ys))
        
        # Allocate the result to the corresponding column of HS
        # Use which() to find the index of the window and allocate to that column
        column <- which(windows == window)
        HS[i+window, column] <- -ys[quant]*portfolio_value
    }
}


# Plotting the forecasts
plot(HS$HS100, main = "HS with different estimation windows", ylab = "VaR in USD",
    xlab = "Date", type = "l", col = "red", lwd = 2)
lines(HS$HS500, col = "blue", lwd = 2)
lines(HS$HS1000, col = "green", lwd = 2)
lines(HS$HS5000, col = "black", lwd = 2)

legend("topright", legend = names(HS), lty = 1, col = c("red", "blue", "green", "black"))

# Subset HS to keep only the observations that fit the most restrictive estimation window
HS <- HS[(max(windows)+1):n,]
# Extract the dates to use
dates <- Y$date[(max(windows)+1):n]

# Plotting them all together
plot(dates, HS$HS100, main = "HS with different estimation windows", ylab = "VaR in USD",
    xlab = "Date", type = "l", col = "red", lwd = 2)
lines(dates,HS$HS500, col = "blue", lwd = 2)
lines(dates,HS$HS1000, col = "green", lwd = 2)
lines(dates,HS$HS5000, col = "black", lwd = 2)

legend("bottomright", legend = names(HS), lty = 1, col = c("red", "blue", "green", "black"))

# Means
colMeans(HS)

# Standard deviations
sd(HS$HS100)
sd(HS$HS500)
sd(HS$HS1000)
sd(HS$HS5000)

# Creating a HS VaR forecast function

DoHS <- function(y, probability = 0.05, portfolio_value = 1, WE = 1000){
    # HS function that takes as argument:
    # y: A vector of returns, ordered by date
    # probability: The probability to be used for VaR - Default 5%
    # portfolio_value: The portfolio value - Default 1
    # WE: Estimation window for the forecast - Default 1000 days
    
    # To calculate elapsed time, first get the current time
    old <- Sys.time()
    
    # Print message
    cat("Doing Historical simulation VaR forecast", "\n",
       "Estimation window:", WE, "\n",
       "Number of observations:", length(y), "\n",
       "VaR probability:", probability, "\n",
       "Portfolio value:", portfolio_value)
    
    # Number of observations
    n <- length(y)
    
    # Initialize empty VaR vector
    VaR <- rep(NA, n)
    
    # Do a loop for the forecast
    for (i in 1:(n-WE)){
        
        # Sort the returns for the estimation window
        ys <- sort(y[i:(i+WE-1)])
        
        # Get the quantile position, using ceiling to round up
        quant <- ceiling(probability * length(ys))
        
        # Allocate the VaR forecast in the vector
        VaR[i+WE] <- -ys[quant]*portfolio_value
    }
    
    # Get the new time and print the elapsed time
    time <- Sys.time() - old
    cat("\n", "Elapsed time:", round(time,4), "seconds")
    
    # Return the VaR vector
    return(VaR)
}

# Loading the returns for Microsoft
y <- Y$MSFT
dates <- Y$date

# Plot the returns
plot(dates, y, main = "Microsoft returns", type = "l", lwd = 2, las = 1,
    xlab = "Date", ylab = "Returns")

# Historical Simulation with an estimation window of 300 days
HS300 <- DoHS(y, probability = p, portfolio_value = portfolio_value, WE = 300)

# Historical Simulation with an estimation window of 1000 days
HS1000 <- DoHS(y, probability = p, portfolio_value = portfolio_value, WE = 1000)

# Historical Simulation with an estimation window of 2000 days
HS2000 <- DoHS(y, probability = p, portfolio_value = portfolio_value, WE = 2000)

# Bind HS into a matrix
HS_VaR <- cbind(HS300, HS1000, HS2000)

# Plot and modify axis to include dates
matplot(dates, HS_VaR, type = "l", lty = 1, col = 1:3, xaxt = "n", main = "HS VaR", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("bottomleft", legend = colnames(HS_VaR), lty = 1, col = 1:3)

# Implementing EWMA VaR

# Specifying the parameters for VaR
p <- 0.05
portfolio_value <- 1000

# Specifying the parameters for EWMA
lambda <- 0.94
n <- length(y)
BurnTime <- 30

# Vector to store estimated conditional volatility
EWMA_Variance <- rep(NA, length = n)

# First value is the sample variance
EWMA_Variance[1] <- var(y)

# See the vector
head(EWMA_Variance)

# Run the EWMA model using a for loop
for (i in 2:n) {
    EWMA_Variance[i] <- lambda * EWMA_Variance[i-1] + (1-lambda) * y[i-1]^2
}

# Replacing the data in the estimation window to NA
EWMA_Variance[1:BurnTime] <- NA

# Plot estimation for conditional volatility
EWMA_cond_volatility <- sqrt(EWMA_Variance)
plot(dates, EWMA_cond_volatility, type = "l", main = "EWMA Conditional Volatility",
    las = 1, col = "red", xlab = "Date", ylab = "EWMA Cond. Volatility")

# Implementing the VaR forecast
EWMA_VaR <- -qnorm(p) * EWMA_cond_volatility * portfolio_value

# Plotting it
plot(dates, EWMA_VaR, type = "l", main = "EWMA VaR",
    las = 1, col = "red", xlab = "Date", ylab = "USD")

# Storing the data
VaR <- cbind(HS_VaR, EWMA_VaR)
VaR <- as.data.frame(VaR)
save(VaR, file = "VaR.RData")
