library(lubridate)
library(rugarch)

# Load files from the previous seminar
load("VaR.RData")

# Load returns
load("Y.RData")
y <- Y$MSFT
dates <- Y$date

# Parameters
portfolio_value = 1000
p = 0.05

# Steps inside the function:
# Take as argument
# GARCH spec, here the default
# Probability, here 0.05
# Portfolio value, here 1000
# Estimation window, here 1000
spec <- ugarchspec()
p <- 0.05
portfolio_value <- 1000
WE <- 1000

# Determine number of observations
n <- length(y)

# Initialize empty VaR vector
    
VaR <- rep(NA, n)
    
# Do a loop for the forecast
for (i in 1:(n-WE)){
        
    # Subset the dataset to the estimation window
    window <- y[i:(i+WE-1)]
        
    # Fit the GARCH
    res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
        
    # Save coefficients
    omega <- coef(res)['omega']
    alpha <- coef(res)['alpha1']
    beta <- coef(res)['beta1']
        
    # Estimate sigma2 using the last observation of window
    sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
        
    # Allocate the VaR forecast in the vector
    VaR[i+WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
    }

# Function that creates a GARCH forecast

DoGARCH <- function(y, spec, probability = 0.05, portfolio_value = 1, WE = 1000){
    # GARCH function that takes as argument:
    # y: A vector of returns, ordered by date
    # spec: The ugarchspec object with the GARCH specification
    # probability: The probability to be used for VaR - Default 5%
    # portfolio_value: The portfolio value - Default 1
    # WE: Estimation window for the forecast - Default 1000 days
    
    # To calculate elapsed time, first get the current time
    old <- Sys.time()
    
    # Print message
    cat("Doing GARCH VaR forecast", "\n",
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
        
        # Subset the dataset to the estimation window
        window <- y[i:(i+WE-1)]
        
        # Fit the GARCH
        res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
        
        # Save coefficients
        omega <- coef(res)['omega']
        alpha <- coef(res)['alpha1']
        beta <- coef(res)['beta1']
        
        # Estimate sigma2 using the last observation of window
        sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
        
        # Allocate the VaR forecast in the vector
        VaR[i+WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
    }
    
    # Get the new time and print the elapsed time
    time <- difftime(Sys.time(), old, units = "secs")
    cat("\n", "Elapsed time:", round(time,4), "seconds")
    
    # Return the VaR vector
    return(VaR)
}

# Create specification
spec <- ugarchspec(
  variance.model = list(garchOrder= c(1,1)),
  mean.model= list(armaOrder = c(0,0), include.mean=FALSE)
)

# GARCH VaR for 300 days
GARCH300 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 300)

# Saving the output
save(GARCH300, file = "GARCH300.RData")

# GARCH VaR for 2000 days
GARCH2000 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 2000)

# Saving the output
save(GARCH2000, file = "GARCH2000.RData")

# If we have it already saved, we can load it by:
load("GARCH300.RData")
load("GARCH2000.RData")

# Bind into a matrix
GARCH_VaR <- cbind(GARCH300, GARCH2000)

# Plot and modify axis to include dates
matplot(dates, GARCH_VaR, type = "l", lty = 1, col = 1:2, xaxt = "n", main = "GARCH VaR", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = c("WE: 300", "WE: 2000"), lty = 1, col = 1:2)

# Creating a matrix for all VaR forecasts
VaR <- cbind(VaR, GARCH300, GARCH2000)

# Means for each forecast
round(colMeans(VaR, na.rm = TRUE),3)

# Standard deviations - We
round(apply(VaR, 2, sd, na.rm = TRUE))

# Plot all
matplot(dates, VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(VaR), lty = 1, col = 1:6)

# Find maximum estimation window
windows <- colSums(is.na(VaR))


# Restrict to largest estimation window
VaR[1:max(windows),] <- NA

# Plot all
matplot(dates, VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(VaR), lty = 1, col = 1:6)

# Backtesting and Violation Ratios

# Let's transform VaR to a data.frame
VaR <- as.data.frame(VaR)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- VaR
Violations[] <- NA

dim(Violations)

# Logicals in R
a <- 10000 < 5
b <- 10000 > 5

a
b

sum(a,b)

# Populating the Violations matrix

# For every model (columns in VaR)
for(i in 1:dim(VaR)[2]){
    
    # Fill the column in Violations with TRUE/FALSE
    # TRUE if the realized return is lower than VaR
    # FALSE otherwise
    Violations[,i] <- y*portfolio_value < -VaR[,i]
}

# Find where violations happened
dates[which(Violations$EWMA_VaR)]

# Get a random day where EWMA VaR is violated using sample()
random_day <- sample(dates[which(Violations$HS2000)],1)

# Find the index in dates using which()
day_index <- which(dates == random_day)

# See that row in Violations
paste0("Violation for HS2000 on ",random_day)
Violations[day_index,]

# Subsetting with logical vectors
logi <- c(TRUE, FALSE, TRUE, TRUE)
vec <- c(1,2,3,4)

# If we subset by "logi", we will only keeps positions where TRUE
vec[logi]

# Plotting the violations
plot(dates, VaR$EWMA_VaR, type = "l", main = "EWMA VaR with violations")

# Add points where the violations happened
points(dates[Violations$EWMA_VaR], VaR$EWMA_VaR[Violations$EWMA_VaR], pch = 16, col = "red")

# Check dates where all models have a violation
w <- apply(Violations, 1, all)

# Days where all models have a violation
sum(w, na.rm = TRUE)

# Plotting the returns and adding the days where all models had a violation
plot(dates, y, main = "Microsoft returns", type = "l", lwd = 2, las = 1,
    xlab = "Date", ylab = "Returns")
points(dates[w], y[w], pch = 16, col = "red")

# Counting Violations by model
colSums(Violations, na.rm = TRUE)

# Creating a Violation Ratio object

# Remove the rows with NA
Violations <- Violations[!is.na(Violations[,1]),]

# Get the column sums
V <- colSums(Violations)

# Calculate expected violations
EV <- dim(Violations)[1]*p

# Violation Ratios
VR <- V/EV

# Call object, rounding to 3 decimals
round(VR,3)

# We can write a function that uses our rule of thumb to assess the model
model_assessment <- function(VR) {
    if (VR > 0.8 & VR < 1.2) {
        paste0(names(VR), "Model is good")
    } else if ((VR > 0.5 & VR <= 0.8) | (VR > 1.2 & VR <= 1.5)) {
        paste0(names(VR), "Model is acceptable")
    } else if ((VR > 0.3 & VR <= 0.5) | (VR > 1.5 & VR <= 2)) {
        paste0(names(VR), "Model is bad")
    } else {
        paste0(names(VR), "Model is useless")
    }
}

# We can use sapply(), the vector version of apply()
sapply(VR, model_assessment)

# Best performing - VR closest to 1
sort(round(abs(VR-1),3))

# Multivariate EWMA and HS VaR

# Determine a vector of portfolio weights
w <- c(0.5, 0.2, 0.3)

# EWMA VaR
multi_y <- Y[,c("MSFT", "JPM", "INTC")]
multi_y <- as.matrix(multi_y)
n <- dim(multi_y)[1]
K <- dim(multi_y)[2]

# Number of variables
nvar <- K+K*(K-1)/2
EWMA <- matrix(NA, nrow = n, ncol = nvar)
lambda <- 0.94
S <-- cov(multi_y)
# Fill initial row, order:
# 1st col: MSFT Variance, 2nd: JPM Variance, 3rd: INTC Variance
# 4th: MSFT-JPM, 5th: MSFT-INTC, 6th: JPM-INTC

EWMA[1,] <- c(S[1,1], S[2,2], S[3,3], S[1,2], S[1,3], S[2,3])
colnames(EWMA) <- c("MSFT", "JPM", "INTC", "MSFT-JPM", "MSFT-INTC", "JPM-INTC")
head(EWMA)

# Populating EWMA matrix and creating the portfolio variance

portfolio_var <- rep(NA,n)

for (i in 2:n) {
    S <- lambda * S + (1-lambda) * multi_y[i-1,] %*% t(multi_y[i-1,])
    EWMA[i,] <- c(S[1,1], S[2,2], S[3,3], S[1,2], S[1,3], S[2,3])
    portfolio_var[i] <- t(w) %*% S %*% w
}


# Implement the VaR forecast
# Plot estimation for conditional volatility
EWMA_cond_volatility <- sqrt(portfolio_var)
EWMA_VaR <- -qnorm(p) * EWMA_cond_volatility * portfolio_value

# Burn the first 1000 observations
EWMA_VaR[1:1000] <- NA

# Multivariate HS

# Initialize the vector
HS_VaR <- rep(NA, length = n)

# Do the HS
window <- 1000
for (i in 1:(n-window)) {
    # Multiply the matrix y (dimension T x 3) by vector w (dimension 3x1)
    # Output is portfolio returns (dimension Tx1)
    yp <- multi_y[i:(i+window),] %*% w
    head(yp)

    # Sort the vector
    ys <- sort(yp)

    # Position of the 5% quantile
    quant <- ceiling(p*length(ys))
    
    # Get VaR
    HS_VaR[i+window] <- -ys[quant]*portfolio_value
}

multi_VaR <- cbind(EWMA_VaR, HS_VaR)

matplot(dates, multi_VaR, type = "l", lty = 1, col = 1:2, xaxt = "n", main = "VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(multi_VaR), lty = 1, col = 1:2)

# Mean and SD
round(colMeans(multi_VaR, na.rm = TRUE))
round(apply(multi_VaR, 2, sd, na.rm = TRUE))

# Violation Ratios

# Transform VaR to data frame
multi_VaR <- as.data.frame(multi_VaR)

multi_Violations <- multi_VaR

multi_Violations[] <- NA

for(i in 1:dim(multi_VaR)[2]){
    returns <- multi_y %*% w
    multi_Violations[,i] <- returns*portfolio_value < -multi_VaR[,i]
}

multi_Violations <- multi_Violations[!is.na(multi_Violations[,1]),]

multi_V <- colSums(multi_Violations)

multi_EV <- dim(multi_Violations)[1]*p

multi_VR <- multi_V/multi_EV

round(multi_VR,3)

model_assessment(multi_VR[1])
model_assessment(multi_VR[2])

# Stress Testing

# Subset for crisis peridos
crisis <- year(dates) >= 2008 & year(dates) < 2013
y_crisis <- y[crisis]
VaR_crisis <- VaR[crisis,]

Violations_crisis <- VaR_crisis
Violations_crisis[] <- NA

for(i in 1:dim(VaR_crisis)[2]){
    Violations_crisis[,i] <- y_crisis*portfolio_value < -VaR_crisis[,i]
}


# Remove the rows with NA
Violations_crisis <- Violations_crisis[!is.na(Violations_crisis[,1]),]

# Get the column sums
V_crisis <- colSums(Violations_crisis)

# Calculate expected violations
EV_crisis <- dim(Violations_crisis)[1]*p

# Violation Ratios
VR_crisis <- V_crisis/EV_crisis

# Call object, rounding to 3 decimals
round(VR_crisis,3)

sapply(VR_crisis, model_assessment)

# Best performing - VR closest to 1
sort(round(abs(VR_crisis-1),3))
