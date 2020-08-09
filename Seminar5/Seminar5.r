library(rmgarch)
library(microbenchmark)
library(lubridate)

# Load the data
load("Y.RData")

# Extract the returns for JPM and C
y <- Y[c("JPM", "C")]

# Check the class of y
class(y)

# Transform into matrix
y  <- as.matrix(y)

# Check class again
class(y)

# Plotting the returns in a 2x1 grid
par(mfrow = c(2,1))
plot(y[,1], type = "l", main = "Returns for JPM", col = 1)
plot(y[,2], type = "l", main = "Returns for C", col = 2)
# Reset the grid
par(mfrow = c(1,1))

# Determine number of entries
n <- dim(y)[1]

# Initializing the EWMA matrix
EWMA <- matrix(NA, nrow = n, ncol = 3)

# Determine lambda
lambda <- 0.94

# Get the sample covariance
S <- cov(y)
S

# Getting the unique values in three ways:

# 1. Creating a vector with the distinct elements
c(S[1,1], S[1,2], S[2,2])

# 2. Vectorizing the matrix and getting distinct elements
c(S)[c(1,2,4)]

# 3. Using the fact that S is symmetric and using upper.tri()/lower.tri()
S[!upper.tri(S)]

# Fill the initial row of EWMA with the sample covariances
EWMA[1,] <- S[!upper.tri(S)]

# View the matrix
head(EWMA)

# Manually computing EWMA elements for t = 2

# Apply the formula for EWMA
S_2 <- lambda * S + (1-lambda) * y[1,] %*% t(y[1,])
# Get the variances and covariances
S_2[!upper.tri(S_2)]

# Populating the EWMA matrix

# Create a loop for rows 2 to n
for (i in 2:n) {
    # Update S with the new weighted moving average
    S <- lambda * S + (1-lambda) * y[i-1,] %*% t(y[i-1,])
    
    # Fill the following EWMA row with the covariances
    EWMA[i,] <- S[!upper.tri(S)]
}

head(EWMA)

# Matrix operations - The order is important

# This is a 2x1 vector
y[1,]

# This is a scalar
t(y[1,]) %*% y[1,]

# This is a matrix
y[1,] %*% t(y[1,])

# Creating a wrong EWMA

# Initialize the matrix the same way
wrong_EWMA <- matrix(NA, nrow = n, ncol = 3)
S <- cov(y)
wrong_EWMA[1,] <- S[!upper.tri(S)]

# Do the loop but interchange the transpose
for (i in 2:n) {
    # Update S with the new weighted moving average
    S <- lambda * S + (1-lambda) * t(y[i-1,]) %*% y[i-1,]
    
    # Fill the following EWMA row with the covariances
    wrong_EWMA[i,] <- S[!upper.tri(S)]
}

# Plotting estimated variances and covariances
matplot(EWMA, type = "l", main = "EWMA", lty = 1)
legend("topleft", legend = c("JPM", "Covar", "C"), col = 1:3, lty = 1)

# Correlation coefficient
EWMArho <- EWMA[,2]/sqrt(EWMA[,1]*EWMA[,3])

# Plot
plot(EWMArho, type = "l", main = "Correlation coefficient of JPM and C", col = "red")

# Plots for conditional volatility
plot(Y$date, sqrt(EWMA[,1]), type = "l", main = "Conditional volatility JPM")
plot(Y$date, sqrt(EWMA[,3]), type = "l", main = "Conditional volatility C")

# Correlation Coefficient plot
plot(Y$date, EWMArho, type = "l", main = "Correlation coefficient of JPM and C", col = "red")

# DCC model

# Specify the default univariat GARCH model with no mean
xspec <- ugarchspec(
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Replicate it into a multispec() element
uspec <- multispec(replicate(2, xspec))

# Define the specification for the DCC model
spec <- dccspec(
    # GARCH specification
    uspec = uspec,
    
    # DCC specification
    dccOrder = c(1,1),
    
    # Distribution, here multivariate normal
    distribution = "mvnorm"
)

# Fit the specification to the data
res <- dccfit(spec, data = y)

# Call the variable
res

coef(res)

# Explore DCC object
names(res@mfit)

# Extracting the H matrix
H <- res@mfit$H

# Dimensions of the H matrix
dim(H)

# First period's covariances
H[,,1]

# First four covariance matrix estimations
print(H[,,1:4])

# Computing the conditional correlations

# Initializing the vector
rhoDCC <- vector(length = n)

# Populate with the correlations
rhoDCC <- H[1,2,] / sqrt(H[1,1,]*H[2,2,])

# Benchmarking

# Use microbenchmark() on the two expressions we are comparing
benchmark <- microbenchmark( 
                            
    for (i in 1:n) {
        rhoDCC[i] <- H[1,2,i] / sqrt(H[1,1,i] * H[2,2,i])
    }
    
    ,
    
    H[1,2,] / sqrt(H[1,1,] * H[2,2,])    
)

# Use aggregate() to find the mean of time by expression
aggregate(benchmark$time, by = list(benchmark$expr), FUN = mean)

# DCC apARCH model

# Univariate specification
xspec <- ugarchspec(variance.model = list(model = "apARCH"),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
                    )

# Duplicate the specification using multispec
uspec <- multispec(replicate(2, xspec))

# Create the DCC specification
spec <- dccspec(uspec = uspec,
               dccOrder = c(1,1),
               distribution = "mvnorm")

# Fit it to the data
res_aparch <- dccfit(spec, data = y)

# Call the object
res_aparch

# DCC tapARCH model

# Univariate specification
xspec <- ugarchspec(variance.model = list(model = "apARCH"),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
                    )

# Duplicate the specification using multispec
uspec <- multispec(replicate(2, xspec))

# Create the DCC specification, replace the multivariate normal by a multivariate T
spec <- dccspec(uspec = uspec,
               dccOrder = c(1,1),
               distribution = "mvt")

# Fit it to the data
res_taparch <- dccfit(spec, data = y)

# Call the object
res_taparch

# Creating the conditional correlations vector from the tapARCH model

# Extracting covariances
H <- res_taparch@mfit$H

# Initializing vector
rhoDCCRich <- vector(length = n)

# Fill the covariance matri

rhoDCCRich <- H[1,2,] / sqrt(H[1,1,] * H[2,2,])

# Importing the LR_test function
source("LR_test.R")
LR_test

# Applying the LR test to our DCC models
LR_test(res, res_taparch, model = "DCC")

# DCC model for all assets

# Extracting all the columns from Y except the date
y_all <- subset(Y, select = -c(date))

# See y
head(y_all)

# Transform to matrix
y_all <- as.matrix(y_all)

# Check dimensions
dim(y_all)

# Build DCC model

# Univariate spec
xspec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE))

# Replicate for each asset
uspec <- multispec(replicate(dim(y_all)[2], xspec))

# Build DCC spec
spec <- dccspec(uspec = uspec,
                dccOrder = c(1,1),
                distribution = "mvnorm")

# Fit the model
dcc_all <- dccfit(spec, data = y_all)

# Call the object
dcc_all

# Extracting H matrix
H <- dcc_all@mfit$H

# Check the dimensions
dim(H)

# See the first one - 6x6 matrix
H[,,1]

# Check which asset is in which column
colnames(y_all)

# Conditional correlation between two stocks
r_msft_jpm <- H[1,4,]/sqrt(H[1,1,]*H[4,4,])

# Writing a function that prevents us from keeping track of numbers

cond_corr <- function(stock1, stock2) {
    
    # Finds the index of each ticker in colnames
    index1 <- which(colnames(y_all) == stock1)
    index2 <- which(colnames(y_all) == stock2)
    
    # Return the vector operation
    return(H[index1, index2, ]/sqrt(H[index1, index1,]*H[index2, index2,]))
}

# Call it on MSFT and JPM
r_msft_jpm_2 <- cond_corr("MSFT", "JPM")

# Compare the two:
head(r_msft_jpm)
head(r_msft_jpm_2)

# JPM and C
r_jpm_c <- cond_corr("JPM", "C")

# Plot it
plot(Y$date, r_jpm_c, main = "Conditional correlation for JPM and C",
     type = "l", col = "red", las = 1)

# Horizontal line at mean
plot(Y$date, r_jpm_c, main = "Conditional correlation for JPM and C",
     type = "l", col = "red", las = 1)
abline(h = mean(r_jpm_c), lwd = 2)

# Comparing DCC and EWMA visually
matplot(cbind(EWMArho, rhoDCC, rhoDCCRich), type = "l", lty = 1,
       main = "Conditional Correlations for EWMA, DCC, DCC and tapARCH")
legend("bottomright", legend = c("EWMA", "DCC", "DCC tapARCH"), lty = 1, col = 1:3)

# Correlation
cor(cbind(EWMArho, rhoDCC, rhoDCCRich))

# Relationship between EWMArho and rhoDCC
plot(rhoDCC, EWMArho, main = "Relationship between EWMArho and rhoDCC")

# Mean of each
colMeans(cbind(EWMArho, rhoDCC, rhoDCCRich))

# Standard deviation of each
sd(EWMArho)
sd(rhoDCC)
sd(rhoDCCRich)

# EWMA and standard DCC
matplot(cbind(EWMArho, rhoDCC), type = "l", lty = 1,
       main = "Conditional Correlations for EWMA and DCC")
legend("bottomright", legend = c("EWMA", "DCC"), lty = 1, col = 1:2)
