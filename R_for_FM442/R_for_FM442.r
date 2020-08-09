for (i in 1:5) {
    print(i)
}

1:5

seq(1,5,1)

for (i in seq(1,5,1)) {
    print(i)
}

vec <- rep(NA, 500)

for (i in seq(1, 500, by = 2)) {    # Loop over i = 1, 3, 5, ...
    vec[i] <- 1                     # Replace the i-th element in vec for 1
}

fibonacci <- rep(NA, 50)                  # Create the empty vector
fibonacci[1:2] <- 1                        # The first two values are 1
for (i in 3:50){                     # Loop over the indexes 3 to 100
    fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]  # Replace the i-th term by the sum of the two previous terms
}

fibonacci

fibonacci <- function(N) {
    fibo <- rep(NA, N)
    fibo[1:2] <- 1
    for (i in 3:N) {
        fibo[i] <- fibo[i-1] + fibo[i-2]
    }
    paste0("The ", N, "-th Fibonacci number is: ", fibo[N])
}

fibonacci(10)
fibonacci(50)

model <- c("GARCH", "EWMA", "HS")

violation_ratio <- c(1.06, 0.95, 0.88)

for (i in 1:3){
    cat("The value for the", model[i], "model is:", violation_ratio[i], "\n")
}

model

for (model_type in model){
    cat("My favorite model is", model_type, "\n")
}

estimation_windows = c(300, 500, 1000, 2000)   # Vector of different "windows"
S = 5000
rand <- rnorm(S)                            # Create a vector of random numbers of length 5k
for (window in estimation_windows){            # Loop over estimation windows  
    # Get last window-length elements of rand
    tmp <- rand[(S-window):S]
    cat("Average of last", window, "elements: ", mean(tmp), "\n")
}          

X = 5

if (X > 0) {
    print("Positive")
} else if (X < 0) {
    print("Negative")
} else {
    print("Zero")
}

is_even <- function(x){
    if (x %% 2 == 0) {
        cat(x, "is even", "\n")
    } else {
        cat(x, "is odd", "\n")
    }
}

is_even(4)
is_even(7)

a <- 1

while (a < 6) {
    print(a)
    a = a + 1
}

less_half <- function(x){
    steps <- 0
    while (abs(x) > 0.5) {
        x = x/2
        steps = steps + 1
    }
    cat("Number of steps:", steps, "\n")
}

less_half(10)
less_half(0.2)
less_half(-500)

stocks <- read.csv("stocks.csv")    # Importing data to a dataframe

head(stocks)

?read.csv

class(stocks)

str(stocks)

dim(stocks)
nrow(stocks)
ncol(stocks)
colnames(stocks)

df <- data.frame(col1 = 1:3,
                 col2 = c("A", "B", "C"),
                 col3 = c(TRUE, TRUE, FALSE),
                 col4 = c(1.0, 2.2, 3.3))

str(df)

dim(df)
colnames(df)

df <- data.frame(col1 = 1:3,
                 col2 = c("A", "B", "C"),
                 col3 = c(TRUE, TRUE, FALSE),
                 col4 = c(1.0, 2.2, 3.3),
                stringsAsFactors = FALSE)

str(df)

my_matrix <- matrix(1:10, nrow = 5, ncol = 2, byrow = TRUE)
class(my_matrix)
my_matrix

df <- as.data.frame(my_matrix)
class(df)
df
str(df)

colnames(df) <- c("Odd", "Even")
str(df)

head(stocks)

# Subsetting by row numbers
stocks[2:3,]

# Subsetting by column number
head(stocks[, 4])

# Subsetting by col names
head(stocks[c("MSFT", "GE")])

# Subsetting using $
head(stocks$MSFT)

# Subsetting both rows and columns
stocks[1:2, c("JPM", "C")]

# Subsetting using a logical
head(stocks[stocks$JPM > 0, c("JPM", "date")])

# Subsetting using a vector
v <- c(1, 2, 4, 8, 16)
stocks[v,]

jpm <- stocks[, c("JPM", "date")]
head(jpm)
class(jpm)
dim(jpm)

jpm2 <- stocks[, c("JPM")]
head(jpm2)
class(jpm2)

# We can use an ifelse to assign values conditional on another value
jpm$negative <- ifelse(jpm$JPM < 0, TRUE, FALSE)
head(jpm)

MSFT <- stocks[, c("date", "MSFT")]
INTC <- stocks[, c("date", "INTC")]

head(MSFT)
head(INTC)

new_df <- merge(MSFT, INTC)
head(new_df)

crsp <- read.csv("crsp.csv")
head(crsp)

library(reshape2)
RET <- dcast(crsp, date ~ PERMNO, value.var = "RET")
head(RET)

names(RET) <- c("date", "MSFT", "XOM", "GE", "JPM", "INTC", "C")
head(RET)

save(RET, file = "RET.RData")

write.csv(RET, file = "RET.csv")

library(lubridate)
ymd("20200110")
class(ymd("20200110"))

ymd("2015JAN11")
class(ymd("20200110"))

ymd("04-MAR-5")
class(ymd("04MAR5"))

dmy("1/june/2019")
class(dmy("1/june/2019"))

dmy("28-december-14")
class(dmy("28-december-14"))

ret <- c(0.18, 0.02, -0.29, 0.00, 0.15)
class(ret)

dates <- c("20190121", "20190122", "20190123", "20190124", "20190125")
class(dates)

library(zoo)
ret.ts <- zoo(ret, order.by = ymd(dates))
class(ret.ts)

ret.ts

ret.ts
lag(ret.ts, k = 2)

ret.ts
lag(ret.ts, k = 2, na.pad = TRUE)

ret.ts
lag(ret.ts, k = -2)

ret.ts

diff(ret.ts, lag = 1, na.pad = TRUE)

diff(ret.ts, lag = 1, differences = 2, na.pad = TRUE)

diff(ret.ts, lag = 2, differences = 1, na.pad = TRUE)

# Example: Sum the last two consecutive returns
ret.ts

rollapply(ret.ts, width = 2, FUN = sum, align = "left")

rollapply(ret.ts, width = 2, FUN = sum, align = "right", fill = NA)

ret <- rt(5000, df = 4)    # Random draws from a Student t with 3 dof
dates <- seq(Sys.Date(), by = "-1 day", length.out = 2000) # 2000 days back from today
ret.ts <- zoo(ret, order.by = ymd(dates))   # Transform to zoo

plot(ret.ts, main = "Returns from the past 2000 days",
    xlab = "Date", ylab = "Returns", col = "mediumblue")

sub_ret.ts <- window(ret.ts, start = "2019/1/1", end = "2019/12/31")
plot(sub_ret.ts, main = "Returns from 2019",
    xlab = "Date", ylab = "Returns", col = "mediumblue")

# Data load and cleaning
df <- read.csv("Shiller.csv", header = T, na.strings = NA)
head(df)

df$date <- paste0(df$date, "01") # Add days to dates
# Create time series
ret.ts <- zoo(df$R_r, order.by = ymd(df$date))
PDdiff.ts <- diff(zoo(df$PD_r, order.by = ymd(df$date)), na.pad = T)
head(ret.ts)
head(PDdiff.ts)
# Adjust length
ret.ts <- ret.ts[2:length(ret.ts)]*100
PDdiff.ts <- PDdiff.ts[2:length(PDdiff.ts)]
head(ret.ts)
head(PDdiff.ts)

X <- cbind(rep(1,NROW(PDdiff.ts)), as.vector(PDdiff.ts))   # Add column of 1s
y <- as.vector(ret.ts)
Xt <- t(X) 

betaHat <- solve(Xt %*% X) %*% (Xt %*% y)
retHat <- X %*% betaHat

betaHat

retHat.ts <- zoo(retHat, order.by = index(ret.ts))
resid.ts <- ret.ts - retHat.ts

SSR = sum((ret.ts - retHat.ts)^2)
SST = sum((ret.ts - mean(ret.ts))^2)
R2 = 1 - (SSR/SST)

reg <- lm(ret.ts ~ PDdiff.ts)    # First dependent variable followed by ~ and formula
summary(reg)

# Everything part of the object reg
names(reg)

reg$coefficients

alpha <- reg$coefficients[1]
alpha

# To compare with our OLS by hand
betaHat[1]

beta <- reg$coefficients[2]
beta

# To compare with our OLS by hand
betaHat[2]

std_alpha <- summary(reg)$coefficient[1,2]
std_alpha

std_beta <- summary(reg)$coefficient[2,2]
std_beta

y_fitted <- reg$fitted.values

R2_lm <- summary(reg)$r.squared
R2_lm

# To compare with our OLS by hand
R2

resid_lm <- reg$residuals
head(resid.ts)
head(resid_lm)

# Log-likelihood function for OLS 

LL.OLS <- function(par, depVar, indVar) {
    
    sigma = par[1]
    alpha = par[2]
    beta = par[3]
    betaVec = c(alpha, beta)
    T = nrow(indVar)
    
    out = - (T/2)*log(2*pi*sigma^2) - 1/(2*sigma^2)* (t(depVar - indVar %*% betaVec) %*% (depVar - indVar %*% betaVec))
    
    return(-out) # Return a negative since by default an optimizer minimizes, doesn't maximize
}

# Estimate ML
res <- optim(c(1,0,0), LL.OLS, gr = NULL, y, X, hessian = T)

# Getting standard errors
H <- res$hessian # Hessian
I <- solve(H)    # Invert Hessian
se <- sqrt(diag(I)) # Sqrt of diagonal elements of the inverse

# Getting the parameters
sigmaMLE <- res$par[1]
betaVecMLE <- c(res$par[2], res$par[3])
yHatMLE <- X %*% betaVecMLE
tstat <- betaVecMLE / se[2:3]
print(betaVecMLE)

sp <- read.csv("sp500.csv")
ret  <- diff(log(sp$price))*100
ret     <- ret[2:length(ret)]

# ARCH(1) MLE

LL.ARCH1 <- function(par, x) { 
  
    omega <- par[1] # First element of par
    alpha <- par[2] # Second element of par

    T <- length(x)	# Number of rows 
    loglikelihood <- -(T-1)/2 * log(2*pi)
    
    # Loop (create the sum) and add every instance fo loglikelihood
    for(i in 2:T){
        loglikelihood = loglikelihood - 1/2 * (log(omega + alpha*x[i-1]^2)+x[i]^2/(omega + alpha*x[i-1]^2))
    }

    return(-loglikelihood) # output the log likelihood
}

res <- optim(c(0.1,0.5) , LL.ARCH1, gr = NULL, ret, hessian = T) # Optimization

cat("Omega:", res$par[1], "\n",
   "Alpha:", res$par[2])

# GARCH(1,1) MLE

LL.GARCH1_1 <- function(par, x) { 
  
    omega <- par[1] # First element of par
    alpha <- par[2] # Second element of par
    beta <- par[3]  # Third element of par

    T <- length(x)	# Number of rows 
    loglikelihood <- -(T-1)/2 * log(2*pi)
    sigma_2 <- rep(NA, T)
    sigma_2[1] <- var(x) # Initialize the sigma with unconditional volatility

    # Loop (create the sum) and add every instance to loglikelihood
    for(i in 2:T){
        loglikelihood = loglikelihood - 1/2 * (log(omega + alpha*x[i-1]^2 + beta*sigma_2[i-1])
                                               + x[i]^2/(omega + alpha*x[i-1]^2 + beta*sigma_2[i-1]))
        
        sigma_2[i] <- omega + alpha*x[i-1]^2 + beta*sigma_2[i-1]    # Update sigma
    }

    return(-loglikelihood) # output the log likelihood
}

res <- optim(c(0.1, 0, 0) , LL.GARCH1_1, gr = NULL, ret, hessian = T, 
             method = 'L-BFGS-B', lower = c(0,0,0)) # Optimization

cat("Omega:", res$par[1], "\n",
   "Alpha:", res$par[2], "\n",
   "Beta:", res$par[3])

## Note that the values obtained in GARCH ML will depend on the optimization method used

library(rugarch)
sp <- read.csv("sp500.csv")
ret <- diff(log(sp$price))*100
ret <- ret[2:length(ret)]

spec1 <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

spec1

garch1_1 <- ugarchfit(spec = spec1, data = ret)

class(garch1_1)

names(garch1_1@model)

garch1_1@model$pars

garch1_1@model$modeldesc

names(garch1_1@fit)

garch1_1@fit$matcoef

garch1_1@fit$LLH

plot(garch1_1@fit$sigma, type = "l")

coef(garch1_1)

likelihood(garch1_1)

# Student t

spec2 <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "std"
)

tGARCH <- ugarchfit(spec = spec2, data = ret)

tGARCH@fit$matcoef

# tapARCH model
spec3 <- ugarchspec(
    variance.model = list(model = "apARCH"),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "std"
)

tapARCH <- ugarchfit(spec = spec3, data = ret)

tapARCH@fit$matcoef

library(rmgarch)
load("RET.RData")
y <- RET[c("JPM", "C")]

# Create the univariate specification
uni_spec <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Replicate it into a multispec element
mspec <- multispec(replicate(2, uni_spec))

mspec

spec <- dccspec(
                # Univariate specifications - Needs to be multispec
                uspec = mspec,
                
                # DCC specification. We will assume an ARMA(1,1)-like process
                dccOrder = c(1,1),
                
                # Distribution, here multivariate normal
                distribution = "mvnorm"
)

spec

res <- dccfit(spec, data = y)
res

names(res@model)
names(res@mfit)

# Coefficient matrix
res@mfit$matcoef

# Log likelihood
res@mfit$llh

H <- res@mfit$H
dim(H)

# First period's covariances
H[,,1]

# Initializing the vector
rhoDCC <- vector(length = dim(y)[1])

# Populate with the correlations
rhoDCC <- H[1,2,] / sqrt(H[1,1,]*H[2,2,])

# Initializing the vector
rhoDCC2 <- vector(length = dim(y)[1])

for (i in 1:dim(y)[1]) {
    rhoDCC2[i] <- rcor(res)[1,2,i]
}

plot(rhoDCC2, type = "l")

y_all <- subset(RET, select = -c(date))
y_all <- as.matrix(y_all)

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

load("RET.RData")
jpm <- RET$JPM
head(jpm)

# Plot the returns
plot(jpm, type = "l")

p <- 0.05

jpms <- sort(jpm)
plot(jpms, type = "l")

n <- length(jpms)

n*p

ceiling(n*p)

# VaR(5%)

jpms[ceiling(n*p)]

# Simple returns
p <- 0.05
S <- 1e5
P <- 100
sigma <- 0.01
ret <- rnorm(S, 0, sigma^2)
Psim_simple <- P*(1+ret)

# VaR 5%
Ps_simple <- sort(Psim_simple - P)
Ps_simple[p*S]

# Compound returns
r <- 0.03   # Assuming risk free rate
Psim_comp <- P*exp(r*(1/365))*exp(ret)*exp(-0.5*sigma^2)
# VaR 5%
Ps_comp <- sort(Psim_comp - P)
Ps_comp[p*S]

library(tseries)
sp <- read.csv("sp500.csv")
ret  <- diff(log(sp$price))
jarque.bera.test(ret)

Box.test(ret, type = "Ljung-Box")
Box.test(ret^2, type = "Ljung-Box")

library(rugarch)

# ARCH
spec_arch <- ugarchspec(
    variance.model = list(garchOrder = c(1,0)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

ARCH <- ugarchfit(spec = spec_arch, data = ret, solver = "hybrid")

# GARCH
spec_garch <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

GARCH <- ugarchfit(spec = spec_garch, data = ret, solver = "hybrid")

# Perform the LR test
LR_statistic <- 2*(likelihood(GARCH)-likelihood(ARCH))
p_value  <- 1 - pchisq(LR_statistic, df = 1)

cat(" Likelihood of ARCH: ", round(likelihood(ARCH),2), "\n", 
    "Likelihood of GARCH: ", round(likelihood(GARCH),2), "\n",
   "2 * (Lu - Lr): ", round(LR_statistic,2), "\n",
   "p-value:", p_value)

load("RET.RData")
y <- subset(RET, select = -c(date))
head(y)

print(var(y$MSFT))
print(var(y$XOM))
print(var(y$GE))
print(var(y$JPM))
print(var(y$INTC))
print(var(y$C))

for (stock in names(y)) {
    print(var(y[c(stock)]))
}

apply(y, 2, var)

a <- matrix(rnorm(20), nrow = 5, ncol = 4)
a

# Row sums
apply(a, 1, sum)

# Column sums
apply(a, 2, sum)

# Max of every row
apply(a, 1, max)

vec <- 1:20
apply(vec, 1, sum)

A <- matrix(1:9, nrow = 3, ncol = 3)
B <- matrix(1:4, nrow = 2, ncol =2)
C <- matrix(rep(1, 100), nrow = 10, ncol = 10)

my_list <- list(A,B,C)

# We can extract the 2nd column from every element using the selector "["
lapply(my_list, "[", , 2)

# Or the first row of all elements
lapply(my_list, "[", 1, )

a <- lapply(my_list, "[", 1, )
a[[1]]

vec = 1:10
a <- lapply(vec, sqrt)
a
class(a)

vec = 1:10
b <- sapply(vec, sqrt)
b
class(b)

# Load in this order
library(plyr)
library(dplyr)

# Tidyverse
library(tidyverse)

# First look at the dataset
data <- mpg
head(mpg)

# Select only manufacturer, model and class
data_select <- select(data, manufacturer, model, class)
head(data_select)

# Filter only the cars that are pickups
data_filter <- filter(data, class == "pickup")
head(data_filter)

# Select only the manufacturer model and class, and then filter
# Note that with pipes you don't have to specify "data" again
data_pipes <- data %>% select(manufacturer, model, class) %>% filter(class == "pickup")
head(data_pipes)

# Create a new column that is a concatenation of manufacturer and model
data <- data %>% mutate(car = paste(manufacturer, model, sep = " - "))
head(data)

# Get the mean for the hwy variable
avg_hwy <- data %>% summarize(avg_hwy = mean(hwy))
head(avg_hwy)

# Get the mean of highway mileage for every manufacturer
avg_hwy2 <- data %>% group_by(manufacturer) %>% summarize(avg_hwy = mean(hwy))
head(avg_hwy2)

# For every manufacturer, get the average highway and city mileage
avg_hwy3 <- data %>% group_by(manufacturer) %>% summarize(avg_hwy = mean(hwy),
                                                          avg_cty = mean(cty))
head(avg_hwy3)

# Add the number of observations
avg_hwy3 <- data %>% group_by(manufacturer) %>% summarize(avg_hwy = mean(hwy),
                                                          avg_cty = mean(cty),
                                                          count = n())
head(avg_hwy3)

# Get a list of the unique manufacturers
manufacturer <- unique(data$manufacturer)

# Create a dataframe with manufacturers and their countries:
countries <- data.frame(manufacturer, 
                        country = c("GER","USA","USA","USA","JAP","KOR","USA","UK","USA","USA","JAP","USA","JAP","JAP","GER"),
                       stringsAsFactors = FALSE)

# Use a left join to add this to our data
# R will be smart enough to find the common variable if unspecified
data_all <- left_join(mpg, countries)
head(data_all)

library(ggplot2)
library(tidyverse)

mpg <- mpg
head(mpg)

# Scatter plot
ggplot(data = mpg) +       # Specify the data
    geom_point(mapping = aes(x = displ, y = hwy))     # Add points

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_wrap(~ class, nrow = 3) 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth(data = filter(mpg, class == "suv"), se = F)

height <- data.frame(
    sex = factor(rep(c("F", "M"), each=200)),
    height = round(c(rnorm(200, mean=161.5, sd=6), rnorm(200, mean=175.4, sd=9)))
)

p1 <- ggplot(data=height, aes(x=height)) +
      geom_histogram(stat ="count")
p1

p2 <- ggplot(data=height, aes(x=height)) +
      geom_histogram(stat ="count", color = "black", fill="blue") +
      geom_vline(aes(xintercept = mean(height)), color = "red")
p2

p3 <- ggplot(data=height, aes(x=height)) +
      geom_histogram(aes(y=..density..), color = "black", fill="blue") +
      geom_density(alpha=.4, fill = "red") # alpha determines how transparent it is
p3

p4 <- ggplot(data=height, aes(x=height, color = sex)) +
      geom_histogram(aes(y=..density..), fill = "white", position="identity")      # Position identity for overlapping histograms
p4 

library(ggpubr)
p <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = F, legend="bottom")
p
