# Black-Scholes function
bs <- function(K, P, r, sigma, Maturity, type = "Put"){
    d1 <- (log(P/K) + (r+0.5*sigma^2/2)*(Maturity))/(sigma*sqrt(Maturity))
    d2 <- d1 - sigma*sqrt(Maturity)
    Call <- P*pnorm(d1) - K*exp(-r*(Maturity))*pnorm(d2)
    Put <- K*exp(-r*(Maturity))*pnorm(-d2) - P*pnorm(-d1)
    if (type == "Put") {
        return(Put)
        } else if (type == "Call") {
        return(Call)
    } else {
        return("Not a valid type")
    }  
}

# Load JPM stock
load("Y.RData")
y <- Y$JPM
plot(y, type = "l", main = "JPM stock")

# Estimating volatility with returns
n <- length(y)
sigma <- sd(y)*sqrt(250)
sigma

# Getting the last price available for JPM
load("PRC.RData")
P <- tail(PRC$JPM,1)
P

# Assuming the necessary parameters
r <- 0.03
Maturity <- 1
K <- 100

# Calculating the option price
put <- bs(K = K, P = P, r = r, sigma = sigma, Maturity = Maturity)
put

# Option price as a function of underlying stock price

# Sequence of underlying prices
x <- seq(50, 150, length = 1000)

# Plot option price
plot(x, bs(K = K, P = x, r = r, sigma = sigma, Maturity = Maturity),
    type = "l", ylab = "Option", xlab = "Underlying price", lwd = 3, col = "pink", las = 1,
    bty = "l", main = "Price of put option depending on underlying stock price")

# Add a point for the observed price
points(P, bs(K=K, P=P, r=r, sigma=sigma, Maturity = Maturity),
       pch = 16, col="blue")

# Option price as a function of Strike price

# Sequence of Strike
x <- seq(50, 150, length = 1000)

# Plot option price
plot(x, bs(K = x, P = P, r = r, sigma = sigma, Maturity = Maturity),
    type = "l", ylab = "Option", xlab = "Strike", lwd = 3, col = "pink", las = 1,
    bty = "l", main = "Price of put option depending on Strike price")

# Add a point for the observed price
points(K, bs(K=K, P=P, r=r, sigma=sigma, Maturity = Maturity),
       pch = 16, col="blue")

# Option price as a function of risk-free rate

# Sequence of risk-free
x <- seq(0.01, 0.2, length = 1000)

# Plot option price
plot(x, bs(K = K, P = P, r = x, sigma = sigma, Maturity = Maturity),
    type = "l", ylab = "Option", xlab = "Risk-free", lwd = 3, col = "pink", las = 1,
    bty = "l", main = "Price of put option depending on risk-free rate")

# Add a point for the observed price
points(r, bs(K=K, P=P, r=r, sigma=sigma, Maturity = Maturity),
       pch = 16, col="blue")

# Option price as a function of sigma

# Sequence of sigma
x <- seq(0.01, 0.4, length = 1000)

# Plot option price
plot(x, bs(K = K, P = P, r = r, sigma = x, Maturity = Maturity),
    type = "l", ylab = "Option", xlab = "Sigma", lwd = 3, col = "pink", las = 1,
    bty = "l", main = "Price of put option depending on sigma")

# Add a point for the observed price
points(sigma, bs(K=K, P=P, r=r, sigma=sigma, Maturity = Maturity),
       pch = 16, col="blue")

# Option price as a function of Maturity

# Sequence of maturity
x <- seq(0.1, 10, length = 1000)

# Plot option price
plot(x, bs(K = K, P = P, r = r, sigma = sigma, Maturity = x),
    type = "l", ylab = "Option", xlab = "Maturity", lwd = 3, col = "pink", las = 1,
    bty = "l", main = "Price of put option depending on maturity")

# Add a point for the observed price
points(Maturity, bs(K=K, P=P, r=r, sigma=sigma, Maturity = Maturity),
       pch = 16, col="blue")

# Using Monte Carlo

# Simulations
S <- 1e6

# Set seed for replicability
set.seed(420)

# Get 1e6 simulations from a standard normal
ysim <- rnorm(S)

# Plot the random numbers
plot(ysim)
hist(ysim)

# Centering in the desired mean and changing the variance
ysim <- ysim - 0.5*sigma^2*Maturity
ysim <- ysim*sigma*sqrt(Maturity)

# Specifying mean/sd in rnorm directly
set.seed(420)
ysim <- rnorm(S, mean=-0.5*sigma^2*Maturity, sd = sigma*sqrt(Maturity))

# Histogram
hist(ysim, probability = TRUE)

# Simulating future prices of stock 
Fsim <- P*exp(r*Maturity)*exp(ysim)
hist(Fsim, probability = TRUE)

# Simulated price of stock
Psim <- K - Fsim

# Replacing negatives with zero
Psim[Psim<0] <- 0

# Histogram
hist(Psim, probability = TRUE)

# Comparing analytical and simulation prices
Psim <- Psim*exp(-r*Maturity)

cat("Analytic price:", put, "\n",
    "Simulation price:", mean(Psim))

# Experimenting using Simulation sizes
# For a given simulation size S, say S = 100
S <- 100

# Set the seed, e.g. 100
set.seed(100)

# Do the simulation
ysim <- rnorm(S,
             mean = -0.5*sigma^2*Maturity,
             sd = sigma*sqrt(Maturity))

# Get the simulated paths
Fsim <- P*exp(r*Maturity)*exp(ysim)

# Subtract from strike price and only exercise the option if its positive
Psim <- K - Fsim
Psim[Psim<0] <- 0
Psim <- Psim * exp(-r*Maturity)

round(mean(Psim),4)

# Function on simulation size and seed
Sim <- function(S, seed = 999, message = TRUE){
    set.seed(seed)
    ysim <- rnorm(S,
                  mean = -0.5*sigma^2*Maturity,
                  sd = sigma*sqrt(Maturity))
    Fsim <- P*exp(r*Maturity)*exp(ysim)
    Psim <- K-Fsim
    Psim[Psim<0] <- 0
    Psim <- Psim*exp(-r*Maturity)
    if (message) {
        cat("**************************", "\n",
            "Number of simulations:", S, "\n",
           "Using seed:", seed, "\n",
           "Simulated price:", mean(Psim),"\n",
           "\n")
    }
    return(mean(Psim))
}

# Different simulation sizes
x <- c(10, 100, 1000, 10000, 100000, 1000000)
for(size in x){
    Sim(size, seed = 420)
}

# Plot converge of price
seed <- 420

# Sequence of simulations
x <- seq(100, 100000, length = 100)

# Simulation price of each
P_sim <- sapply(x, Sim, seed = seed, message = FALSE)

# Plot
plot(x, P_sim, type = "l", main = "Simulated price for option", sub = paste0("Seed: ", seed),
    xlab = "Simulation Size", ylab = "Price", las = 1)

# Add horizontal line on Analytic price
abline(h = put, col = "red")
axis(2, at = put, label = round(put,2), las = 1)

# Different seeds

seed <- 666

# Simulation price of each
P_sim <- sapply(x, Sim, seed = seed, message = FALSE)

# Plot
plot(x, P_sim, type = "l", main = "Simulated price for option", sub = paste0("Seed: ", seed),
    xlab = "Simulation Size", ylab = "Price", las = 1)

# Add horizontal line on Analytic price
abline(h = put, col = "red")
axis(2, at = put, label = round(put,2), las = 1)

seed <- sample(100:999,1)

# Simulation price of each
P_sim <- sapply(x, Sim, seed = seed, message = FALSE)

# Plot
plot(x, P_sim, type = "l", main = "Simulated price for option", sub = paste0("Seed: ", seed),
    xlab = "Simulation Size", ylab = "Price", las = 1)

# Add horizontal line on Analytic price
abline(h = put, col = "red")
axis(2, at = put, label = round(put,2), las = 1)

# Simulating arithmetic returns

# Specifying VaR probability
probability <- 0.01

# Sample standard deviation
sigma <- sd(y)

# Number of simulations
S <- 1e3

# Set a seed
seed <- 456
set.seed(seed)

# Draw from a normal distribution, using daily rate and standard deviation
ysim <- rnorm(S, mean = r/365, sd = sigma)

# Simulate prices
Psim <- P*(1+ysim)

# Sort the simulated profit/loss
q <- sort(Psim - P)

# VaR from quantile
VaR1s <- -q[ceiling(probability*S)]

# Round
round(VaR1s,2)

# Analytical VaR
VaR1t <- abs(sigma*qnorm(probability)*P)
round(VaR1t,2)

# Function that computes simulation VaR for a number of simulations and seed
simVaR <- function(S, seed = 999, message = TRUE) {
    # Include elapsed time
    old <- Sys.time()
    
    set.seed(seed)
    ysim <- rnorm(S, mean = r/365, sd = sigma)
    Psim <- P*(1+ysim)
    q <- sort(Psim - P)
    VaR1s <- -q[ceiling(probability*S)]
    if (message) {
        cat("Number of simulations", S, "\n")
        cat("Elapsed time:", difftime(Sys.time(), old, units = "secs"), "seconds")
    }
    
    return(VaR1s)
}

round(simVaR(S = 1e4, seed = seed),2)
round(simVaR(S = 1e6, seed = seed),2)

# Plotting

# Sequence of simulations
x <- seq(100, 1000000, length = 100)

# Simulation VaR of each
VaR_sim <- sapply(x, simVaR, seed = seed, message = FALSE)

# Plot
plot(x, VaR_sim, type = "l", main = "Simulated VaR for stock", sub = paste0("Seed: ", seed),
    xlab = "Simulation Size", ylab = "VaR", las = 1)

# Add horizontal line on Analytic VaR
abline(h = VaR1t, col = "red")

# VaR for the option

# Calculate put
put <- bs(K = K, P = P, r = r, sigma = sqrt(sigma^2 * 250), Maturity = Maturity)

# Simulate prices
S <- 1e3
set.seed(444)
ysim <- rnorm(S, mean=r/365, sd=sigma)
Psim <- P*(1+ysim)

# Use simulated prices in Black-Scholes formula
fsim <- bs(K=K, P=Psim, r=r, sigma=sqrt(sigma^2 *250), Maturity = Maturity-(1/365))

# Sort the vector
q <- sort(fsim - put)

# Get the value that meets the VaR probability
VaR2 <- -q[ceiling(probability*S)]
round(VaR2,3)

# VaR for a portfolio

# Assets
xb <- 3
xo <- 2

# Create portfolio
portfolio_today <- xb*P + xo*put
round(portfolio_today)

# Calculating tomorrow's value of the portfolio
option_tomorrow <- bs(K = K, P = P, r = r, sigma = sqrt(sigma^2 * 250), Maturity = Maturity - (1/365))
portfolio_tomorrow <- xb*Psim + xo*option_tomorrow

# Find the profit-loss
portfolio_loss <- portfolio_tomorrow - portfolio_today

# We can see its distribution
hist(portfolio_loss, main = "P/L distribution")

# Find VaR
portfolio_loss <- sort(portfolio_loss)
portfolio_VaR <- -portfolio_loss[ceiling(S*probability)]
round(portfolio_VaR,2)

# Introducing a second option
xo2 <- 15
K2 <- 90

# Get the prices using Black-Scholes
option2_today <- bs(K=K2, P=P, r=r, sigma=sqrt(sigma^2 *250), Maturity=Maturity)
option2_tomorrow <- bs(K=K2, P=Psim, r=r, sigma=sqrt(sigma^2 * 250), Maturity=Maturity-(1/365))

# Portfolio values
portfolio_today <- xb*P + xo*put + xo2*option2_today
portfolio_tomorrow <- xb*Psim + xo*option_tomorrow + xo2*option2_tomorrow

# Find the profit-loss
portfolio_loss <- portfolio_tomorrow - portfolio_today

# We can see its distribution
hist(portfolio_loss, main = "P/L distribution")

# Find VaR
portfolio_loss <- sort(portfolio_loss)
portfolio_VaR <- -portfolio_loss[ceiling(S*probability)]
round(portfolio_VaR,2)
