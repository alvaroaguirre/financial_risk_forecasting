library(rugarch)
library(tseries)
library(car)

# Load the data
load("Y.RData")

# Save the returns in a variable called y
y <- Y$JPM

# Simple plot
plot(y, type = "l", main = "Returns for JP Morgan")

# Autocorrelations
source("acf_fm442.R")
acf_fm442(y, main = "Autocorrelations of JPM returns")
acf_fm442(y^2, main = "Autocorrelations of JPM returns squared")

# Create the specifications
default_spec <- ugarchspec()

# We can call the variable to see what's inside
default_spec

# Fit the model to the data using ugarchfit
default_garch <- ugarchfit(spec = default_spec, data = y)

# Call the variable
default_garch

# Check the content of default_garch
names(default_garch@fit)

# Extract a particular element
default_garch@fit$coef

# Extracting coefficients with coef()
coef(default_garch)

# Accessing omega using its index
coef(default_garch)[4]

# Accessing omega using its name
coef(default_garch)["omega"]

likelihood(default_garch)

# Importing function that only outputs what we are interested in
source("garch_fm442.R")

# Call it
garch_fm442(default_garch)

# Plotting the estimated conditional volatility
plot(default_garch@fit$sigma, type = "l", main = "Esimated conditional volatilitty for JPM")

# New specification
spec1 <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Fit the model
GARCH <- ugarchfit(spec = spec1, data = y)

garch_fm442(GARCH)

# Specification for ARCH(1)
spec2 <- ugarchspec(
    variance.model = list(garchOrder = c(1,0)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Fit to the data
ARCH <- ugarchfit(spec = spec2, data = y, solver = "hybrid")

garch_fm442(ARCH)

# Specify tGARCH
spec3 <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "std"
)

# Fit the model
tGARCH <- ugarchfit(spec = spec3, data = y)

coef(tGARCH)

# Specify APARCH model
spec4 <- ugarchspec(
    variance.model = list(model = "apARCH"),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Fit to the data
apARCH <- ugarchfit(spec = spec4, data = y)

# View the coefficients
coef(apARCH)

# Specify tapARCH model
spec5 <- ugarchspec(
    variance.model = list(model = "apARCH"),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "std"
)

# Fit to the data
tapARCH <- ugarchfit(spec = spec5, data = y)

garch_fm442(tapARCH)

# Subset the data
subset <- y[4000:5000]

spec <- ugarchspec()

# Stationary GARCH - Default
stationary_GARCH <- ugarchfit(spec = spec, data = subset)

# Non-stationary GARCH - using fit.control = list(stationarity = 0)
non_stationary_GARCH <- ugarchfit(spec = spec, data = subset, fit.control = list(stationarity = 0))

# Compare alpha + beta
cat("Stationary alpha + beta:", sum(coef(stationary_GARCH)[c("alpha1", "beta1")]), "\n",
   "Nonstationary alpha + beta:", sum(coef(non_stationary_GARCH)[c("alpha1", "beta1")]))

plot(stationary_GARCH@fit$sigma, type = "l", col = "black")
lines(non_stationary_GARCH@fit$sigma, col="red")
legend("topleft", legend = c("Stationary", "Non-stationary"), lty = 1, col = c("black", "red"))

# Create a new variable with the absolute difference between stationary and non-stationary
dif <- abs(stationary_GARCH@fit$sigma - non_stationary_GARCH@fit$sigma)

# Plot it
plot(dif, type = "l", main = "Distance in Stationary and Non-stationary models cond volatility")

# Number of restrictions ARCH vs GARCH
coef(ARCH)
coef(GARCH)

# Number of restrictions ARCH vs tGARCH
coef(ARCH)
coef(tGARCH)

# Creating the LR statistic - using cat() for output display
cat(" Likelihood of ARCH: ", round(likelihood(ARCH),2), "\n", 
    "Likelihood of GARCH: ", round(likelihood(GARCH),2), "\n",
   "2 * (Lu - Lr): ", round(2*(likelihood(GARCH)-likelihood(ARCH)),2))

# 95% quantile of chi-square
qchisq(p = 0.95, df = 1)

# Adding the p-value
p.value  <- 1 - pchisq(2*(likelihood(GARCH)-likelihood(ARCH)), df = 1)
cat(" Likelihood of ARCH: ", round(likelihood(ARCH),2), "\n", 
    "Likelihood of GARCH: ", round(likelihood(GARCH),2), "\n",
   "2 * (Lu - Lr): ", round(2*(likelihood(GARCH)-likelihood(ARCH)),2), "\n",
   "p-value:", p.value)

# Creating a function for Likelihood Tests
Test <- function(restricted, unrestricted) {
    
    # Specifying the degrees of freedom as the number of restrictions
    df <- length(unrestricted@fit$coef) - length(restricted@fit$coef)
    
    # Creating the statistic
    lr <- 2*(likelihood(unrestricted) - likelihood(restricted))
    
    # Finding its p-value
    p.value <- 1 - pchisq(lr, df)
    
    # Output
    cat("Degrees of freedom:", df, "\n",
        "Likelihood of unrestricted model:", likelihood(unrestricted), "\n",
        "Likelihood of restricted model:", likelihood(restricted), "\n",
        "LR: 2*(Lu-Lr):", lr, "\n",
        "p-value:", p.value
       )
}

# Testing the GARCH and tapARCH
Test(GARCH, tapARCH)

# Performing residual analysis
residuals <- GARCH@fit$residuals

# Autocorrelation of residuals
acf_fm442(residuals, main = "Autocorrelation of residuals")

# Autocorrelation of residuals squared
acf_fm442(residuals^2, main = "Autocorrelation of residuals squared")

# Jarque-Bera test for Normality
jarque.bera.test(residuals)

# Ljung-Box test for residuals
Box.test(residuals, type = "Ljung-Box")

# Ljung-Box test for residuals squared
Box.test(residuals^2, type = "Ljung-Box")

# qqPlot for residuals under a normal distribution
qqPlot(residuals)

# qqPlot for residuals under a Student-t with 3 degrees of freedom
qqPlot(residuals, distribution = "t", df = 3, envelope = FALSE)

# Half-life
# We use unname() to remove the names of the objects
alpha <- unname(coef(GARCH)['alpha1'])
beta <- unname(coef(GARCH)['beta1'])
half_life <- 1 + (log(0.5)/log(alpha+beta))
cat("The Half-life for our GARCH(1,1) is", half_life, "time periods.")

# Create 1000 simulations from our fitted model
simulations <- ugarchsim(GARCH, m.sim = 1000)

# Plot the simulated returns
matplot(simulations@simulation$seriesSim, type = "l",
       main = "Simulations of returns")

# Plot the simulated volatilities
matplot(simulations@simulation$sigmaSim, type = "l", 
       main = "Simulations of volatility")

coef(GARCH)

# Number of simulations
sim <- 1000

# Create vectors to hold the fitted parameters
omega_sim <- vector(length = sim)
alpha_sim <- vector(length = sim)
beta_sim <- vector(length = sim)

# Specify the GARCH we will use
spec <- ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

# Create the simulations
simulations <- ugarchsim(GARCH, m.sim = sim)

# Fit a GARCH for every simulation and store the parameter values
for (i in 1:sim) {
    # Fit the GARCH model
    temp <- ugarchfit(spec = spec1, data = simulations@simulation$seriesSim[,i])
    # Add parameters to vectors created
    omega_sim[i] <- unname(coef(temp)['omega'])
    alpha_sim[i] <- unname(coef(temp)['alpha1'])
    beta_sim[i] <- unname(coef(temp)['beta1'])
}

# Omega

# Distribution
hist(omega_sim, col = "lightgray", main = "Distribution of simulated omega")

# Adding a vertical line on the true parameter
abline(v = coef(GARCH)['omega'], col = "red", lwd = 2)

# Alpha

# Distribution
hist(alpha_sim, col = "lightgray", main = "Distribution of simulated alpha")

# Adding a vertical line on the true parameter
abline(v = coef(GARCH)['alpha1'], col = "red", lwd = 2)

# Beta

# Distribution
hist(beta_sim, col = "lightgray", main = "Distribution of simulated beta")

# Adding a vertical line on the true parameter
abline(v = coef(GARCH)['beta1'], col = "red", lwd = 2)
