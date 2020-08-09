% London School of Economics
% Jon Danielsson, Department of Finance
% FM442 Classwork - MT 2020

The practical content of the course will be delivered in eight seminars. Each seminar consists of a notebook with the material that will be covered in the session and homework exercises that you are expected to solve on your own. 

The course also includes a support notebook with discussion on more advanced R topics that can be useful for your assignments and final project.

All the material is available for download on Moodle and in the  _Exercises_ section of the *Financial Risk Forecasting* [**book page**](https://www.financialriskforecasting.com/exercises/).

***

# Seminar One

* Familiarize ourselves with R and RStudio
* Learn some basic commands
* Download and import financial data
* Create a simple plot

### Homework

* Download the stock prices and returns from Amazon and Apple between January 1st, 1990 and December 31st, 2010
* Load the data into R
* Create two variables "amazon" and "apple" that hold the downloaded data of each stock
* Do a simple plot for each of the stock's returns


# Seminar Two

* Install and load packages in R
* Basic Data Handling
* Save created Data Frames
* Create, customize and export plots

### Homework

* Open the data from Amazon and Apple from the previous seminar
* Use the dcast function to create RET and PRC data frames with the returns and prices 
* Make sure your returns are compounded and not simple
* Save each data frame in your working directory
* Plot the returns and prices for each stock, well-labeled, using a 2x2 grid
* Export your plot


# Seminar Three

* Learn how to work with statistical distributions
* Explore random numbers and the Monte Carlo simulation
* Visualize, analyse, and comment on the prices of a stock
* Perfrom graphical analyses and statistical tests

### Homework

Elaborate a commentary on the price of the Amazon stock. To do this:

* Load the returns data frame you created in Seminar Two
* Plot the returns for Amazon 
* Zoom into the dot-com years (2000-2002)
* Find the best and worst perfoming days, and find out what happened on those dates
* Compare the returns to a normal distribution graphically and using a test
* Plot the ACF of the returns and returns squared
* Use a 2x2 grid to elaborate four QQplots comparing the returns to a Student-T of 2, 3, 4 and 5 degrees of freedom


# Seminar Four

* Build unvariate GARCH models
* Plot GARCH outputs
* Work with various specifications (ARCH, Student t, apARCH)
* Relax the GARCH stationarity condition
* Assess model quality using likelihood ratio tests and residual analysis
* Learn about half-life and GARCH simulations

### Homework

Choose a stock between Amazon and Apple and:

* Fit a univariate GARCH(1,1) where conditional returns follow a normal distribution
* Fit a univariate GARCH(1,1) where conditional returns follow a T-distribuion
* Plot the estimated conditional volatilities against each other and comment
* Fit an ARCH(1,1) model and perform a LR test versus the GARCH(1,1)


# Seminar Five

* Introduce multivariate volatility models
* Build a bivariate EWMA model
* Run DCC models with different specifications
* Compare models

### Homework

* Load the Amazon and Apple returns
* Build a bivariate EWMA model for these stocks
* Build a loop that creates an EWMA for values of lamba = 0.9, 0.91, 0.92,... 0.99
* Create one plot for each stock with the estimated conditional volatilies for the different lambda
* Fit a DCC tapARCH model and calculate the conditional correlation
* Plot it and zoom into the dot-com bubble period (2000-2002)
* Comment any findings


# Seminar Six

* Solve exam-like questions regarding risk measures
* Perform univariate and multivariate estimations of ES and VaR using Historical Simulation
* Experiment with different estimation windows for Historical Simulation VaR
* Build an EWMA VaR model

### Homework

* Load the Amazon and Apple returns
* Perform a Historical Simulation VaR 99% forecast for each stock, use an estimation window of 1000 days
* Perform a Historical Simulation VaR 95% forecast for each stock, use an estimation window of 1000 days
* For each stock, plot the HS VaR forecast for both 95% and 99% in the same plot
* Comment


# Seminar Seven

* Implement GARCH VaR
* Analyze and compare VaR forecasts between models
* Perform backtests with violation ratios
* Implement multivariate EWMA and HS VaR
* Perform stress-testing

### Homework

* Use the six models covered (EWMA, HS300, HS1000, HS2000, GARCH300, GARCH2000) to backtest 1% VaR for both Amazon and Apple stocks individually. 
* Perform a stress-test in the dot-com years (2000-2002)

 
# Seminar Eight

* Use the Black-Scholes equation to price an option
* Simulate option prices with Monte Carlo
* Experiment with different simulation sizes
* Use analytical and simulation methods to get VaR
* Calculate VaR for an option
* Calculate VaR for a portfolio of stock and option 

### Homework

* Repeat the seminar for a Call option instead of a Put

 
# Seminars Nine and Ten

Group project presentations