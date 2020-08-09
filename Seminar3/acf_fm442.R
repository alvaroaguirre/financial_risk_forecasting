################## ACF FM442 function ##################
#' Function that creates a better looking ACF plot
#' Takes three arguments:
    #' xseries - Time Series
    #' main - Title, by default it is "ACF"
    #' lag.max - Maximum number of lags. By default it is a function of time series length

acf_fm442 <- function(xseries, main = "ACF", lag.max = as.integer(10*log10(length(xseries)))) {
    # Get the ACF values from the acf function 
    acfx 	= acf(xseries, lag.max = lag.max, plot = FALSE)
    # Calculate the confidence interval
    clim0	= qnorm((1 + 0.95)/2)/sqrt(acfx$n.used)
    # Determine the limits of the plot
    ylim 	= range(c(-clim0, clim0, as.numeric(acfx$acf)[-1]))
    # Initialize a vector
    clx 	= vector(mode = "character", length = lag.max)
    # Assign the color steelblue to positive ACF values
    clx[which(as.numeric(acfx$acf)[-1]>=0)] = "steelblue"
    # Assign the color orange to negative ACF values
    clx[which(as.numeric(acfx$acf)[-1]<0)] = "orange"
    # Create a barplot using the ACF values, lags in the x-axis, colors determined above
    barplot(height = as.numeric(acfx$acf)[-1], names.arg = as.numeric(acfx$lag)[-1], ylim = 1.2*ylim, col = clx,
            ylab = "ACF", xlab="lag", main = main, cex.main = 0.8)
    # Add confidence intervals
    abline(h = c(clim0, -clim0), col = "tomato1", lty = 2, lwd = 2)
    abline(h = 0, col = "black", lty = 1)
    # Box it
    box()
}