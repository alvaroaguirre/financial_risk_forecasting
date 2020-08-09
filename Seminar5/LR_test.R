############### LR Test function ###############

# This function takes three arguments:
    # First argument: fitted restricted model
    # Second argument: fitted unrestricted model
    # Third argument: model type (GARCH vs DCC)

# Performs a LR test and outputs the degrees of freedom, likelihoods, LR statistic, and p-value

LR_test <- function(restricted, unrestricted, model = "GARCH") {
    
    # Specifying the degrees of freedom. Model type determines if "fit" or "mfit"
    if (model == "GARCH") {
        df <- length(unrestricted@fit$coef) - length(restricted@fit$coef)
    } else if (model == "DCC") {
        df <- length(unrestricted@mfit$coef) - length(restricted@mfit$coef)
    } else {
        return("Supports GARCH and DCC models")
    }
    
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