library(reshape2)
library(lubridate)

data <- read.csv("crsp.csv")
head(data)

class(data)

# Keeping the unadjusted prices in a new column
data$Unadjusted_PRC <- data$PRC

# Modifying the PRC column
data$PRC <- data$PRC / data$CFACPR

head(data)

# Getting the date and PRC variables for Microsoft
MSFT <- data[data$PERMNO == 10107, c("date", "PRC")]

# Renaming PRC to MSFT
names(MSFT)[2] <- "MSFT"

# Now we do the same for the five others

XOM <- data[data$PERMNO==11850, c("date", "PRC")]
names(XOM)[2] <- "XOM"

GE <- data[data$PERMNO==12060, c("date", "PRC")]
names(GE)[2] <- "GE"

JPM <- data[data$PERMNO==47896, c("date", "PRC")]
names(JPM)[2] <- "JPM"

INTC <- data[data$PERMNO==59328, c("date", "PRC")]
names(INTC)[2] <- "INTC"

C <- data[data$PERMNO==70519, c("date", "PRC")]
names(C)[2] <- "C"

# And merge all into a single table called PRC using the merge() function
PRC <- merge(MSFT, XOM)
PRC <- merge(PRC, GE)
PRC <- merge(PRC, JPM)
PRC <- merge(PRC, INTC)
PRC <- merge(PRC, C)
head(PRC)

# Remove the previous variable
rm(PRC)

# Create a new data frame 
PRC <- dcast(data, date ~ PERMNO, value.var = "PRC")
names(PRC) <- c("date", "MSFT", "XOM", "GE", "JPM", "INTC", "C")
head(PRC)

RET <- dcast(data, date ~ PERMNO, value.var = "RET")
names(RET) <- c("date", "MSFT", "XOM", "GE", "JPM", "INTC", "C")
head(RET)

# We choose all the columns except the first one 
# And transform them into a new Y data frame
Y <- log(1 + RET[,2:7])
Y$date <- RET$date
head(Y)

class(Y$date)

# Use the function ymd() to transform the column into Dates
Y$date <- ymd(Y$date)
# Check if it worked
class(Y$date)

# Lets do the same for PRC
PRC$date <- ymd(PRC$date)

# Saving the data frame of returns
save(Y, file = "Y.RData")

# Saving the data frame of prices
save(PRC, file = "PRC.RData")

# Remove the existing data frame of returns
rm(Y)

# Load the saved file
load("Y.RData")
head(Y)

write.csv(Y, file = "Y.csv")

?plot

# Simple plot, if we do not specify an X variable, plot() will use an index
plot(Y$JPM)

# By default, plot() uses points, we can plot a line with the option "type"
plot(Y$JPM, type = "l")

# We can add a title with the option "main"
# Change the axes labels with "xlab" and "ylab"
# Choose a color for the graph with "col"
plot(Y$JPM, type = "l", main = "Compound returns for JP Morgan", 
    ylab = "Returns", xlab = "Observation", col = "red")

# The first data argument is used as the X variable
plot(Y$date, Y$JPM, type = "l", main = "Compound returns for JP Morgan", 
    ylab = "Returns", xlab = "Date", col = "red")

# The first data argument is used as the X variable
plot(Y$date, Y$JPM, type = "l", main = "Compound returns for JP Morgan", 
    ylab = "Returns", xlab = "Date", col = "red", las = 1)

# First we plot the returns of JPM
plot(PRC$date, PRC$JPM, type = "l", main = "Prices for JP Morgan and Citi", 
    ylab = "Price", xlab = "Date", col = "red")

# Then we add the returns of C
lines(PRC$date, PRC$C, col = "blue")

# And we create a legend
legend("bottomright",legend = c("JPM", "C"), col = c("red", "blue"), lty=1)

plot(PRC$date, PRC$JPM, type = "l", main = "Prices for JP Morgan and Citi", 
    ylab = "Price", xlab = "Date", col = "red", ylim = c(0, 600))

lines(PRC$date, PRC$C, col = "blue")

legend("topright",legend = c("JPM", "C"), col = c("red", "blue"), lty=1)

# Extract Microsoft data
msft <- data[data$PERMNO == 10107,]

# Plot the adjusted prices
plot(msft$PRC, type = "l", col = 1, main = "Adjusted and unadjusted prices for MSFT",
    ylab = "USD")

# Plot the unadjusted prices
lines(msft$Unadjusted_PRC, col = 2)

# Adding a legend
legend("bottomright",legend = c("Adjusted", "Unadjusted"), col = c(1:2), lty=1)

# Plotting all prices
matplot(PRC[,2:7], type = "l", main = "Prices for our stocks", 
    ylab = "Price", lty = 1)

# Adding the legend
legend("topright", legend = names(PRC[,2:7]), col = c(1:6), fil = c(1:6))

matplot(RET[,2:7], type = "l", main = "Returns for our stocks", 
    ylab = "Returns", lty = 1)

# Adding the legend
legend("bottomright", legend = names(RET[,2:7]), col = c(1:6), fil = c(1:6))

# Divide the space into a 3x2 grid
par(mfrow = c(3,2))

# We will use a FOR loop, which repeats a same piece of code as many times as we indicate
for (i in 2:7) { 
    # The code will be repeated for i = 2, 3, ..., 7
    # In each instance, we will plot the i-th column of PRC
    plot(PRC$date, PRC[,i], type = "l", ylab = "Returns", xlab = "Date",
         main = paste("Returns for", names(PRC)[i]))
}
