# This is a comment, it won't produce any output
3 + 2
14 * 2
0.94^10

my_number <- 442
my_number

my_string <- "Hello world"
my_string

# This is a vector:
vec1 <- c(1,2,3)
vec2 <- c("FM", "442")

# This is a matrix:
mtx1 <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
mtx2 <- matrix(c("a", "b", "c", "d", "e","f"), nrow = 3, ncol = 2, byrow = TRUE)

vec1
vec2
mtx1
mtx2

# Lenght of a vector
length(vec1)

# Dimensions of a matrix
dim(mtx1)

# Second element of vec2
vec2[2]

# Third element of the second column of mtx2
mtx2[3,2]

# We can also change an element this way
vec2[1] <- "Finance"
mtx2[3,2] <- "X"

vec2
mtx2

# You need to specify the first and last element, and the increment
seq1 <- seq(2, 10, by = 2)

# With only one input it will create an integer sequence from 1 
seq2 <- seq(5)

seq1
seq2

# Create a data with two variables frame using data.frame
x <- data.frame("Stock" = c("A", "B"), "Price" = c(42,68))
x

# Accessing a column
x[,2]
x$Price

# Creating a new column
x$Price_plus_1 <- x$Price + 1
x

for (i in 1:10) { # For every element i in 1, 2, ... 10
    print(i^2) # print() displays the value in the console 
}

x <- 10

if (x > 0) {
    print("x has a positive value")
} else {
    print("x has a negative value")
}

# This line will just make our lifes easier
options(stringsAsFactors = FALSE)

# Importing the downloaded data
data <- read.csv('crsp.csv')

# Checking the dimensions
dim(data)

# First observations
head(data)

# A single column
head(data$RET)
head(data[,6])

# Names of the columns
names(data)

# Getting unique values of PERMNO
unique(data$PERMNO)

# Creating a variable for a company
    # We are filtering the dataset for the rows with the City PERMNO
citi <- data[data$PERMNO == 70519,]

# Dimension
dim(citi)

# Check the first few elements
head(citi)

# Check different tickers for the same PERMNO
unique(citi$TICKER)

# Why does this happen?

# Highest return for Citi
highest_citi <- max(citi$RET) * 100
paste0("Highest return for Citi: ", highest_citi, "%")

plot(citi$PRC, type = "l", main = "Price of Citi")
