# UCS548 Lab Assignment 7 - apply() family



# Q1. Create a data matrix named MARKS having marks of three subjects SUB1, SUB2 and SUB3 for 20 students.

# To generate random marks for each SUB as a truncated normal distribution
generate_marks <- function(n = 1, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
    if (lower >= upper) {
        stop("Lower bound must be less than upper bound")
    }
    alpha <- pnorm(lower, mean, sd)
    beta <- pnorm(upper, mean, sd)
    u <- runif(n, min = alpha, max = beta)
    samples <- qnorm(u, mean, sd)
    return(round(samples))
}

# Defining data
maxmark <- 100

MARKS <- matrix(
    data = generate_marks(20*3, mean = 70, sd = 20, lower = 0, upper = maxmark),
    nrow = 20,
    ncol = 3,
)
colnames(MARKS) <- c("SUB1", "SUB2", "SUB3")
MARKS

# Use apply function
# a. To find total marks of each students.
# Same as fixing row index, summing across cols (margin = 1)
TOTAL <- apply(MARKS, 1, sum) # OR total_marks <- rowSums(MARKS)
TOTAL

# b. Append the total to the given MARKS dataset.
MARKS1 <- cbind(MARKS, TOTAL)
MARKS1

# c. To create a function called st.err()=sd(x)/sqrt(length(x)) to find the standard error in SUB1, SUB2, and SUB3.
# Same as fixing col index, sd() across rows (margin = 2)
st.err <- function(x) {apply(x, 2, sd) / sqrt(nrow(x))}
st.err(MARKS)

# d. Add 0.25 bonus marks to each mark in SUB1, SUB2 and SUB3.
MARKS2 <- apply(MARKS, 2, `+`, 0.25)
MARKS2



# Q2. Create three vectors V1, V2, and V3 from the SUB1, SUB2, and SUB3 above respectively.
#     Use lapply() function to find the sum of all the marks in V1, V2, and V3.
V1 <- MARKS[, "SUB1"]
V2 <- MARKS[, "SUB2"]
V3 <- MARKS[, "SUB3"]
lapply(list(V1 = V1, V2 = V2, V3 = V3), sum)



# Q3. Create a vector TOTAL_SUM that hold the value of V1, V2, and V3 using sapply().
TOTAL_SUM <- sapply(list(V1 = V1, V2 = V2, V3 = V3), sum)
TOTAL_SUM



# Q4. Compute the square of each value of marks in V1, V2, and V3 using sapply().
sapply(list(V1 = V1, V2 = V2, V3 = V3), `^`, 2)



# Q5. Add an index field I =(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4) to matrix MARKS.
#     Use tapply() function to compute mean() and sd() of SUB1 based on index I.
I <- rep(1:4, each = 5)
MARKSI <- cbind(MARKS, I)
MARKSI

print("Mean of SUB1 = ")
tapply(MARKSI[, "SUB1"], MARKSI[, "I"], mean)
print("Standard Deviation of SUB1 = ")
tapply(MARKSI[, "SUB1"], MARKSI[, "I"], sd)



# Q6. Create a function f(x,y)=x/y where x is V1 and y is V2.
#     Use mapply() to compute this function.
f <- function(x, y) {x / y}
mapply(f, V1, V2)



# Q7. Practice all apply functions on “Seatbelts” data set given in R.
data(Seatbelts)
sb_df <- as.data.frame(Seatbelts)   # Convert dataset to dataframe
time_info <- time(Seatbelts)        # Extract time information
sb_df$year <- floor(time_info)      # Get year
sb_df$month <- cycle(Seatbelts)     # Get month
head(sb_df)

# lapply: Convert kms to miles
miles1 <- lapply(sb_df$kms, `*`, 0.6214)
head(miles1, 3)

# sapply: Append miles vector to Seatbelts
miles <- sapply(sb_df$kms, `*`, 0.6214)
S1 <- cbind(sb_df, miles)
head(S1)

# apply: Find the mean value of all statistics in columns
Smean <- apply(subset(sb_df, select = -c(year, month)), 2, mean)
round(Smean, 4)

# tapply: Find mean drivers killed for each year
Smean_year <- tapply(sb_df$DriversKilled, sb_df$year, mean)
round(Smean_year, 4)

# mapply: Front Seat : Rear Seat Passengers injured ratio
Sratio <- mapply(`/`, sb_df$front, sb_df$rear)
round(head(Sratio), 2)
