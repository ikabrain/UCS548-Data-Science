# UCS548 Assignment 2



# Q1. Create an array A with elements (12, 13, 14, 15, 16) and display them.
A <- array(c(12, 13, 14, 15, 16))
print(paste("Array A = ", paste(A, collapse=", ")))



# Q2. Find the sum of all the elements of A.
sum_A <- sum(A)
print(paste("Sum of A =", sum_A))



# Q3. Find the product of all the element of A.
prod_A <- prod(A)
print(paste("Product of A =", prod_A))



# Q4. Find the maximum and minimum element of A
max_A <- max(A)
min_A <- min(A)
print(paste("Maximum element of A =", max_A))
print(paste("Minimum element of A =", min_A))



# Q5. Find the range of array A.
range_A <- range(A)
print(paste("Range of A is from", paste(range_A, collapse = " to ")))
print(paste("Range =", max_A - min_A))



# Q6. Find the mean, variance, standard deviation and median of value of A.
mean_A <- mean(A)
var_A <- var(A)
std_A <- sd(A)
med_A <- median(A)
print(paste("Mean of A =", mean_A))
print(paste("Variance of A =", var_A))
print(paste("Standard Deviation of A =", std_A))
print(paste("Median of A =", med_A))



# Q7. Sort the element of A both in increasing and decreasing order and store them in B and C.
B <- sort(A)
C <- sort(A, decreasing = TRUE)
print(paste("Increasing A = B =", paste(B, collapse=", ")))
print(paste("Decreasing A = C =", paste(C, collapse=", ")))



# Q8. Create a matrix of 3x4 to have the set of natural numbers.
r <- 3
c <- 4
n <- r * c
matrix(data = c(1:n), nrow = r, ncol = c, byrow=TRUE)



# Q9. Create MxN matrix by combining A, B and C row-wise (RW) and column-wise(CW).
m <- as.integer(readline(prompt = "Enter number of rows in new matrix: "))
n <- as.integer(readline(prompt = "Enter number of columns in new matrix: "))

D <- c(A, B, C)
E <- c()
if (length(D) != m*n) {
    warning(paste("In the new matrix, data length differs from size of matrix:", length(D), "!=", m, "x", n))
    if (length(D) < m*n) {
        E <- rep(0, m*n - length(D))
    }
}



RW <- matrix(c(D, E), nrow = m, ncol = n, byrow = TRUE)
print("Row-wise Matrix:-")
print(RW)

CW <- matrix(c(D, E), nrow = m, ncol = n, byrow = FALSE)
print("Column-wise Matrix:-")
print(CW)



# Q10. Find the 2 and 3 row element of RW.
RW[2,]
RW[3,]

# Q11. Find the 1 and 4 column of CW.
CW[,1]
CW[,4]



# Q12. Using both RW and CW find sub-matrices having elements [2, 3] and [2, 4].
# A12.  A sub-matrix with element @ [2, 3] can have
#       rows starting from 1 - 2 and ending from 2 - nrows.
#       And, for each of those possibilities,
#       columns can start from 1 - 3 and end from 3 - ncols!!!

# Function to get sub-matrices
all_submatrices <- function(mat, r, c) {
    nrows <- nrow(mat)
    ncols <- ncol(mat)

    subs <- list()
    keys <- character() # Gives me an empty vector of type `string`/`character`!!!
    idx <- 1
    for (r_start in 1:r) {
        for (r_end in r:nrows) {
            for (c_start in 1:c) {
                for (c_end in c:ncols) {

                    sub <- mat[r_start:r_end, c_start:c_end, drop=FALSE]
                    # drop=FALSE prevents the automatic simplification of a single column/row into a vector!

                    key <- paste(nrow(sub), ncol(sub), paste(sub, collapse=","), sep="|")
                    # `collapse` just flattens my matrix with a separator

                    if (key %in% keys) {
                        next # Skips to next sub-matrix
                    } else {
                        subs[[idx]] <- sub
                        keys <- c(keys, key)
                        idx <- idx + 1
                    }
                }
            }
        }
    }

    return(subs)
}

all_submatrices(RW, 2, 3)
all_submatrices(CW, 2, 4)
