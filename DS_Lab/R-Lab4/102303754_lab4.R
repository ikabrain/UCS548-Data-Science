# UCS548 Lab Assignment 4
# `seq(from, to, by, length.out, along.with)` => Generates vector sequence starting from a numeric to another, following an AP !
# `rep()` => Repeats value, character, or vector multiple times
#               times:  If a single integer, whole data is repeated that many times.
#                       If a vector of the same length as x, x[i] is repeated times[i] times.
#                       If a named vector, the names are matched to elements of x to determine repetition counts.
#               each: Repeats EACH ELEMENT some number of times
#               length.out: desired length of output vector



# Q1. Write R code to generate the following vectors, explore the functions seq() and rep() using the help of commands:
#       (a) 1.3 1.6 1.9 2.2 2.5 2.8 3.1 3.4 3.7 4.0 4.3 4.6 4.9
seq(1.3, 4.9, 0.3)
#       (b) 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
rep(seq(1, 4), 5)
#       (c) 14 12 10 8 6 4 2 0
seq(14, 0, -2)
#       (d) 5 5 12 12 13 13 20 20
rep(c(5, 12, 13, 20), each=2)



# Q2. Load the iris data that R provides internally by typing data(iris)
data(iris) # Just returns the string name "iris". Actual data.frame is in `iris` var!
head(iris)

#       A. What sort of data type is iris?
class(iris)

#       B. How many rows (observations) and columns (variables) does the iris dataset have?
r <- nrow(iris)
c <- ncol(iris)
print(paste("No. of observations in `iris` =", r))
print(paste("No. of variables in `iris` =", c))

#       C. Which variable of the data frame iris is a factor and how many levels does it have?
str(iris)
#           Therefore, (b) The variable Species is a factor and it has 3 levels.


# Q3. Use the “iris” dataset to find

#       a) The mean and standard deviation of the sepal width and sepal length for each type of species.
#           Ans. tapply(x, INDEX, FUN = NULL, ..., simplify = TRUE) => Apply a function FUN over subsets INDEX of a vector x
#                colMeans(data.frame) => Finds mean of each column of data.frame
#                sapply(FUN) => Applies a function over each individual element of collection (here, col of data.frame) and simplifies it!
print("Mean of Sepal width and length for each type of species:-")
print(tapply(iris[c("Sepal.Width", "Sepal.Length")], iris[["Species"]], colMeans))
print("Standard Deviation of Sepal width and length for each type of species:-")
print(tapply(iris[c("Sepal.Width", "Sepal.Length")], iris[["Species"]], sapply, sd))

#       b)  Create a new dataset called iris.class from the iris dataset.
#           Use a loop and if-else statement to create a vector in the iris.class dataset called Calyx.Width,
#           which is “short” if Sepal.Length is less than 5, and otherwise is “long”.
#           (The sepals of a flower are collectively known as the calyx.)
Calyx.Width <- character()
for (i in 1:nrow(iris)) {
    if (iris[[i, "Sepal.Length"]] < 5.0) {
        calyx_width <- "short"
    } else {
        calyx_width <- "long"
    }
    Calyx.Width <- c(Calyx.Width, calyx_width)
}
iris.class <- cbind(iris, Calyx.Width)
head(iris.class)



# Q4. Explore dataset- mtcars in R.
#     You can get the structure and column names of data by typing the command
#     str(mtcars) and names(mtcars) respectively.
#     Write your code to subset the dataset mtcars according to the following requirements
#     (NOTE: each requirement is independent.)
data(mtcars)
head(mtcars)
str(mtcars)
names(mtcars)

#       A. Select cars whose cyl (a column in the dataset) value is no smaller than 5.
#           Ans. For conditions, we use data.frame[boolean_mask, ] to get all rows satisfying given condition!
print("Cars with cylinder value no smaller than 5:-")
print(rownames(mtcars[mtcars$cyl >= 5, ]))

#       B. Show all the fields (columns) of the first 10 cars.
head(mtcars, 10L)

#       C. Find all cars matching “Honda”.
#           Ans. grep(pattern, x) => Returns the indices of elements in a character vector `x` that match the `pattern`
#               grepl(pattern, x) => Returns a logical vector indicating whether each element in `x` matches the `pattern`
print(paste("Car(s) with 'Honda' string:", grep("Honda", rownames(mtcars), value = TRUE)))
