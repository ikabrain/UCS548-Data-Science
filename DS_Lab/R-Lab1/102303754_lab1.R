# UCS548 Assignment 1



# Q1. Assign and print the values 23.4, 45 and 678 to the variables A, B, C.
A <- mst_marks <- 23.4 # LOCAL
45 -> B -> est_marks
C <<- 678 # GLOBAL
print(paste0("MST Marks A = ", A))
print(paste0("EST Marks B = ", B))
print(paste0("Net Marks of all subjects C = ", C))



# Q2. Display the entire variable you have created on the screen.
?ls
ls()



# Q3. Remove the variable C and display the list.
help(rm)
rm(C)
ls()



# Q4. Create a comment “I am learning R”
# I am learning R



# Q5. Create strings firstname and lastname as “MyName” and “MySurname”
firstname <- "Ikansh"
lastname <- "Mahajan"
print(paste("My name is", firstname, lastname))



# Q6. Create the variable that can hold a value as 0 or 1.
BooleanVar <- function(x = 0) {
    if (!is.numeric(x) || !(x %in% c(0, 1))) {
        stop("ERROR: BooleanVar can only hold 0/1.")
    }
    return(x)
}

pass <- BooleanVar(5)

pass <- BooleanVar(1)
if (pass == 1) {
    print("Student has passed.")
} else {
    print("Student has failed.")
}



# Q7. Perform the operation as +, -, * and / on variables A, B, C together.
A <- as.numeric(readline(prompt = "Enter marks in MST: "))
B <- as.numeric(readline(prompt = "Enter marks in EST: "))
C <- as.numeric(readline(prompt = "Enter course credit / mark: "))

print(paste("Sum of marks =", A + B))
print(paste("Improvement in marks =", B - A))
print(paste("Credits earned =", (A + B) * C))
print(paste("Average marks per credit =", round(((A + B) / C), 2)))



# Q8. Apply the following functions on some values: Exp(), log(), log10(), log2(), pi, sqrt()

# EXPONENTIAL: Compound interest
P <- as.numeric(readline(prompt = "Enter principal amount: "))
r <- as.numeric(readline(prompt = "Enter annual interest rate (%): ")) / 100
t <- as.numeric(readline(prompt = "Enter time in years: "))
final_amount <- P * exp(r * t)
print(paste("Final amount after", t, "years =", round(final_amount, 2)))

# LOGARITHM: Half-life calculation
C0 <- as.numeric(readline(prompt = "Enter initial drug concentration: "))
Ct <- as.numeric(readline(prompt = "Enter remaining concentration: "))
t <- as.numeric(readline(prompt = "Enter time in hours: "))
k <- -log(Ct / C0) / t
print(paste("Elimination rate constant k =", round(k, 4)))

# LOG 10: pH calculation
H_conc <- as.numeric(readline(prompt = "Enter [H+] ion concentration in mol/L: "))
pH <- -log10(H_conc)
print(paste("pH of the solution =", round(pH, 2)))

# LOG 2: Bits required to store messages
messages <- as.numeric(readline(prompt = "Enter number of unique messages: "))
bits_required <- log2(messages)
print(paste("Minimum bits required to encode =", round(bits_required, 2)))

# PI: Area of circle
radius <- as.numeric(readline(prompt = "Enter radius of the circle in meters: "))
area <- pi * radius^2
print(paste("Area of the circle =", round(area, 2), "sq. meters"))

# SQRT: Euclidean distance
x1 <- as.numeric(readline(prompt = "Enter x-coordinate of point A: "))
y1 <- as.numeric(readline(prompt = "Enter y-coordinate of point A: "))
x2 <- as.numeric(readline(prompt = "Enter x-coordinate of point B: "))
y2 <- as.numeric(readline(prompt = "Enter y-coordinate of point B: "))
distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
print(paste("Distance between points A and B =", round(distance, 2)))



# Q9. Write the statement to solve the following expressions:-

# 1. 23 + (4.5 * 2.3) / 10
print(23 + ((4.5 * 2.3) / 10))

# 2. 456 / 12 – log(90)
print((456 / 12) - log(90))

# 3. Exp(5) + 12 / (5 ^ 6)
print(exp(5) + (12 / (5^6)))

# 4. √45 * 12/3
print(sqrt(45) * (12 / 3))
