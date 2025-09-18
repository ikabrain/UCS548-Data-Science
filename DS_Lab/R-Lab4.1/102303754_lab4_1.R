# UCS548 Lab Assignment - 4.1



# Q1. Create a DataFrame (DF) for the given dataset in Table 1.
df <- data.frame(
    PatientID = c(1, 2, 3, 4),
    AdmDate = as.Date(c("10/15/2009", "11/01/2009", "10/21/2009", "10/28/2009"), "%m/%d/%Y"),
    Age = c(25, 34, 28, 52),
    Diabetes = c("Type1", "Type2", "Type1", "Type1"),
    Status = c("Poor", "Improved", "Excellent", "Poor")
)
df
str(df)



# Q2. Perform the following operation on DF.

#       a. Extract PatientID and Age in Subset 1.
subset1 <- df[, c("PatientID", "Age")]
subset1

#       b. Identify the Type1 patients from DF.
type1_patients <- df[df$Diabetes == "Type1", ][["PatientID"]]
print(paste("Patients with Type 1 diabetes have the ID(s)", paste(type1_patients, collapse=", ")))

#       c. Count the patient of Poor status.
n_poor <- sum(df$Status == "Poor")
print(paste("No. of poor patients =", n_poor))

#       d. Print the summary of the DF.
summary(df[, 2:ncol(df)])

#       e. Find the average age of patient having Diabetes.
print(paste("On an average, a diabetes patient is", mean(df$Age), "years old."))

#       f. Input more patient data from Keyboard.

# Helper functions
get.int <- function(prompt) {
    n <- readline(prompt)
    if (suppressWarnings(is.na(as.numeric(n)))) {
        stop("Input must be numeric.")
    } else {
        n <- as.integer(n)
        if (n <= 0) {
            stop("Input must be a natural number.")
        }
    }
    return(n)
}
get.date <- function(prompt, format) {
    date <- as.Date(readline(prompt), format)
    if (is.na(date)) {
        stop("Invalid date format.")
    }
    return(date)
}

# Main code
n <- get.int("Enter no. of patient records to add: ")
for (i in 1:n) {
    id <- get.int(paste("Enter patient", i, "id: "))
    adm_date <- get.date(paste("Enter patient", i, "admission date in mm/dd/yyyy format: "), "%m/%d/%Y")
    age <- get.int(paste("Enter patient", i, "age: "))
    diab_type <- get.int(paste("What type of diabetes does patient", i, "have? (1/2): Type"))
    if (!(diab_type %in% c(1, 2))) {
        stop("Enter a valid integer diabetes type.")
    }
    status <- readline(paste("How is patient", i, "doing now? "))

    new_row_df <- data.frame(
        PatientID = id,
        AdmDate = adm_date,
        Age = age,
        Diabetes = paste("Type", diab_type, sep=""),
        Status = status
    )
    df <- rbind(df, new_row_df)
}
df



# Q3.   Create a list named MyList having title “My First List” and criteria having following items:
#           a. Age vector a = (12, 14,16, 20)
#           b. A two dimensional matrix with 5 rows.
#           c. A score vector with values s = (‘First’, ‘Second’, ‘Third’)
#       Print MyList, criteria, and vector a.

a <- c(12, 14, 16, 20)
m <- matrix(c(1:10), nrow = 5)
s <- c("First", "Second", "Third")

MyList <- list(
    Title = "My First List",
    Criteria = list(
        Age = a,
        Matrix = m,
        Score = s
    )
)

print("MyList:-")
MyList
print("Criteria:-")
MyList$Criteria
print("Vector 'a' from MyList:-")
MyList$Criteria$Age
