# UCS548 Lab Assignment 5 - dplyr package

install.packages("dplyr")
library(dplyr)



# Q1. Read the “daily_show_guests.csv” file from GitHub in the R and print the first 10 records
#     (Context: https://fivethirtyeight.com/features/every-guest-jon-stewart-ever-had-on-the-daily-show/)
data.csv <- read.csv("./daily_show_guests.csv")
head(data.csv, 10)



# Q2. Display the “column names” of the table and rename the columns as:-
# year: YEAR; job: GoogleKnowlege_Occupation; date: Show; category: Group; guest_name: Raw_Guest_List

colnames(data.csv)
data <- rename(
    data.csv,
    year = YEAR,
    job = GoogleKnowlege_Occupation,
    date = Show,
    category = Group,
    guest_name = Raw_Guest_List
)
colnames(data)



# Q3. Create a report having year, date, and guest_name
data %>% select(year, date, guest_name) %>% summarise_all(n_distinct)



# Q4. Use “select” (dplyr) function to print all the record except “year”
select(data, -year)



# Q5. Extract the list of peoples who are “actor” only, name in “ABC”
data %>%
    filter(tolower(job) == "actor") %>%
    select(guest_name) %>%
    arrange(guest_name)



# Q6. Arrange the records in-order of date
data <- arrange(data, year, date)
head(data)



# Q7. Add one column to the database with the name “Experience”
#     (Help: mutate() function in dplyr)

data <- mutate(data, Experience = 2025 - year)# calculate years since guest appeared
head(data)
