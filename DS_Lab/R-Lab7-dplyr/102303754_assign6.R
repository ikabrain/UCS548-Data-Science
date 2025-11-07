# UCS548 Lab Assignment 6

library(dplyr)
set.seed(42)



# Q1. Generating dataset with 20 different years for each country
# Simple dataset creation using only base R

# Define countries and their continents
Country <- c(
    "India", "China", "Japan", "Indonesia",           # Asia
    "Germany", "France", "Italy", "Spain",            # Europe
    "Nigeria", "Egypt", "South Africa", "Kenya",      # Africa
    "USA", "Canada", "Brazil", "Mexico",              # North America
    "Argentina", "Chile", "Peru", "Colombia"          # South America
)

Continent <- c(
    rep("Asia", 4),
    rep("Europe", 4),
    rep("Africa", 4),
    rep("North America", 4),
    rep("South America", 4)
)

# 20 years of data (2000–2019)
Year <- rep(2000:2019, each = length(Country))

# Repeat country/continent lists for each year
Country <- rep(Country, times = 20)
Continent <- rep(Continent, times = 20)

# Generate life expectancy, population, GDP per capita
LifeExp <- round(
    ifelse(
        Continent == "Asia",
        65 + (Year - 2000) * 0.3,
        ifelse(
            Continent == "Europe",
            75 + (Year - 2000) * 0.2,
            ifelse(
                Continent == "Africa",
                55 + (Year - 2000) * 0.4,
                ifelse(
                    Continent == "North America",
                    70 + (Year - 2000) * 0.25,
                    68 + (Year - 2000) * 0.22
                )
            )
        )
    ) + rnorm(length(Year), 0, 1),
    1
)

Pop <- round(
    ifelse(
        Continent == "Asia",
        runif(length(Year), 50, 1400) * 1e6,
        ifelse(
            Continent == "Europe",
            runif(length(Year), 10, 200) * 1e6,
            ifelse(
                Continent == "Africa",
                runif(length(Year), 5, 250) * 1e6,
                ifelse(
                    Continent == "North America",
                    runif(length(Year), 30, 400) * 1e6,
                    runif(length(Year), 20, 300) * 1e6
                )
            )
        )
    )
)

gdpPerc <- round(LifeExp * runif(length(Year), 900, 1600) + rnorm(length(Year), 0, 200), 2)

# Combine into one dataset
data <- data.frame(Country, Continent, Year, LifeExp, Pop, gdpPerc)
head(data)


# 1. How many unique countries are represented per continent?
data %>% group_by(Continent) %>% summarise(UniqueCountries = n_distinct(Country))


# 2. Which European nation had the lowest GDP per capita in a given year?
year <- 2007
min_gdp <- data %>%
    filter(Continent == "Europe", Year == year) %>%
    slice_min(gdpPerc, n = 1)
print(paste(min_gdp$Country, "had the lowest GDP per capita in Europe in", year, "with GDP =", min_gdp$gdpPerc))


# 3. According to the data available, what was the average life expectancy across each continent in a given year?
year <- 2007
data %>%
    filter(Year == year) %>%
    group_by(Continent) %>%
    summarise(avg_life_expectancy = mean(LifeExp))


# 4. What 5 countries have the highest total GDP over all years combined?
max_total_gdp_countries <- data %>%
    group_by(Country) %>%
    summarise(total_gdp = sum(gdpPerc)) %>%
    slice_max(total_gdp, n = 5) %>%
    .$Country                   # magrittr placeholder `.` used for extraction!
print(paste(
    paste(max_total_gdp_countries, collapse = ", "),
    "have the highest total GDP over all years"
))


# 5. What countries and years had life expectancies of at least 80 years?
data %>% filter(LifeExp >= 80) %>% select(Country, Year)


# 6. What 10 countries have the strongest correlation (in either direction) between life expectancy and per capita GDP?
data %>%
    group_by(Country) %>%
    summarise(corr = cor(LifeExp, gdpPerc)) %>%
    arrange(desc(abs(corr))) %>%
    slice_head(n = 10)                          # Can also directly use head(10)!


# 7. Which combinations of continent (besides Asia) and year have the highest average population across all countries?
data %>%
    filter(Continent != "Asia") %>%
    group_by(Continent, Year) %>%
    summarise(avg_pop = mean(Pop)) %>%
    arrange(desc(avg_pop))


# 8. Which three countries have had the most consistent population estimates (i.e. lowest standard deviation) across the years of available data?
data %>%
    group_by(Country) %>%
    summarise(pop_sd = sd(Pop)) %>%
    slice_min(pop_sd, n = 3)

# 9. Excluding records from a given year, which observations indicate that the population of a country has decreased from the previous year and the life expectancy has increased?
# Here, I'll use `'lag()` - dplyr function that accesses previous row's value!
year <- 2007
data %>%
    arrange(Country, Year) %>%
    group_by(Country) %>% # If no grouping, filter will compare BETWEEN countries too
    filter(Year != year, Pop < lag(Pop), LifeExp > lag(LifeExp))



# Q2. Use R to answer the following questions:

# 1. Create a database file “DataSet.csv” that contains 10 records of medicine with attributes:-
# MedID, Med_Name, Company, Manf_year, Exp_date, Quantity_in_stock, Sales
MedID <- 1:10
Med_Name <- c("Paracetol", "Coughnil", "GlucoFast", "PainAway", "HealMax",
              "Allerclear", "Coughnil Plus", "GlucoFast XR", "VitaCure",
              "PainAway Forte")
Company <- c("MediHealth", "PharmaLife", "WellnessCo", "MediHealth",
             "HealthGen", "PharmaLife", "PharmaLife", "WellnessCo",
             "NutriLabs", "MediHealth")
Manf_year <- c(2018, 2019, 2020, 2019, 2021, 2018, 2022, 2020, 2023, 2019)
Exp_date <- as.Date(c("2025-06-30", "2024-11-15", "2026-03-01", "2023-10-12",
                      "2025-09-05", "2024-02-20", "2025-12-10", "2026-09-18",
                      "2026-01-01", "2023-05-25"))
Quantity_in_stock <- c(500, 300, 450, 150, 700, 200, 350, 400, 250, 100)
Sales <- c(12000, 8500, 15000, 4000, 18000, 6000, 10000, 14500, 9000, 5000)
medicine_data <- data.frame(
    MedID,
    Med_Name,
    Company,
    Manf_year,
    Exp_date,
    Quantity_in_stock,
    Sales
)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv(medicine_data, "DataSet.csv", row.names = FALSE)

# 2. Read the data file and show the first 4 record of the file
medicine_data = read.csv("DataSet.csv")
head(medicine_data, 4)

# 3. Read the data file and show the last 4 record of the file
tail(medicine_data, 4)

# 4. Find the correlation between Quantity_in_stock and Exp_date
Exp_secs <- as.numeric(as.Date(Exp_date))
correl <- cor(medicine_data$Quantity_in_stock, Exp_secs)
print(paste("Correlation between Quantity_in_stock and Exp_date (in seconds) =", round(correl, 4)))

# 5. Plot the bar graph for the Sales with year of manufacturing
# `aggregate()` is a base R function that helps bring together groups of data!
year_sales <- aggregate(Sales ~ Manf_year, data = medicine_data, sum)
barplot(
    height = year_sales$Sales,
    names.arg = year_sales$Manf_year,
    xlab = "Manufacturing Year",
    ylab = "Total Sales",
    main = "Sales vs Manufacturing Year",
    col = "skyblue",
    border = "blue"
)

# 6. Find the company(s) having more than one type of medicine
medicine_data %>%
    group_by(Company) %>%
    summarise(distinct_meds = n_distinct(Med_Name)) %>%
    filter(distinct_meds > 1)

# 7. Find the types of Medicines available
unique(medicine_data$Med_Name)

# 8. Which medicines are expiring? Show by box plots.
expiring_meds <- filter(medicine_data, Exp_date < as.Date("2025-01-01"))
boxplot(
    Quantity_in_stock,
    data = expiring_meds,
    col = "lightblue",
    main = "Stock Distribution for Expiring Medicines (Before 2025)",
    xlab = "Expiring Meds",
    ylab = "Stock"
)

# 9. Find the average stock in the store
medicine_data %>% summarise(Average_Stock = mean(Quantity_in_stock))

# 10. Draw the regression line between Manufacturing year and Sales
plot(
    x = medicine_data$Manf_year,
    y = medicine_data$Sales,
    main = "Regression Line: Sales vs Manufacturing Year",
    xlab = "Manufacturing Year",
    ylab = "Sales",
    pch = 19,
    col = "blue"
)
model <- lm(Sales ~ Manf_year, data = medicine_data)
abline(model, col = "red", lwd = 2)
