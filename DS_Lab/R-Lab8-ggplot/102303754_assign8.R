# UCS548 Lab Assignment 8

install.packages(c("ggplot2", "rstudioapi", "dplyr"))
library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Sets current folder as wd


# Q1. Use the following data set (Project1) to plot the graphs using ggplot2
project1 <- read.csv("ex12_normalized_intensities.csv")
rownames(project1) <- project1[, 1]
project1[, 1] <- NULL

head(project1)
str(project1)
summary(project1)

# 1. Create a simple scatter plot representing gene expression of “sampleB” on the x-axis and “sampleH” on the y-axis.
g <- ggplot(data = project1)
scatter <- g + aes(x = sampleB, y = sampleH) + geom_point()
scatter

# 2. Add a column to the data frame “project1” (call this column “expr_limits”), that will be filled the following way:
#     i) if the expression of a gene is > 13 in both sampleB and sampleH, set to the value in “expr_limits” to “high”
#     ii) if the expression of a gene is < 6 in both sampleB and sampleH, set it to “low”
#     iii) if different, set it to “normal”.

# We will use dplyr + case_when() - R's match-case!
project1 <- project1 %>%
    mutate(
        expr_limits = case_when(
            sampleB > 13 & sampleH > 13 ~ "high",
            sampleB < 6 & sampleH < 6 ~ "low",
            TRUE ~ "normal"
        ),
        expr_limits = factor(expr_limits, levels = c("low", "normal", "high"))
    )
head(project1)

# 3. Color the points of the scatter plot according to the newly created column “expr_limits”. Save that plot in the object “p”.
p <- scatter + geom_point(data = project1, aes(color = expr_limits))
p

# 4. Produce a boxplot of the expression of all samples (i.e. each sample is represented by a box).

# We will achieve this by pivoting table to a longer format
project1.samples <- project1 %>% select(-"expr_limits")
expr_data <- stack(project1.samples)
colnames(expr_data) <- c("expression", "sample")
expr_data$expr_limits <- project1$expr_limits
head(expr_data)

boxplot <- ggplot(data = expr_data) +
    aes(x = sample, y = expression, color = factor(sample)) +
    geom_boxplot() +
    guides(color = "none")
boxplot

# 5. Modify the previous boxplot so as to obtain 3 “sub-boxplots” per sample, each representing the expression of either “low”, “normal” or “high” genes.

boxplot.fill <- boxplot +
    aes(fill = expr_limits) +
    scale_fill_manual(
        values = c(
            low = "#1f77b4", # blue
            normal = "#2ca02c", # green
            high = "#d62728" # red
        )
    )
boxplot.fill

# 6. Produce a bar plot of how many low/normal/high genes are in the column “expr_limits” of “project1”.
barplot <- ggplot(project1) +
    aes(x = expr_limits, fill = expr_limits) +
    geom_bar() +
    scale_fill_manual(
        values = c(
            low = "#1f77b4", # blue
            normal = "#2ca02c", # green
            high = "#d62728" # red
        )
    )
barplot
