#' ---
#' title: "Assignment 0 "
#' author: "Erik Aune"
#' date: "2/1/2025"
#' ---


# Install the packages needed for this code file
# install.packages(c("stats","factoextra","gridExtra","cluster"))

library(tidyverse)
library(dplyr)
library(ggplot2)
#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
# Load the dataset

getwd()
setwd("C:/Users/E-man/OneDrive/Desktop/ABA_R_Files")


# Load the dataset
netflix <- read.csv("netflix_titles.csv")


#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------

#Inspect the dataset
summary(netflix)

# Check for missing values
colSums(is.na(netflix))

# Check the structure of the dataset 
str(netflix)

# (number of rows and columns)
cat("The dataset has", nrow(netflix), "rows and", ncol(netflix), "columns.\n")

# Column names
cat("Column names are:", paste(colnames(netflix), collapse = ", "), "\n")

#----------------------------------------------------------------
#' #3. Graphing Data 
#----------------------------------------------------------------
# Calculate the count of tv shows and movies
type_count <- netflix %>% 
  group_by (type) %>% 
  summarise(count = n())

# graph the counts

ggplot(type_count, aes(x = type, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Movies vs TV Shows on Netflix",
       x = "Type",
       y = "Count") +
  theme_minimal()



#----------------------------------------------------------------
#' #4. Summary statistics by release year 
#----------------------------------------------------------------


# Calculate statistics
stats <- netflix %>%
  summarise(
    Mean = mean(release_year, na.rm = TRUE),
    Variance = var(release_year, na.rm = TRUE),
    Max = max(release_year, na.rm = TRUE),
    Standard_Deviation = sd(release_year, na.rm = TRUE)
  )

# Print the statistics
print(stats)

# Summary statistics
summary_stats <- summary(netflix$release_year)
print(summary_stats)

# Count occurrences of each release year for the bar plot
year_counts <- netflix %>%
  group_by(release_year) %>%
  summarise(count = n()) %>%
  ungroup()

#print count by year
print(year_counts, n=74)

# Create the bar plot
ggplot(year_counts, aes(x = release_year, y = count, fill = as.factor(release_year))) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Titles by Release Year",
       x = "Release Year",
       y = "Count") +
  theme_minimal()







