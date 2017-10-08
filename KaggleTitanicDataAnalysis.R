library(forecast)
plot(wineind)
sm <- ma(wineind,order=12)
lines(sm,col="red")

View(flights14)
install.packages("data.table")
install.packages("xlsx")

# Introduction to Data Science with R - _Data Analysis Part 1.mp4

# Kaggle Titanic Data Analysis...

# Data From:
# https://www.kaggle.com/c/titanic/data
# train.csv and test.csv

# What is my current working directory? Run this function:
# getwd()

# Make this the folder, with the correct versions of the csv files (No Passenger ID), the Home directory, from: Session, Set Working Directory, Choose Directory...
# C:\Users\Hon\Documents\R\Kaggle Titanic Data


# Alternatively, using code, set working directory using:
setwd("C:/Users/Hon/Documents/R/Kaggle Titanic Data") # Acer Travelmate
# setwd("C:/Users/Hontan/Documents/R/Kaggle Titanic Data") # Work HP computer
# setwd("E:/Hon 2017-01-01 FULL/Hon R/Kaggle Titanic Data") # Gigabyte


# Load row data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# To remove data sets, us the rm() function
# rm(data_1, data_2, data_3)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
#test[1,4]

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

# Change some data types
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Take a look at gross survival rates
table(data.combined$Survived)


# Distibution across classes
table(data.combined$Pclass)

# From Packages install ggplot2 and stringr
# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
str(train) #train$Pclass is still an int, need to change this to a factor.
train$Pclass <- as.factor(train$Pclass)
# Run using geom_bar() not geom_histogram
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Examine the first few names in the training data set
head(as.character(train$Name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))
# Returns 1307, would have expected 1309.

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
# The which() function is like the Where clause in SQL. 

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]
# In SQL, this is like saying in data.combined, grab me all the records Where Name in dup.names


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)
# This tells me stringr() was built R version 3.3.3

# What version of R am I running
# Type this into the Console: > Version
# This tells me I using R version 3.3.1 (2016-06-21)
# After installing a newer version of R, 3.4.1, and restarting RStudio, went through above code, and stringr() doesn't give this warning anymore.

# What version of RStudio am I running? Type this into the console window:
# RStudio.Version()
# From this, I find that I am currently running RStudio Desktop version 1.0.143
# On the RStudio website, I find that the current version is 1.0.153, so I close RStudio, and install the latest version.

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")), ]
# First five rows, all the columns:
misses[1:5, ]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5, ]

# Check out makes to see if pattern continues
males <- data.combined[which(train$Sex == "male"), ]
males[1:5, ]


# Expand upon the relationship between 'Survived' and 'Pclass' by adding the new 'Title' variable to the
# data set and then exploring a potential 3-dimensional relationship.
# Create a functions that brings out the title.
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

# Loop
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

# Add this to data.combined
data.combined$title <- as.factor(titles)

# Let's do a 3d visualization on this.
# Since we only have survived lables for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# David Langer's github
# https://github.com/EasyD/IntroToDataScience


# Hadley Wickham - Chief Scientist at RStudio.


# Install the tidyverse, which includes a set of packages that work in harmony
# Includes: broom, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
install.packages("tidyverse")
# Takes a while. Finished when you get this message:
# The downloaded binary packages are in
# C:\Users\Hon\AppData\Local\Temp\Rtmp8kkH30\downloaded_packages


# Most businesses don't care about beautiful optimal code. They care more about cycle time. How quickly can you get something to work.
# Knowing your data is the best way to let it speak to you, and find what is predictive and what is not.
# Feature engineering trumps algorithms.


# Introduction to Data Science with R - _Data Analysis Part 2.mp4

# Deduced that:
# - Pclass is a proxy for socio-economic status.
# - Title can be used a proxy for age.
# - Males in the 3rd class were most likely to perish, unless your title was Master.

# What's the distibution of females to males across train * test?
table(data.combined$Sex)

# Visualize the 3-way relationship of sex, Pclass, and survival, compare to analysis of title.
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Take a look at age.
summary(data.combined$Age)
# Imputation, use predictive analysis to fill missing vlaues.
# Better to use proxy for Age when it is missing, e.g. Title.
# Just summary on the train data...
summary(data.combined[1:891,"Age"])

# See visual plot of this.
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)
# Can just run the which() bit:
which(data.combined$title == "Master.")

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar(width = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# OK, appears female children may have different survival rate,
# Could be a candidate for feature engineering later.
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone)
length(which(misses.alone$Age <= 14.5))

# Moving on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)


#####################################################

# Find data contains...
subset(data.combined, Name == "McCarthy, Mr. Timothy J")
subset(data.combined[which(data.combined$Name == "Timothy"),])
grep("Allen", data.combined$Name, value = TRUE)
NamedAllen <- data.combined[which(data.combined$Name == grep("Allen", data.combined$Name, value = TRUE)),]

# Select * Into [NamedAllen] From [data.combined] Where Name Like '%Allen%'
NamedAllen <- data.combined[grep("Allen", data.combined$Name), ]
SpecificNamedAllen <- data.combined[which(data.combined$Name == 'Allen, Miss. Elisabeth Walton'), ]

#####################################################


# Can we treat sibsp as a factor (i.e. a dropdown list)
# Does it have a small enough range of defined values.
# length is linke count here
length(unique(data.combined$SibSp))

# Do transformation of sibsp from integer variable to factor variable.
data.combined$SibSp <- as.factor(data.combined$SibSp)

# Check
str(data.combined)

# SibSp as a factor allows us to do more with it in a ggplot bar graph.
# We believe title is predictive. Visualize survival rates by SipSp, Pclass, and [something else]
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the parch variable as a factor and visualize.
data.combined$Parch <- as.factor(data.combined$Parch)
str(data.combined) # check
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

## Let's try some feature engineering. what about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp) #grabbing these again, becuase I want their integer variable.
temp.parch <- c(train$Parch, test$Parch) #grabbing these again, becuase I want their integer variable.
data.combined$fammily.size <- as.factor(temp.sibsp + temp.parch + 1)

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = fammily.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

## Completed Video 2.

