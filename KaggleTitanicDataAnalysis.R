library(forecast)
plot(wineind)
sm <- ma(wineind,order=12)
lines(sm,col="red")

View(flights14)
install.packages("data.table")
install.packages("xlsx")

# Introduction to Data Science with R - _Data Analysis Part 1.mp4

# Kaggle Titanic Data Analysis...

# This is a classification problem. Skill applicable to things like: Fraud Detection, Customer Segmentation.

# "Data Analysis" and "Feature Engineering".

# Data From:
# https://www.kaggle.com/c/titanic/data
# train.csv and test.csv

# What is my current working directory? Run this function:
# getwd()

# Make this the folder, with the correct versions of the csv files (No Passenger ID), the Home directory, from: Session, Set Working Directory, Choose Directory...
# C:\Users\Hon\Documents\R\Kaggle Titanic Data


# Alternatively, using code, set working directory using:
setwd("C:/Users/Hon/Documents/R/Kaggle Titanic Data") # Acer Travelmate
# setwd("C:/Users/Hontan/Documents/R/Kaggle Titanic Data") # PT HP
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


# Cambridge Analytica
# +===================================================+
# https://cambridgeanalytica.org/
# Cambridge Analytica uses data to change audience behavior. Visit our Commercial or Political divisions to see how we can help you.
# Questions have been raised about the role of Cambridge Analytica, a data mining and analysis company, in both the US election and the EU referendum.
# +===================================================+


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
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Take a look at the ticket variable
str(data.combined$Ticket)

# Based on the huge number of levels, Ticket really isn't a factor (dropdown list) variable, it's a string.
# So, convert it, and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
str(data.combined$Ticket)
data.combined$Ticket[1:20]

# There is no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each.
Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)

# We can make a factor for analysis purposes and visualize
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

# View it
View(data.combined)

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap("Pclass") +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of Pclass and Title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

# Occam's razor is a principle from philosophy. Suppose there exist two explanations for an occurrence. In this case the simpler one is usually better.
# Logistic regression as a classification algorithm is preferred over deep neural network.

# Next, the fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))
str(data.combined$Fare)

# Can't make fare a factor, too many variations, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_bar(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)
# Error message means, tossed out one value because there was one NA. See from the summary: summary(data.combined$Fare)

# Let's check to see if far has predictive power...
# ideo 3, 25:35.
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")


# Analysis of the cabin variable
str(data.combined$Cabin)
## 187 levels, so probably not good as a factor.

# Cabin rally isn't a factor, make a string and display first 100.
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U" for "Unknown".
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take a look at just the fist char of a factor
# If there is any signal in this variable, it's going to most likely be denoted in the decks, the first character.
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)

# Add to combined data set and plot
data.combined$Cabin.first.char <- Cabin.first.char
# See what this looks like, a variable, not in the data.combined dataset.
View(data.combined$Cabin.first.char)

# Hight level plot
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Cound hace some predictive power, drill in...
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")
# As Scientists, we don't want to just be going with out gut, we want data to be driving what we are doing.

# Does this feature improve upon Pclass + title?
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Cabin.Multiple") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(fill = "Survived")

# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)

# Plat data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, full = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

## Video 4: Exploratory Modelling

# Exploratory modelling.
# Logistic Regression. Fast, but linear model, so maybe not effective.
# Random Forests are effective.
# Deep neural networks and booster tree (e.g. xgboost) ecplipse Random Forest.

# Search Youtube: Random Forest Kaggle
# For University of British Columia: Nando De Freitas, 4 hour lecture.

# Install plain old randomForest package
install.packages("randomForest")

# Load randomForest
library(randomForest)

# Train a Random Forest with the default parameters using Pclass & Title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
# Title is more to the right and therefore more predictie that Pclass.

# Leo Breiman created Random Forest

# Train a Random Forest using Pclass, title, SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a Random Forest using Pclass, title, & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train a Random Forest using Pclass, title, & Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a Random Forest using Pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using Pclass, title, SibSp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Train a Random Forest using Pclass, title, Parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# Feature engineering, understanding your data is the number one thing.
# Over-fitting is the bane of applied "Predictive Modelling"/"Machine Learning" in Data Science.

# Before we jump into features engineering we need to establish a methodology
# for estimating our error rate on the test set (i.e. unseen data). This is
# critical, because without this we are more likely to overfit. Let's start with a
# submission of rf.5 to Kaggle to see if our OOB error estimate is accurate.

# subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20171019_1.csv", row.names = FALSE)

# Submit file to Kaggle at this point. Not shown in video.

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates

# YouTube search: Cross Validation
# YouTube search: Jeff Leek, Pick "Jeff Leek", Click: "Corsera Data Analysis". Start with "PredictionStudyDesign"(17:50, Study Design, Holdout Sets), then do "CrossValidation"(all)

# caret stands for: Classification and Regression Training.
# Recommended book: "Applied Predictive Modeling" by Max Kuhn & Kjell Johnson.
# Website: caret.r-forge.r-project.org which goes to http://topepo.github.io/caret/index.html

install.packages("caret")
library(caret)
install.packages("doSNOW")
library(doSNOW)

# Research has shown that 10-fold CV (Cross Validation) repeated 10 time is the best place to start,
# however there are no hard and fast rules - this is where the experience of the
# Data Scientist (i.e., the "art) comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.

# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cros validation and generally provides better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rd.label)
342 / 549

table(rf.label[cv.10,folds[[33]]])
308 /494

# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

## Paused here: Video 5, "Cross Validation", 26:22.

