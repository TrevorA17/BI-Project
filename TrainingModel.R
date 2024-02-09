#Load dataset
TitanicData <-read.csv("data/titanic.csv", colClasses = c(
  PassengerId = "numeric",
  Pclass = "numeric",
  Name = "character",
  Sex = "factor",
  Age = "numeric",
  SibSp = "numeric",
  Parch = "numeric",
  Ticket = "character",
  Fare = "numeric",
  Cabin = "character",
  Embarked = "factor"
))

# Define levels for categorical columns
embarked_levels <- c("C", "Q", "S")
sex_levels <- c("female", "male")

# Update factor columns with levels
TitanicData$Embarked <- factor(TitanicData$Embarked, levels = embarked_levels)
TitanicData$Sex <- factor(TitanicData$Sex, levels = sex_levels)

# Display the dataset
View(TitanicData)


# Install and load the required packages
install.packages("caret")
install.packages("e1071")
install.packages("pROC")

library(caret)
library(e1071)
library(pROC)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
split_index <- createDataPartition(TitanicData$Survived, p = 0.8, list = FALSE)
train_data <- TitanicData[split_index, ]
test_data <- TitanicData[-split_index, ]

# Check the dimensions of the training and testing sets
cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Install and load the required packages
install.packages("boot")
library(boot)