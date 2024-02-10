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

# In this example, we'll use the proportion of passengers who survived
compute_statistic <- function(data, indices) {
  subset_data <- data[indices, ]
  mean(subset_data$Survived == 1)
}

# Set the seed for reproducibility
set.seed(123)

# Perform bootstrapping with 1000 replicates
bootstrap_results <- boot(data = TitanicData, statistic = compute_statistic, R = 1000)

# Display the bootstrap results
print(bootstrap_results)

# Plot the bootstrap distribution (histogram)
hist(bootstrap_results$t, main = "Bootstrap Distribution of Survival Proportion", xlab = "Proportion Survived")

#Cross-validation
# Install and load the required packages
install.packages("caret")
library(caret)

# Set the seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation (e.g., 10-fold)
num_folds <- 10

# Create a training control object for k-fold cross-validation
train_control <- trainControl(method = "cv", number = num_folds)