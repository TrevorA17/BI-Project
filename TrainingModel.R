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

# Load required libraries
library(caret)
library(glmnet)  # You may need to install this package if not already installed

# Remove rows with missing values in the 'Survived' column
TitanicData <- na.omit(TitanicData)

# Set up the training control
train_control <- trainControl(method = "cv", number = 5)

# Train a logistic regression model
model <- train(Survived ~ ., data = TitanicData, method = "glm", trControl = train_control, family = "binomial")

# Print the model summary
print(model)

# Load required libraries
library(caret)
library(glmnet)  
library(randomForest)  


# Remove rows with missing values in the 'Survived' column
TitanicData <- na.omit(TitanicData)

# Train a generalized linear model
glm_model <- train(Survived ~ ., data = TitanicData, method = "glm", trControl = train_control, family = "binomial")

# Train Random Forest Model
rf_model <- train(
  Survived ~ .,
  data = TitanicData,
  method = "rf",
  trControl = train_control,
  ntree = 100  # Adjust as needed
)

# Make predictions on the test set
glm_predictions <- predict(glm_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate performance for logistic regression model
glm_conf_matrix <- confusionMatrix(glm_predictions, test_data$Survived)
glm_accuracy <- glm_conf_matrix$overall["Accuracy"]

# Evaluate performance for random forest model
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Survived)
rf_accuracy <- rf_conf_matrix$overall["Accuracy"]

# Display results
cat("Logistic Regression Model:\n")
cat(paste("Accuracy: ", glm_accuracy, "\n"))
cat("Confusion Matrix:\n")
print(glm_conf_matrix)

cat("\nRandom Forest Model:\n")
cat(paste("Accuracy: ", rf_accuracy, "\n"))
cat("Confusion Matrix:\n")
print(rf_conf_matrix)
