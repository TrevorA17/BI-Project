#Load dataset
TitanicData <-read.csv("data/titanic.csv", colClasses = c(
  PassengerId = "numeric",
  Survived = "factor",
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
survived_levels <- c("0", "1")

# Update factor columns with levels
TitanicData$Embarked <- factor(TitanicData$Embarked, levels = embarked_levels)
TitanicData$Sex <- factor(TitanicData$Sex, levels = sex_levels)
TitanicData$Survived <- factor(TitanicData$Survived, levels = survived_levels)

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


# Load required libraries
library(caret)

# Assuming 'TitanicData' is your dataset and 'train_control' is your control object

# Train Support Vector Machine (SVM) model
svm_model <- train(Survived ~ ., data = TitanicData, method = "svmRadial", trControl = train_control)

# Train k-Nearest Neighbors (kNN) model
knn_model <- train(Survived ~ ., data = TitanicData, method = "knn", trControl = train_control)

# Train Gradient Boosting Machine (GBM) model
gbm_model <- train(Survived ~ ., data = TitanicData, method = "gbm", trControl = train_control)

# Train Neural Network model
nnet_model <- train(Survived ~ ., data = TitanicData, method = "nnet", trControl = train_control)

# Train Decision Tree model
tree_model <- train(Survived ~ ., data = TitanicData, method = "rpart", trControl = train_control)

# Train Naive Bayes model
nb_model <- train(Survived ~ ., data = TitanicData, method = "nb", trControl = train_control)

# Create a list of models for comparison
models <- list(
  glm_model = glm_model,
  rf_model = rf_model,
  svm_model = svm_model,
  knn_model = knn_model,
  gbm_model = gbm_model,
  nnet_model = nnet_model,
  tree_model = tree_model,
  nb_model = nb_model
)

# Compare models using resamples
resamples <- resamples(models)

# Print summary of performance metrics
summary(resamples)



