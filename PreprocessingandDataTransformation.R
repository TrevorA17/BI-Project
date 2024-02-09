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

# Check for missing values in the dataset
missing_values <- sapply(TitanicData, function(x) sum(is.na(x)))

# Display columns with missing values and their counts
missing_values_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)
missing_values_df <- missing_values_df[missing_values_df$Missing_Values > 0, , drop = FALSE]

# Print the result
print(missing_values_df)

# Install necessary package
install.packages("mice")

# Load the mice package
library(mice)
# Impute missing values in numeric columns using mean imputation
numeric_cols <- sapply(TitanicData, is.numeric)
imputed_data_numeric <- complete(mice(TitanicData[, numeric_cols]))

# Impute missing values in categorical columns using mode imputation
categorical_cols <- sapply(TitanicData, is.factor)
imputed_data_categorical <- complete(mice(TitanicData[, categorical_cols]))

# Combine the imputed numeric and categorical datasets
imputed_data <- cbind(imputed_data_numeric, imputed_data_categorical)

# Check if there are still missing values in the imputed dataset
missing_values_after_imputation <- any(is.na(imputed_data))

# Display the result
if (missing_values_after_imputation) {
  cat("There are still missing values after imputation.\n")
} else {
  cat("All missing values have been successfully imputed.\n")
}

# Replace TitanicData dataset with the imputed dataset
TitanicData <- imputed_data

View(TitanicData)

#Feature Scaling
# Scaling numeric variables
TitanicData$ScaledAge <- scale(TitanicData$Age)
TitanicData$ScaledFare <- scale(TitanicData$Fare)

# Creating FamilySize variable
TitanicData$FamilySize <- TitanicData$SibSp + TitanicData$Parch + 1

