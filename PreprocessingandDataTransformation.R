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

