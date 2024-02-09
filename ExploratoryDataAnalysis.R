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

# Display frequency and percentage of Pclass
table_Pclass <- table(TitanicData$Pclass)
percentage_Pclass <- prop.table(table_Pclass) * 100
class_labels_Pclass <- c("First Class", "Second Class", "Third Class")
cat("Frequency and Percentage of Pclass:\n")
cat(table_Pclass, "\n")
cat(paste0("Percentage:\n", round(percentage_Pclass, 2)), "\n")
cat(paste0("Class Labels:\n", class_labels_Pclass, "\n\n"))

# Display frequency and percentage of Sex
table_Sex <- table(TitanicData$Sex)
percentage_Sex <- prop.table(table_Sex) * 100
class_labels_Sex <- c("Female", "Male")
cat("Frequency and Percentage of Sex:\n")
cat(table_Sex, "\n")
cat(paste0("Percentage:\n", round(percentage_Sex, 2)), "\n")
cat(paste0("Class Labels:\n", class_labels_Sex, "\n\n"))

# Display frequency and percentage of Embarked
table_Embarked <- table(TitanicData$Embarked)
percentage_Embarked <- prop.table(table_Embarked) * 100
class_labels_Embarked <- c("Cherbourg", "Queenstown", "Southampton")
cat("Frequency and Percentage of Embarked:\n")
cat(table_Embarked, "\n")
cat(paste0("Percentage:\n", round(percentage_Embarked, 2)), "\n")
cat(paste0("Class Labels:\n", class_labels_Embarked, "\n\n"))

# Calculate mean, median, and mode for Age
mean_Age <- mean(TitanicData$Age, na.rm = TRUE)
median_Age <- median(TitanicData$Age, na.rm = TRUE)
mode_Age <- as.numeric(names(table(TitanicData$Age))[which.max(table(TitanicData$Age))])

cat("Measures of Central Tendency for Age:\n")
cat(paste0("Mean: ", round(mean_Age, 2)), "\n")
cat(paste0("Median: ", median_Age), "\n")
cat(paste0("Mode: ", mode_Age), "\n\n")

# Calculate mean, median, and mode for Fare
mean_Fare <- mean(TitanicData$Fare, na.rm = TRUE)
median_Fare <- median(TitanicData$Fare, na.rm = TRUE)
mode_Fare <- as.numeric(names(table(TitanicData$Fare))[which.max(table(TitanicData$Fare))])

cat("Measures of Central Tendency for Fare:\n")
cat(paste0("Mean: ", round(mean_Fare, 2)), "\n")
cat(paste0("Median: ", median_Fare), "\n")
cat(paste0("Mode: ", mode_Fare), "\n\n")

# Calculate range, variance, and standard deviation for Age
range_Age <- range(TitanicData$Age, na.rm = TRUE)
variance_Age <- var(TitanicData$Age, na.rm = TRUE)
sd_Age <- sd(TitanicData$Age, na.rm = TRUE)

cat("Measures of Distribution for Age:\n")
cat(paste0("Range: ", paste(range_Age, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_Age, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_Age, 2)), "\n\n")

# Calculate range, variance, and standard deviation for Fare
range_Fare <- range(TitanicData$Fare, na.rm = TRUE)
variance_Fare <- var(TitanicData$Fare, na.rm = TRUE)
sd_Fare <- sd(TitanicData$Fare, na.rm = TRUE)

cat("Measures of Distribution for Fare:\n")
cat(paste0("Range: ", paste(range_Fare, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_Fare, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_Fare, 2)), "\n\n")
