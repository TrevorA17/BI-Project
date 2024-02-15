Titanic Survival Prediction Model
================
Trevor Okinda

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Understanding the Dataset (Exploratory Data Analysis
  (EDA))](#understanding-the-dataset-exploratory-data-analysis-eda)
  - [Loading the Dataset](#loading-the-dataset)
    - [Source:](#source)
    - [Reference:](#reference)

# Student Details

|                       |                                   |
|-----------------------|-----------------------------------|
| **Student ID Number** | 134780                            |
| **Student Name**      | Trevor Okinda                     |
| **BBIT 4.2 Group**    | C                                 |
| **Project Name**      | Titanic Survival Prediction Model |

# Setup Chunk

**Note:** the following KnitR options have been set as the global
defaults: <BR>
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

# Understanding the Dataset (Exploratory Data Analysis (EDA))

## Loading the Dataset

### Source:

The dataset that was used can be downloaded here: *\<<a
href="https://www.kaggle.com/datasets/rahulsah06/titanic?resource=download\"
class="uri">https://www.kaggle.com/datasets/rahulsah06/titanic?resource=download\</a>\>*

### Reference:

*\<Avagyan, Z. (2017). Weather CSV \[Data set\]. Kaggle.
<a href="https://www.kaggle.com/datasets/zaraavagyan/weathercsv\"
class="uri">https://www.kaggle.com/datasets/zaraavagyan/weathercsv\</a>\>  
Refer to the APA 7th edition manual for rules on how to cite datasets:
<https://apastyle.apa.org/style-grammar-guidelines/references/examples/data-set-references>*

``` r
#Load dataset
TitanicData <-read.csv("titanic.csv", colClasses = c(
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
```

``` r
# Display frequency and percentage of Pclass
table_Pclass <- table(TitanicData$Pclass)
percentage_Pclass <- prop.table(table_Pclass) * 100
class_labels_Pclass <- c("First Class", "Second Class", "Third Class")
cat("Frequency and Percentage of Pclass:\n")
```

    ## Frequency and Percentage of Pclass:

``` r
cat(table_Pclass, "\n")
```

    ## 216 184 491

``` r
cat(paste0("Percentage:\n", round(percentage_Pclass, 2)), "\n")
```

    ## Percentage:
    ## 24.24 Percentage:
    ## 20.65 Percentage:
    ## 55.11

``` r
cat(paste0("Class Labels:\n", class_labels_Pclass, "\n\n"))
```

    ## Class Labels:
    ## First Class
    ## 
    ##  Class Labels:
    ## Second Class
    ## 
    ##  Class Labels:
    ## Third Class

``` r
# Display frequency and percentage of Sex
table_Sex <- table(TitanicData$Sex)
percentage_Sex <- prop.table(table_Sex) * 100
class_labels_Sex <- c("Female", "Male")
cat("Frequency and Percentage of Sex:\n")
```

    ## Frequency and Percentage of Sex:

``` r
cat(table_Sex, "\n")
```

    ## 314 577

``` r
cat(paste0("Percentage:\n", round(percentage_Sex, 2)), "\n")
```

    ## Percentage:
    ## 35.24 Percentage:
    ## 64.76

``` r
cat(paste0("Class Labels:\n", class_labels_Sex, "\n\n"))
```

    ## Class Labels:
    ## Female
    ## 
    ##  Class Labels:
    ## Male

``` r
# Display frequency and percentage of Embarked
table_Embarked <- table(TitanicData$Embarked)
percentage_Embarked <- prop.table(table_Embarked) * 100
class_labels_Embarked <- c("Cherbourg", "Queenstown", "Southampton")
cat("Frequency and Percentage of Embarked:\n")
```

    ## Frequency and Percentage of Embarked:

``` r
cat(table_Embarked, "\n")
```

    ## 168 77 644

``` r
cat(paste0("Percentage:\n", round(percentage_Embarked, 2)), "\n")
```

    ## Percentage:
    ## 18.9 Percentage:
    ## 8.66 Percentage:
    ## 72.44

``` r
cat(paste0("Class Labels:\n", class_labels_Embarked, "\n\n"))
```

    ## Class Labels:
    ## Cherbourg
    ## 
    ##  Class Labels:
    ## Queenstown
    ## 
    ##  Class Labels:
    ## Southampton

``` r
# Calculate mean, median, and mode for Age
mean_Age <- mean(TitanicData$Age, na.rm = TRUE)
median_Age <- median(TitanicData$Age, na.rm = TRUE)
mode_Age <- as.numeric(names(table(TitanicData$Age))[which.max(table(TitanicData$Age))])

cat("Measures of Central Tendency for Age:\n")
```

    ## Measures of Central Tendency for Age:

``` r
cat(paste0("Mean: ", round(mean_Age, 2)), "\n")
```

    ## Mean: 29.7

``` r
cat(paste0("Median: ", median_Age), "\n")
```

    ## Median: 28

``` r
cat(paste0("Mode: ", mode_Age), "\n\n")
```

    ## Mode: 24

``` r
# Calculate mean, median, and mode for Fare
mean_Fare <- mean(TitanicData$Fare, na.rm = TRUE)
median_Fare <- median(TitanicData$Fare, na.rm = TRUE)
mode_Fare <- as.numeric(names(table(TitanicData$Fare))[which.max(table(TitanicData$Fare))])

cat("Measures of Central Tendency for Fare:\n")
```

    ## Measures of Central Tendency for Fare:

``` r
cat(paste0("Mean: ", round(mean_Fare, 2)), "\n")
```

    ## Mean: 32.2

``` r
cat(paste0("Median: ", median_Fare), "\n")
```

    ## Median: 14.4542

``` r
cat(paste0("Mode: ", mode_Fare), "\n\n")
```

    ## Mode: 8.05

``` r
# Calculate range, variance, and standard deviation for Age
range_Age <- range(TitanicData$Age, na.rm = TRUE)
variance_Age <- var(TitanicData$Age, na.rm = TRUE)
sd_Age <- sd(TitanicData$Age, na.rm = TRUE)

cat("Measures of Distribution for Age:\n")
```

    ## Measures of Distribution for Age:

``` r
cat(paste0("Range: ", paste(range_Age, collapse = " - ")), "\n")
```

    ## Range: 0.42 - 80

``` r
cat(paste0("Variance: ", round(variance_Age, 2)), "\n")
```

    ## Variance: 211.02

``` r
cat(paste0("Standard Deviation: ", round(sd_Age, 2)), "\n\n")
```

    ## Standard Deviation: 14.53

``` r
# Calculate range, variance, and standard deviation for Fare
range_Fare <- range(TitanicData$Fare, na.rm = TRUE)
variance_Fare <- var(TitanicData$Fare, na.rm = TRUE)
sd_Fare <- sd(TitanicData$Fare, na.rm = TRUE)

cat("Measures of Distribution for Fare:\n")
```

    ## Measures of Distribution for Fare:

``` r
cat(paste0("Range: ", paste(range_Fare, collapse = " - ")), "\n")
```

    ## Range: 0 - 512.3292

``` r
cat(paste0("Variance: ", round(variance_Fare, 2)), "\n")
```

    ## Variance: 2469.44

``` r
cat(paste0("Standard Deviation: ", round(sd_Fare, 2)), "\n\n")
```

    ## Standard Deviation: 49.69

``` r
# Calculate correlation between Age and Fare
correlation_Age_Fare <- cor(TitanicData$Age, TitanicData$Fare, use = "complete.obs")

cat("Measures of Relationship (Correlation) between Age and Fare:\n")
```

    ## Measures of Relationship (Correlation) between Age and Fare:

``` r
cat(paste0("Correlation Coefficient: ", round(correlation_Age_Fare, 2)), "\n\n")
```

    ## Correlation Coefficient: 0.1

``` r
# Perform ANOVA for Fare among different Passenger Classes
anova_result <- aov(Fare ~ Pclass, data = TitanicData)

# Display ANOVA summary
summary(anova_result)
```

    ##              Df  Sum Sq Mean Sq F value Pr(>F)    
    ## Pclass        1  663625  663625   384.5 <2e-16 ***
    ## Residuals   889 1534174    1726                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Univariate Plots

library(ggplot2)

# Univariate Histogram for Numerical Variable (e.g., Age)
ggplot(TitanicData, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Basic%20Visualization-1.png)<!-- -->

``` r
# Univariate Bar Plot for Categorical Variable (e.g., Pclass)
ggplot(TitanicData, aes(x = factor(Pclass))) +
  geom_bar(fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Bar Plot of Passenger Class", x = "Passenger Class", y = "Count") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Basic%20Visualization-2.png)<!-- -->

``` r
# Univariate Box Plot for Numerical Variable (e.g., Fare)
ggplot(TitanicData, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Box Plot of Fare by Passenger Class", x = "Passenger Class", y = "Fare") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Basic%20Visualization-3.png)<!-- -->

``` r
# Univariate Bar Plot for Categorical Variable (e.g., Sex)
ggplot(TitanicData, aes(x = factor(Sex))) +
  geom_bar(fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Basic%20Visualization-4.png)<!-- -->

``` r
#Multivariate plots
# Multivariate Scatter Plot for Numerical Variables (e.g., Age and Fare)
ggplot(TitanicData, aes(x = Age, y = Fare, color = factor(Pclass))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot of Age and Fare by Passenger Class",
       x = "Age", y = "Fare", color = "Passenger Class") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Multivariate%20Plots-1.png)<!-- -->

``` r
# Multivariate Box Plot for Numerical and Categorical Variables (e.g., Age and Pclass)
ggplot(TitanicData, aes(x = factor(Pclass), y = Age, fill = factor(Pclass))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Box Plot of Age by Passenger Class",
       x = "Passenger Class", y = "Age", fill = "Passenger Class") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Multivariate%20Plots-2.png)<!-- -->

``` r
# Multivariate Bar Plot for Categorical Variables (e.g., Pclass and Sex)
ggplot(TitanicData, aes(x = factor(Pclass), fill = factor(Sex))) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Bar Plot of Passenger Class by Gender",
       x = "Passenger Class", fill = "Gender") +
  theme_minimal()
```

![](TitanicSurvivalPrediction_files/figure-gfm/Multivariate%20Plots-3.png)<!-- -->
