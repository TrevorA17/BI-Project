# Load required libraries
library(plumber)
library(randomForest)

# Load the saved RandomForest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Define the Plumber API
#* @apiTitle Titanic Survival Prediction Model API
#* @apiDescription Used to predict whether a passenger survived on the Titanic.

#* @param Pclass Passenger class (1st, 2nd, or 3rd)
#* @param Sex Sex of the passenger
#* @param Age Age of the passenger
#* @param SibSp Number of siblings/spouses aboard
#* @param Parch Number of parents/children aboard
#* @param Fare Passenger fare
#* @param Embarked Port of embarkation (C = Cherbourg, Q = Queenstown, S = Southampton)

#* @get /titanic_survival

predict_titanic_survival <- function(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    Age = as.numeric(Age),
    SibSp = as.numeric(SibSp),
    Parch = as.numeric(Parch),
    Fare = as.numeric(Fare),
    Embarked = as.factor(Embarked)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}

