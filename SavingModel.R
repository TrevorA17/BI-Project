# Train Random Forest Model
rf_model <- train(
  Survived ~ .,
  data = TitanicData,
  method = "rf",
  trControl = train_control,
  ntree = 100  # Adjust as needed
)

# Save the Random Forest model
saveRDS(rf_model, "./models/saved_rf_model.rds")

# Load the saved Random Forest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Arrange variables in the desired order
new_data <- data.frame(
  Pclass = 3,                   # Passenger class (1st, 2nd, or 3rd)
  Age = 25,                     # Age of the passenger
  SibSp = 1,                    # Number of siblings/spouses aboard
  Parch = 0,                    # Number of parents/children aboard
  Fare = 20,                    # Passenger fare
  Survived = 1,                 # Survival indicator
  Sex = "female",               # Sex of the passenger
  Embarked = "S"                # Port of embarkation (C = Cherbourg, Q = Queenstown, S = Southampton)
)

# Use the loaded Random Forest model to make predictions
predictions_loaded_rf_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_rf_model)
