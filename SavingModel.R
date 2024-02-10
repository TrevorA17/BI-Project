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

# New data for prediction
new_data <- data.frame(
  # Include your variables here based on the model features
)

# Use the loaded Random Forest model to make predictions
predictions_loaded_rf_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_rf_model)
