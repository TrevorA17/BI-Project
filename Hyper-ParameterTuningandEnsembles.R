# Load required libraries
library(caret)
library(randomForest)

# Assuming 'TitanicData' is your dataset and 'train_control' is your control object

# Split the dataset into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(TitanicData$Survived, p = 0.8, list = FALSE)
train_data <- TitanicData[splitIndex, ]
test_data <- TitanicData[-splitIndex, ]

# Define a random search grid
random_grid <- expand.grid(
  mtry = seq(2, 10, by = 1),   # Example range for mtry
  nodesize = seq(1, 10, by = 1)  # Example range for nodesize
)

# Random Search using train function
random_search <- train(
  Survived ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = random_grid
)

# Display the best parameters from random search
print("Random Search - Best Parameters:")
print(random_search$bestTune)

# Define a grid search grid
grid <- expand.grid(
  mtry = seq(2, 10, by = 1),   # Example range for mtry
  nodesize = seq(1, 10, by = 1)  # Example range for nodesize
)

# Grid Search using train function
grid_search <- train(
  Survived ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = grid
)

# Display the best parameters from grid search
print("\nGrid Search - Best Parameters:")
print(grid_search$bestTune)

# Manual Search using train function
manual_search <- train(
  Survived ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(mtry = 5, nodesize = 5)  # Example manual parameters
)

# Display the best parameters from manual search
print("\nManual Search - Best Parameters:")
print(manual_search$bestTune)


 
