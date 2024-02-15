# Install and load the required packages
if (!require("httr")) {
  install.packages("httr", dependencies = TRUE, repos = "https://cloud.r-project.org")
}
if (!require("jsonlite")) {
  install.packages("jsonlite", dependencies = TRUE, repos = "https://cloud.r-project.org")
}
library(httr)
library(jsonlite)

# Generate the URL required to access the Titanic survival prediction API
base_url <- "http://127.0.0.1:5022/titanic_survival"

# Create a named list called "params" with the Titanic features
params <- list(
  Pclass = 3,
  Sex = "female",
  Age = 25,
  SibSp = 1,
  Parch = 0,
  Fare = 20,
  Embarked = "S"
)

# Modify the URL with the parameters
query_url <- httr::modify_url(url = base_url, query = params)

# Print the modified URL
print(query_url)

# Make a GET request to the API
model_prediction <- GET(query_url)

# Extract the results from the JSON response
model_result <- content(model_prediction, as = "text", encoding = "utf-8")
parsed_result <- fromJSON(model_result)

# Print the parsed result
print(parsed_result)
