# api.R
library(plumber)
library(randomForest)
library(readxl)
library(caret)
library(ggplot2)

#* @apiTitle Diabetes Prediction API

# Set seed for reproducibility
set.seed(13579)

# Read in the data
diabetes_data <- read_excel("diabetes_binary_health_indicators_BRFSS2015.xlsm")

# Convert Diabetes_binary to a factor with two levels
diabetes_data$Diabetes_binary <- factor(diabetes_data$Diabetes_binary, levels = c(0, 1))

# Ensure that the factor levels have valid R names
levels(diabetes_data$Diabetes_binary) <- make.names(levels(diabetes_data$Diabetes_binary))

# Split the data into training (70%) and test (30%) sets
trainIndex <- createDataPartition(diabetes_data$Diabetes_binary, p = 0.7, list = FALSE)
trainData <- diabetes_data[trainIndex, ]
testData <- diabetes_data[-trainIndex, ]

# Define the best model formula
best_model_formula <- Diabetes_binary ~ HighBP + HighChol + CholCheck + Smoker + Stroke + HeartDiseaseorAttack 

# Fit the best model with a corrected mtry value
num_predictors <- length(all.vars(best_model_formula)) - 1 # Subtract 1 for the response variable
mtry_value <- max(1, floor(sqrt(num_predictors))) # Ensure mtry is at least 1

# Fit the best model
best_model <- randomForest(best_model_formula, data = diabetes_data, mtry = mtry_value, ntree = 20, importance = TRUE)

# Compute default values for numeric and categorical variables
default_values <- sapply(diabetes_data, function(x) {
  if (is.numeric(x)) {
    # Return the mean for numeric columns
    return(mean(x, na.rm = TRUE))
  } else if (is.factor(x) || is.character(x)) {
    # Return the most frequent value for categorical columns
    return(as.character(sort(table(x), decreasing = TRUE)[1]))
  } else {
    # Return NA for unknown types
    return(NA)
  }
})

#* Predict Diabetes Risk
#* @param BMI:numeric BMI value
#* @param Age:numeric Age value
#* @param HighBP:integer Indicator for high blood pressure
#* @param HighChol:integer Indicator for high cholesterol
#* @param CholCheck:integer Indicator for cholesterol check
#* @param Smoker:integer Indicator for smoking status
#* @param Stroke:integer Indicator for stroke history
#* @param HeartDiseaseorAttack:integer Indicator for heart disease or attack
#* @get /pred
function(BMI = default_values['BMI'],
         Age = default_values['Age'],
         HighBP = default_values['HighBP'],
         HighChol = default_values['HighChol'],
         CholCheck = default_values['CholCheck'],
         Smoker = default_values['Smoker'],
         Stroke = default_values['Stroke'],
         HeartDiseaseorAttack = default_values['HeartDiseaseorAttack']) {
  
  # Create a data frame with the input values
  input_data <- data.frame(BMI = as.numeric(BMI),
                           Age = as.numeric(Age),
                           HighBP = as.factor(HighBP),
                           HighChol = as.factor(HighChol),
                           CholCheck = as.factor(CholCheck),
                           Smoker = as.factor(Smoker),
                           Stroke = as.factor(Stroke),
                           HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack))
  
  # Predict the probability of diabetes
  prediction <- predict(best_model, input_data, type = "response")
  
  # Return the prediction
  list(prediction = prediction)
}

#* Get Information
#* @get /info
function() {
  list(
    name = "Hanan Ali",
    github_url = "https://github.com/hbali23/FinalProject"
  )
}

# Example function calls
# curl -X GET "http://127.0.0.1:8000/pred"
# curl -X GET "http://127.0.0.1:8000/pred?BMI=25.0&Age=50&HighBP=yes&HighChol=yes&Stroke=no&HeartDiseaseorAttack=no&PhysActivity=yes&Fruits=yes&Veggies=yes&HvyAlcoholConsump=no&GenHlth=3&MentHlth=2&PhysHlth=2&DiffWalk=no&Education=3&Income=4"
# curl -X GET "http://127.0.0.1:8000