# Load necessary libraries
library(plumber)
library(randomForest)
library(dplyr)
library(readxl)

# Set seed for reproducibility
set.seed(13579)

# Read in the data
diabetes_data <- read_excel("~/Downloads/diabetes_binary_health_indicators_BRFSS2015.xlsm")

# Convert Diabetes_binary to a factor with two levels
diabetes_data$Diabetes_binary <- factor(diabetes_data$Diabetes_binary, levels = c(0, 1))
levels(diabetes_data$Diabetes_binary) <- make.names(levels(diabetes_data$Diabetes_binary))

# Define the best model formula
best_model_formula <- Diabetes_binary ~ HighBP + HighChol + CholCheck + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Education + Income

# Fit the best model
best_model <- randomForest(best_model_formula, data = diabetes_data, mtry = floor((ncol(diabetes_data) - 1) / 3), ntree = 50, importance = TRUE)

# Define default values for predictors
default_values <- sapply(diabetes_data, function(x) {
  if (is.numeric(x)) {
    return(mean(x, na.rm = TRUE))
  } else {
    return(as.character(sort(table(x), decreasing = TRUE)[1]))
  }
})

# Create a Plumber router
r <- plumb()

# Define the /pred endpoint
#* @apiTitle Diabetes Prediction API
#* @apiDescription API for predicting diabetes using a random forest model

#* @param BMI Numeric, default = mean(BMI)
#* @param Age Numeric, default = mean(Age)
#* @param HighBP Factor, default = most prevalent class
#* @param HighChol Factor, default = most prevalent class
#* @param CholCheck Factor, default = most prevalent class
#* @param Smoker Factor, default = most prevalent class
#* @param Stroke Factor, default = most prevalent class
#* @param HeartDiseaseorAttack Factor, default = most prevalent class
#* @param PhysActivity Factor, default = most prevalent class
#* @param Fruits Factor, default = most prevalent class
#* @param Veggies Factor, default = most prevalent class
#* @param HvyAlcoholConsump Factor, default = most prevalent class
#* @param AnyHealthcare Factor, default = most prevalent class
#* @param NoDocbcCost Factor, default = most prevalent class
#* @param GenHlth Factor, default = most prevalent class
#* @param MentHlth Numeric, default = mean(MentHlth)
#* @param PhysHlth Numeric, default = mean(PhysHlth)
#* @param DiffWalk Factor, default = most prevalent class
#* @param Sex Factor, default = most prevalent class
#* @param Education Factor, default = most prevalent class
#* @param Income Factor, default = most prevalent class
#* @get /pred
function(BMI = default_values['BMI'], Age = default_values['Age'], HighBP = default_values['HighBP'],
         HighChol = default_values['HighChol'], CholCheck = default_values['CholCheck'], Smoker = default_values['Smoker'],
         Stroke = default_values['Stroke'], HeartDiseaseorAttack = default_values['HeartDiseaseorAttack'], 
         PhysActivity = default_values['PhysActivity'], Fruits = default_values['Fruits'], Veggies = default_values['Veggies'], 
         HvyAlcoholConsump = default_values['HvyAlcoholConsump'], AnyHealthcare = default_values['AnyHealthcare'], 
         NoDocbcCost = default_values['NoDocbcCost'], GenHlth = default_values['GenHlth'], MentHlth = default_values['MentHlth'], 
         PhysHlth = default_values['PhysHlth'], DiffWalk = default_values['DiffWalk'], Sex = default_values['Sex'], 
         Education = default_values['Education'], Income = default_values['Income']) {
  
  new_data <- data.frame(
    BMI = as.numeric(BMI), Age = as.numeric(Age), HighBP = as.factor(HighBP), HighChol = as.factor(HighChol), 
    CholCheck = as.factor(CholCheck), Smoker = as.factor(Smoker), Stroke = as.factor(Stroke), 
    HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack), PhysActivity = as.factor(PhysActivity), 
    Fruits = as.factor(Fruits), Veggies = as.factor(Veggies), HvyAlcoholConsump = as.factor(HvyAlcoholConsump), 
    AnyHealthcare = as.factor(AnyHealthcare), NoDocbcCost = as.factor(NoDocbcCost), GenHlth = as.factor(GenHlth), 
    MentHlth = as.numeric(MentHlth), PhysHlth = as.numeric(PhysHlth), DiffWalk = as.factor(DiffWalk), 
    Sex = as.factor(Sex), Education = as.factor(Education), Income = as.factor(Income)
  )
  
  prediction <- predict(best_model, new_data, type = "prob")
  return(prediction)
}

# Define the /info endpoint
#* @get /info
function() {
  list(
    name = "Your Name",
    github_url = "https://your-github-pages-site-url"
  )
}

# Start the Plumber API
r$run(port = 8000)

# Example function calls to the API:
# http://localhost:8000/pred
# http://localhost:8000/pred?BMI=30&Age=45&HighBP=1
# http://localhost:8000/info
