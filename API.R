# Load required packages
library(plumber)
library(randomForest)
library(readxl)

#* @apiTitle Diabetes Prediction API

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

#* Predict Diabetes Risk
#* @param BMI:numeric BMI value
#* @param Age:numeric Age value
#* @param HighBP:integer Indicator for high blood pressure
#* @param HighChol:integer Indicator for high cholesterol
#* @param CholCheck:integer Indicator for cholesterol check
#* @param Smoker:integer Indicator for smoking status
#* @param Stroke:integer Indicator for stroke history
#* @param HeartDiseaseorAttack:integer Indicator for heart disease or attack
#* @param PhysActivity:integer Indicator for physical activity
#* @param Fruits:integer Indicator for fruit consumption
#* @param Veggies:integer Indicator for vegetable consumption
#* @param HvyAlcoholConsump:integer Indicator for heavy alcohol consumption
#* @param AnyHealthcare:integer Indicator for any healthcare access
#* @param NoDocbcCost:integer Indicator for no doctor visit due to cost
#* @param GenHlth:integer General health status
#* @param MentHlth:numeric Number of bad mental health days
#* @param PhysHlth:numeric Number of bad physical health days
#* @param DiffWalk:integer Indicator for difficulty walking
#* @param Sex:integer Sex of the individual
#* @param Education:integer Education level
#* @param Income:integer Income level
#* @get /pred
function(BMI = default_values['BMI'],
         Age = default_values['Age'],
         HighBP = default_values['HighBP'],
         HighChol = default_values['HighChol'],
         CholCheck = default_values['CholCheck'],
         Smoker = default_values['Smoker'],
         Stroke = default_values['Stroke'],
         HeartDiseaseorAttack = default_values['HeartDiseaseorAttack'],
         PhysActivity = default_values['PhysActivity'],
         Fruits = default_values['Fruits'],
         Veggies = default_values['Veggies'],
         HvyAlcoholConsump = default_values['HvyAlcoholConsump'],
         AnyHealthcare = default_values['AnyHealthcare'],
         NoDocbcCost = default_values['NoDocbcCost'],
         GenHlth = default_values['GenHlth'],
         MentHlth = default_values['MentHlth'],
         PhysHlth = default_values['PhysHlth'],
         DiffWalk = default_values['DiffWalk'],
         Sex = default_values['Sex'],
         Education = default_values['Education'],
         Income = default_values['Income']) {
  
  # Create a data frame with the input values
  input_data <- data.frame(BMI = as.numeric(BMI),
                           Age = as.numeric(Age),
                           HighBP = as.factor(HighBP),
                           HighChol = as.factor(HighChol),
                           CholCheck = as.factor(CholCheck),
                           Smoker = as.factor(Smoker),
                           Stroke = as.factor(Stroke),
                           HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
                           PhysActivity = as.factor(PhysActivity),
                           Fruits = as.factor(Fruits),
                           Veggies = as.factor(Veggies),
                           HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
                           AnyHealthcare = as.factor(AnyHealthcare),
                           NoDocbcCost = as.factor(NoDocbcCost),
                           GenHlth = as.factor(GenHlth),
                           MentHlth = as.numeric(MentHlth),
                           PhysHlth = as.numeric(PhysHlth),
                           DiffWalk = as.factor(DiffWalk),
                           Sex = as.factor(Sex),
                           Education = as.factor(Education),
                           Income = as.factor(Income))
  
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
