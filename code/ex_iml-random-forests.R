#--example code from katherine

library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)

#--example using iml
library(iml)
bike <- load("data/bike.RData")
data(bike)
# Fit a random forest
bike %>% as_tibble()
bike_mod = randomForest::randomForest(x = bike %>% dplyr::select(-cnt), y = bike$cnt)

# Create a "predictor" object that 
# holds the model and the data
bike_pred = 
  Predictor$new(
    model = bike_mod,
    data = bike)
# Compute the partial dependence 
# function for temp and windspeed
# takes a looooooooong time
pdp = 
  FeatureEffect$new(
    predictor = bike_pred, 
    feature = c("hum", "temp"), 
    method = "pdp") 
# Create the partial dependence plot
pdp$plot() +
  viridis::scale_fill_viridis(
    option = "D") + 
  labs(x = "Humidity", 
       y = "Temperature", 
       fill = "Prediction")
