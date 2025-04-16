########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
#Dataset should exist of the same features used in the training of a model 
#                                           minus complete wound healing.
#Paste the path of the model (Mpath)

########################################################################

#install.packages("caTools")
#install.packages("xgboost")

url_excelfile <- "processed_data.csv"
New_patient <- read.csv2(url_excelfile, sep = ',')
Mpath <- "logistic_regression_model.rds"


#changes characters to integers
for (colname in names(predictorset)){
  if (class(predictorset[,colname]) == "character"){
    class(predictorset[,colname]) <- "numeric"
  }
}

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library("e1071") #for support vector classifier
library(class) #for k-nearest neighbor
library("xgboost")#for extreme gradient boost


New_patient$ï..Ageyears <- sqrt(max_age - New_patient$ï..Ageyears)
New_patient$AverageO2saturationlevel <- sqrt(max_02 - New_patient$AverageO2saturationlevel)
New_patient$AverageserumHbA1c <- log(New_patient$AverageserumHbA1c)
New_patient$AverageserumeGFR <- sqrt(New_patient$AverageserumeGFR)
New_patient$Woundareameasurement <- log(New_patient$Woundareameasurement)

model <- readRDS(Mpath)
prediction <- predict(model, New_patient)

if (class(prediction) == "numeric") {
  prediction <- round(prediction,0)
}
#Shows predicted classes of the new patients
prediction
