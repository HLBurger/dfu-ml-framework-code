#install.packages("themis")
library(themis)

# Use SMOTE for Oversampling


# Function to use SMOTE algorithm in training of the models

process_data <- function(data, ratio) {
  # Convert Target to factor with specified levels
  data$Target <- factor(data$Target, levels = c(0, 1, 2))
  
  # Oversample using SMOTE
  data <- smotenc(data, var = "Target", over_ratio = ratio)
  
  # Since patients are interpolated, numeric values can have a big amount of decimals
  # So numeric columns should be rounded in order to follow the original format of the numeric values
  
  numeric_columns <- c("Ageyears", "AverageserumHb", "AverageserumeGFR", 
                       "Averagediastolicbloodpressure", "Toepressureaffectedside",
                       "Degreeofnecrotictissuemeasurement", "Degreeofsloughmeasurement",
                       "Woundareameasurement")
  data[numeric_columns] <- lapply(data[numeric_columns], round, 0)
  
  # Convert columns based on conditions
  # Binairy values should either be a 0 or 1, if the value is above 0.5 it is converted to 1
  
  data$COPD <- ifelse(data$COPD > 0.5, 1, 0)
  data$Kidneydialysis <- ifelse(data$Kidneydialysis > 0.5, 1, 0)
  data$Type1 <- ifelse(data$Type1 > 0.5, 1, 0)
  data$IDDM <- ifelse(data$IDDM > 0.5, 1, 0)
  data$Neuropathic <- ifelse(data$Neuropathic > 0.5, 1, 0)
  data$LowerOcclusion <- ifelse(data$LowerOcclusion > 0.5, 1, 0)
  data$UpperOcclusion <- ifelse(data$UpperOcclusion > 0.5, 1, 0)
  data$LowerStenosis <- ifelse(data$LowerStenosis > 0.5, 1, 0)
  
  # For categories multiple values should be checked. If texas 2 is higher than 0.5
  # and texas 3 lower than 0.5 than texas 2 becomes 1
  # if Texas 3 is higher than Texas 2 it becomes Texas 3
  # if Texas 2 and Texas 3 are lower than 0.5 both become 0, since it is likely that it is Texas 1 wound
  data$Texas2 <- ifelse(data$Texas2 > 0.5 & data$Texas3 <= 0.5, 1, 
                        ifelse(data$Texas2 <= 0.5 & data$Texas3 > 0.5, 0, 0))
  data$Texas3 <- ifelse(data$Texas3 > 0.5 & data$Texas2 <= 0.5, 1, 
                        ifelse(data$Texas3 <= 0.5 & data$Texas2 > 0.5, 0, 0))
  data$TexasB <- ifelse(data$TexasB > 0.5 & data$TexasC <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasB <= 0.5 & data$TexasC > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasB <= 0.5 & data$TexasC <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasC <- ifelse(data$TexasC > 0.5 & data$TexasB <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasC <= 0.5 & data$TexasB > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasC <= 0.5 & data$TexasB <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasD <- ifelse(data$TexasD > 0.5 & data$TexasB <= 0.5 & data$TexasC <= 0.5, 1, 
                        ifelse(data$TexasD <= 0.5 & data$TexasB > 0.5 & data$TexasC <= 0.5, 0, 
                               ifelse(data$TexasD <= 0.5 & data$TexasB <= 0.5 & data$TexasC > 0.5, 0, 0)))
  
  # Convert wound location columns based on condition
  wound_location_columns <- c("Woundlocation_Anterior_Tibial_Artery", 
                              "Woundlocation_Dorsalis_Pedis", 
                              "Woundlocation_Lateral_Calcaneal", 
                              "Woundlocation_Lateral_Plantar", 
                              "Woundlocation_Medial_Calcaneal", 
                              "Woundlocation_Medial_Plantar", 
                              "Woundlocation_Posterior_Tibial_Artery")
  for (column in wound_location_columns) {
    data[[column]] <- ifelse(data[[column]] > 0.5, 1, 0)
  }
  
  return(data)
}



# SMOTE algortihm when Target is the level of amputation

process_data_level <- function(data, ratio) {
  data$TargetLevel <- factor(data$TargetLevel, levels = c(0, 1, 2, 3, 4))
  
  data <- smotenc(data, var = "TargetLevel", over_ratio = ratio)

  numeric_columns <- c("Ageyears", "AverageserumHb", "AverageserumeGFR", 
                       "Averagediastolicbloodpressure", "Toepressureaffectedside",
                       "Degreeofnecrotictissuemeasurement", "Degreeofsloughmeasurement",
                       "Woundareameasurement")
  data[numeric_columns] <- lapply(data[numeric_columns], round, 0)

  data$Hypertension <- ifelse(data$Hypertension > 0.5, 1, 0)
  data$HeartFailure <- ifelse(data$HeartFailure > 0.5, 1, 0)
  data$Neuropathic <- ifelse(data$Neuropathic > 0.5, 1, 0)
  data$WoundInfection <- ifelse(data$WoundInfection > 0.5, 1, 0)
  data$LowerOcclusion <- ifelse(data$LowerOcclusion > 0.5, 1, 0)
  data$UpperOcclusion <- ifelse(data$UpperOcclusion > 0.5, 1, 0)
  data$LowerStenosis <- ifelse(data$LowerStenosis > 0.5, 1, 0)
  data$UpperStenosis <- ifelse(data$UpperStenosis > 0.5, 1, 0)
  data$Texas2 <- ifelse(data$Texas2 > 0.5 & data$Texas3 <= 0.5, 1, 
                        ifelse(data$Texas2 <= 0.5 & data$Texas3 > 0.5, 0, 0))
  data$Texas3 <- ifelse(data$Texas3 > 0.5 & data$Texas2 <= 0.5, 1, 
                        ifelse(data$Texas3 <= 0.5 & data$Texas2 > 0.5, 0, 0))
  data$TexasB <- ifelse(data$TexasB > 0.5 & data$TexasC <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasB <= 0.5 & data$TexasC > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasB <= 0.5 & data$TexasC <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasC <- ifelse(data$TexasC > 0.5 & data$TexasB <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasC <= 0.5 & data$TexasB > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasC <= 0.5 & data$TexasB <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasD <- ifelse(data$TexasD > 0.5 & data$TexasB <= 0.5 & data$TexasC <= 0.5, 1, 
                        ifelse(data$TexasD <= 0.5 & data$TexasB > 0.5 & data$TexasC <= 0.5, 0, 
                               ifelse(data$TexasD <= 0.5 & data$TexasB <= 0.5 & data$TexasC > 0.5, 0, 0)))

  wound_location_columns <- c("Woundlocation_Anterior_Tibial_Artery", 
                              "Woundlocation_Dorsalis_Pedis",
                              "Woundlocation_Lateral_Plantar",
                              "Woundlocation_Medial_Plantar"
                              )
  for (column in wound_location_columns) {
    data[[column]] <- ifelse(data[[column]] > 0.5, 1, 0)
  }
  
  return(data)
}



# SMOTE algorithm for Logistic Regression model, because Logistic Regression
# uses other variables than the other models.

process_data_logistic <- function(data, ratio) {
  data$Target <- factor(data$Target, levels = c(0, 1, 2))

  data <- smotenc(data, var = "Target", over_ratio = ratio)

  numeric_columns <- c("Degreeofnecrotictissuemeasurement", "Degreeofsloughmeasurement",
                       "Woundareameasurement")
  data[numeric_columns] <- lapply(data[numeric_columns], round, 0)
  
  data$COPD <- ifelse(data$COPD > 0.5, 1, 0)
  data$Kidneydialysis <- ifelse(data$Kidneydialysis > 0.5, 1, 0)
  data$HeartFailure <- ifelse(data$HeartFailure > 0.5, 1, 0)
  data$Type1 <- ifelse(data$Type1 > 0.5, 1, 0)
  data$IDDM <- ifelse(data$IDDM > 0.5, 1, 0)
  data$LowerOcclusion <- ifelse(data$LowerOcclusion > 0.5, 1, 0)
  data$UpperOcclusion <- ifelse(data$UpperOcclusion > 0.5, 1, 0)
  data$LowerStenosis <- ifelse(data$LowerStenosis > 0.5, 1, 0)
  data$Texas2 <- ifelse(data$Texas2 > 0.5 & data$Texas3 <= 0.5, 1, 
                        ifelse(data$Texas2 <= 0.5 & data$Texas3 > 0.5, 0, 0))
  data$Texas3 <- ifelse(data$Texas3 > 0.5 & data$Texas2 <= 0.5, 1, 
                        ifelse(data$Texas3 <= 0.5 & data$Texas2 > 0.5, 0, 0))

  Texas <- c("TexasB", "TexasC")
  for (column in Texas) {
    data[[column]] <- ifelse(data[[column]] > 0.5, 1, 0)
  }

  wound_location_columns <- c("Woundlocation_Anterior_Tibial_Artery",
                              "Woundlocation_Dorsalis_Pedis",
                              "Woundlocation_Lateral_Calcaneal", 
                              "Woundlocation_Medial_Calcaneal", 
                              "Woundlocation_Medial_Plantar")
  for (column in wound_location_columns) {
    data[[column]] <- ifelse(data[[column]] > 0.5, 1, 0)
  }
  
  return(data)
}


# SMOTE algortihm for Neural Networks, numeric values are normalized
# it is not necessary to round up the numeric values

process_data_MLP <- function(data, ratio) {
  data$Target <- factor(data$Target, levels = c(0, 1, 2))

  data <- smotenc(data, var = "Target", over_ratio = ratio)

  data$COPD <- ifelse(data$COPD > 0.5, 1, 0)
  data$Kidneydialysis <- ifelse(data$Kidneydialysis > 0.5, 1, 0)
  data$Type1 <- ifelse(data$Type1 > 0.5, 1, 0)
  data$IDDM <- ifelse(data$IDDM > 0.5, 1, 0)
  data$Neuropathic <- ifelse(data$Neuropathic > 0.5, 1, 0)
  data$LowerOcclusion <- ifelse(data$LowerOcclusion > 0.5, 1, 0)
  data$UpperOcclusion <- ifelse(data$UpperOcclusion > 0.5, 1, 0)
  data$LowerStenosis <- ifelse(data$LowerStenosis > 0.5, 1, 0)
  data$Texas2 <- ifelse(data$Texas2 > 0.5 & data$Texas3 <= 0.5, 1, 
                        ifelse(data$Texas2 <= 0.5 & data$Texas3 > 0.5, 0, 0))
  data$Texas3 <- ifelse(data$Texas3 > 0.5 & data$Texas2 <= 0.5, 1, 
                        ifelse(data$Texas3 <= 0.5 & data$Texas2 > 0.5, 0, 0))
  data$TexasB <- ifelse(data$TexasB > 0.5 & data$TexasC <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasB <= 0.5 & data$TexasC > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasB <= 0.5 & data$TexasC <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasC <- ifelse(data$TexasC > 0.5 & data$TexasB <= 0.5 & data$TexasD <= 0.5, 1, 
                        ifelse(data$TexasC <= 0.5 & data$TexasB > 0.5 & data$TexasD <= 0.5, 0, 
                               ifelse(data$TexasC <= 0.5 & data$TexasB <= 0.5 & data$TexasD > 0.5, 0, 0)))
  data$TexasD <- ifelse(data$TexasD > 0.5 & data$TexasB <= 0.5 & data$TexasC <= 0.5, 1, 
                        ifelse(data$TexasD <= 0.5 & data$TexasB > 0.5 & data$TexasC <= 0.5, 0, 
                               ifelse(data$TexasD <= 0.5 & data$TexasB <= 0.5 & data$TexasC > 0.5, 0, 0)))

  wound_location_columns <- c("Woundlocation_Anterior_Tibial_Artery", 
                              "Woundlocation_Dorsalis_Pedis", 
                              "Woundlocation_Lateral_Calcaneal", 
                              "Woundlocation_Lateral_Plantar", 
                              "Woundlocation_Medial_Calcaneal", 
                              "Woundlocation_Medial_Plantar", 
                              "Woundlocation_Posterior_Tibial_Artery")
  for (column in wound_location_columns) {
    data[[column]] <- ifelse(data[[column]] > 0.5, 1, 0)
  }
  
  return(data)
}
