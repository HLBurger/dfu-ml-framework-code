########################################################################
#Insert excel location path between the " "
#Select whether imputation of empty values (IMP) needs to be performed
#Select whether balancing of the data (BAL) needs to be performed
#install the required packages if not yet done
########################################################################
#install.packages('tidyverse')
#install.packages('fastDummies')
#install.packages('ggplot2')
#install.packages("smotefamily")
#install.packages("mice")

path_excelfile <- "diabetic_ulcer_data.csv"
Patient_dataset <- read.csv2(path_excelfile)
IMP <- TRUE
BAL <- TRUE

library('tidyverse') #for dataset cleaning
library('fastDummies') #for dummy creation
library(ggplot2)  #better viz in general
library(smotefamily)#for smote balancing
library(mice)     #for mice imputation
library(dplyr)
library("heatmaply") #correlation matrix visualization
library("car")

############################################
#Performs correctional tests on the data
############################################
Patient_dataset <- Patient_dataset[Patient_dataset$Comments == " ",]                                 #Removes the LTFU's from the dataset
Patient_dataset$Woundareameasurement <- replace(Patient_dataset$Woundareameasurement, 
                                         Patient_dataset$Woundareameasurement == 0, NA)              #Replaces 0 with NA, as a wound cannot be the size of 0
Patient_dataset <- dummy_cols(Patient_dataset, select_columns = "Ethnicity")                         #dummy vars for Ethnicity
Patient_dataset <- dummy_cols(Patient_dataset, select_columns = "Levelofamputation")                 #dummy vars for Levelofamputation
Patient_dataset <- dummy_cols(Patient_dataset, select_columns = "Levelofreamputation")               #dummy vars for Levelofreamputation
Patient_dataset <- dummy_cols(Patient_dataset, select_columns = "Patentpedalarch")                   #dummy vars for Patentpedalarch

#Looks at length of treatment. If > 365 it counts as non fully healing (within a year)
time_till_healing <- as.POSIXct(Patient_dataset$Dateofcompletewoundhealing, format = "%m/%d/%Y") -  as.POSIXct(Patient_dataset$StartdateoftreatmentWEC, format = "%m/%d/%Y")
time_till_healing <- as.data.frame((time_till_healing))
Patient_dataset[which(time_till_healing$`(time_till_healing)` > 365),]$Completewoundhealing <- 0

#creates seventh level of (re)amputation/ethnicity if lacking in dummy vars
if (!("Levelofamputation_7" %in% names(Patient_dataset))){
  Patient_dataset$Levelofamputation_7 <- 0
}
if (!("Levelofreamputation_7" %in% names(Patient_dataset))){
  Patient_dataset$Levelofreamputation_7 <- 0
}
if (!("Ethnicity_5" %in% names(Patient_dataset))){
Patient_dataset$Ethnicity_5 <- 0
}
#renames columns for convenience
Patient_dataset <- Patient_dataset %>%
  rename(
    "European" = Ethnicity_1,
    "Hindustan" = Ethnicity_2,
    "African" = Ethnicity_3,
    "Asian" = Ethnicity_4,
    "Latin_American" = Ethnicity_5,
    "Arabian" = Ethnicity_6,
    "Mixed" = Ethnicity_7,
    "Unknown" = Ethnicity_8,
    
    "PedalArch.Open" = Patentpedalarch_1,
    "PedalArch.Stenose" = Patentpedalarch_2,
    "PedalArch.Occlusie" = Patentpedalarch_3,
    
    "Levelofamputation.Toe_amputation" = Levelofamputation_1,
    "Levelofamputation.TMTA_of_1_toe" = Levelofamputation_2,
    "Levelofamputation.TMTA_of_multiple_toes" = Levelofamputation_3,
    "Levelofamputation.TMT_forefoot_amputation" = Levelofamputation_4,
    "Levelofamputation.Proximal_forefoot_amputation" = Levelofamputation_5,
    "Levelofamputation.Transtibial_amputation" = Levelofamputation_6,
    "Levelofamputation.Knee_disarticulation" = Levelofamputation_7,
    "Levelofamputation.Transfemoral_amputation" = Levelofamputation_8,
    
    "Levelofreamputation.Toe_amputation" = Levelofreamputation_1,
    "Levelofreamputation.TMTA_of_1_toe" = Levelofreamputation_2,
    "Levelofreamputation.TMTA_of_multiple_toes" = Levelofreamputation_3,
    "Levelofreamputation.TMT_forefoot_amputation" = Levelofreamputation_4,
    "Levelofreamputation.Proximal_forefoot_amputation" = Levelofreamputation_5,
    "Levelofreamputation.Transtibial_amputation" = Levelofreamputation_6,
    "Levelofreamputation.Knee_disarticulation" = Levelofreamputation_7,
    "Levelofreamputation.Transfemoral_amputation" = Levelofreamputation_8,
  )

#Replaces complete healing with 0 if amputation is higher than foot
Patient_dataset[rowSums(Patient_dataset[,
                          c("Levelofreamputation.Knee_disarticulation", "Levelofreamputation.Transfemoral_amputation", 
                            "Levelofamputation.Knee_disarticulation", "Levelofreamputation.Transtibial_amputation", 
                            "Levelofamputation.Transfemoral_amputation", "Levelofamputation.Transtibial_amputation", 
                            "Levelofsecondreamputation.Transtibial_amputation", "Levelofsecondreamputation.Knee_disarticulation", 
                            "Levelofsecondreamputation.Transfemoral_amputation")], na.rm = TRUE) >= 1,]$Completewoundhealing <- 0

#Redistributes Texas wound classification
Patient_dataset$Texas1 <- rowSums(Patient_dataset[,c("A1", "B1", "C1", "D1")])
Patient_dataset$Texas2 <- rowSums(Patient_dataset[,c("A2", "B2", "C2", "D2")])
Patient_dataset$Texas3 <- rowSums(Patient_dataset[,c("A3", "B3", "C3", "D3")])
Patient_dataset$TexasA <- rowSums(Patient_dataset[,c("A1", "A2", "A3")])
Patient_dataset$TexasB <- rowSums(Patient_dataset[,c("B1", "B2", "B3")])
Patient_dataset$TexasC <- rowSums(Patient_dataset[,c("C1", "C2", "C3")])
Patient_dataset$TexasD <- rowSums(Patient_dataset[,c("D1", "D2", "D3")])

Patient_dataset[Patient_dataset$Neuroischemic == 1,][,c("Neuropathic", "Ischemic")] <- 1

#Deletes medical specifications on medicin use (to be discussed)
Patient_dataset <- subset(Patient_dataset, select = -c(DiabetesMedication, Medicationaffectingwoundhealing ))

#Deletes pedal arch (not enough datapoints)
Patient_dataset <- subset(Patient_dataset, select = -c(PedalArch.Stenose, PedalArch.Open, PedalArch.Occlusie, Patentpedalarch_NA))

#Deletes chkcategorisation (based of AverageserumGFR)
Patient_dataset <- subset(Patient_dataset, select = -c(Chronickidneydisease, CKD, CKD3a, CKD3b, CKD4, CKD5))

#Deletes TPaff50 (highly correlated with/based off Toepressureaffectedside)
Patient_dataset <- subset(Patient_dataset, select = -c(TPaff50))

#Deletes HeartFailure (lacking elements and highly correlated with LVEF (Y/N))
Patient_dataset <- subset(Patient_dataset, select = -c(HeartFailure))

#Deletes LVEF (lacking elements)
Patient_dataset <- subset(Patient_dataset, select = -c(LVEF))

#Deletes Toepressurecontralateralside (highly correlated with Toepressureaffectedside which had a higher correlation wit Completewoundhealing)
Patient_dataset <- subset(Patient_dataset, select = -c(Toepressurecontralateralside))

#Deletes Anemia because of high correlation and derived from AverageserumHb
Patient_dataset <- subset(Patient_dataset, select = -c(Anemia))

#Adds degreeofredareameasurement
Patient_dataset$Degreeofredareameasurement <- (100 - Patient_dataset$Degreeofnecrotictissuemeasurement - Patient_dataset$Degreeofsloughmeasurement)

############################################
#Statistical correlation tests
############################################
signames <- c()
chinames <- c()
cornames <- c()
for ( i in 1:length(Patient_dataset)) {

  #binary combinations / Tetrachoric correlation / chi squared test
  if (((max(Patient_dataset[i]) == 1) & n_distinct(Patient_dataset[i]) <= 2) |
        (is.na(max(Patient_dataset[i])) & (n_distinct(Patient_dataset[i]) <= 3))){
    
    chitest <- chisq.test(Patient_dataset$Completewoundhealing, unlist(unname(Patient_dataset[i])))
    
    if (chitest["p.value"] < 0.05){
      print(i)
      signames <- append(signames, colnames(Patient_dataset[i]))
      chinames <- append(chinames, colnames(Patient_dataset[i]))
    }

  }
  #numerical and binary combination / Pearson coefficient correlation 
  else {
    cor <- oneway.test(unlist(unname(Patient_dataset[i])) ~ Patient_dataset$Completewoundhealing)
    if (cor["p.value"] < 0.05 & n_distinct(Patient_dataset[i]) > 1){
      print(i)
      signames <- append(signames, colnames(Patient_dataset[i]))
      cornames <- append(cornames, colnames(Patient_dataset[i]))

    }

  }
}

print("Features where Completewoundhealing causes a significant difference between groups: \n")
print(cornames)

#Selects significant features
predictorset <- subset(Patient_dataset, select = c("Ageyears", "Nosmoking",
                                            "Averagesystolicbloodpressure", "Averagediastolicbloodpressure",
                                            "AverageO2saturationlevel", "AverageserumHbA1c", "AverageserumHb", "AverageserumeGFR",
                                            "Woundlocation.Anterior_Tibial_Artery", "Woundlocation.Dorsalis_Pedis", "Woundlocation.Lateral_Calcaneal",
                                            "Woundlocation.Lateral_Plantar", "Woundlocation.Medial_Calcaneal", "Woundlocation.Medial_Plantar",
                                            "Woundlocation.Posterior_Tibial_Artery", "Toepressureaffectedside",
                                            "Type1", "Texas1", "Texas2", "TexasA", "TexasB", "TexasC", "Neuropathic",
                                            "Woundareameasurement",
                                            "Completewoundhealing"))


#Performs feature transformation
max_age <- max(predictorset$Ageyears)
predictorset$Ageyears <- sqrt(max(predictorset$Ageyears) - predictorset$Ageyears)

max_02 <- max(predictorset$AverageO2saturationlevel, na.rm = TRUE)
predictorset$AverageO2saturationlevel <- sqrt(max(predictorset$AverageO2saturationlevel, na.rm = TRUE) - predictorset$AverageO2saturationlevel)

predictorset$AverageserumHbA1c <- log(predictorset$AverageserumHbA1c)

predictorset$AverageserumeGFR <- sqrt(predictorset$AverageserumeGFR)

predictorset$Woundareameasurement <- log(predictorset$Woundareameasurement)


#creates correlation matrix + heatmap + vif checks
cor_matrix <- cor(predictorset, use = "complete.obs")
heatmaply(cor_matrix, Rowv = NA, Colv = NA, node_type = "scatter", point_size_mat = abs(cor_matrix), row_text_angle = 45, column_text_angle = 90)
heatmaply(cor(predictorset, predictorset$Completewoundhealing, use = "complete.obs"), Rowv = NA, Colv = NA, node_type = "scatter", point_size_mat = abs(cor(predictorset, predictorset$Completewoundhealing, use = "complete.obs")))

vif_model <- lm(Completewoundhealing ~ ., data = predictorset)
vif(vif_model)

############################################
#Performs imputation on data (optional)
#Only if IMP = TRUE (see line 12)
############################################
if (IMP == TRUE){
  missing_names <- names(which(colSums(is.na(predictorset)) > 0))
  imputation <- mice(predictorset[,missing_names], m = 50, method = c("midastouch"))
  
  #Validation for imputation of empty values
  for(name in 1:length(missing_names)){
    impdata <- data.frame(imputation$imp[name])
    median_vector <- apply(impdata, 1, median)
    weight_matrix <- 1/abs(impdata - median_vector)
    weight_matrix[weight_matrix > 2] <- 2
    impute_list <- c()
    for (row in 1:nrow(impdata)){
      
      new_impval <- (weight_matrix[row,] * impdata[row,])/sum(weight_matrix[row,])
      impute_list <- append(impute_list, sum(new_impval) )
      
    }
    predictorset[,missing_names[name]][is.na(predictorset[,missing_names[name]])] <- impute_list
    
  }
}

############################################
#Performs balancing on data (optional)
#Only if BAL = TRUE (see line 13)
############################################

if (BAL == TRUE){
  
  genpred <- ADAS(predictorset[,-ncol(predictorset)], predictorset[,ncol(predictorset)])
  
  #rounds each categorical variable to its closest category
  for (col in names(genpred$data)[sapply(genpred$data, function(x) is.factor(x) || (is.numeric(x) && max(unique(x)) <= 1 && min(unique(x)) >= 0))]){
    
    genpred$data[[col]] <- round(genpred$data[[col]])
    
  }
  
  
  predictorset <- genpred$data
  names(predictorset)[ncol(predictorset)] <- "Completewoundhealing"
  predictorset$Completewoundhealing <- as.numeric(predictorset$Completewoundhealing)
  
  
  #Dataset validity tests
  predictorset <- predictorset[predictorset$Averagesystolicbloodpressure >= predictorset$Averagediastolicbloodpressure,] #check blood pressure
  predictorset <- predictorset[rowSums(predictorset[,c("Woundlocation.Anterior_Tibial_Artery", "Woundlocation.Dorsalis_Pedis", "Woundlocation.Lateral_Calcaneal",
                                                       "Woundlocation.Lateral_Plantar", "Woundlocation.Medial_Calcaneal", "Woundlocation.Medial_Plantar",
                                                       "Woundlocation.Posterior_Tibial_Artery")]) == 1,] #check only 1 woundlocation
  predictorset <- predictorset[rowSums(predictorset[,c("Texas1", "Texas2", "TexasA", "TexasB", "TexasC")]) <= 2,] #check Texas classifications

}

#changes characters to integers
for (colname in names(predictorset)){
  if (class(predictorset[,colname]) == "character"){
    class(predictorset[,colname]) <- "numeric"
  }
}

#export predictorset as a csv file
directory <- dirname(normalizePath(path_excelfile))
write.csv(predictorset, paste0(directory, "/processed_data.csv"), row.names = FALSE)
