#library(MASS)
#library(caret)
library(ggplot2)

# Check feature importances

# the dataset used is the one were patients with unknown wound healing 
# and patients who had a toe pressure lower than 51 and did not have a CT scan
# were excluded from the dataset
# in total there are:
# 344 patients without amputation (within a year)
# 164 with amputation (within a year)
# 144 with no wound healing (within a year)

data <- ImputedDatasetALL

#################################################
# FEATURE IMPORTANCE CATEGORIES
#################################################

variables_to_check_category <- c("Gender", "Smoking", "Nosmoking", "Quitsmoking", "Activesmoking","NotSmokingNow",
                        "Alcoholuse", "NoAlcoholuse", "QuitAlcoholuse", "CurrentAlcoholuse",
                        "Ethnicity_1", "Ethnicity_2", "Ethnicity_7","OtherEthnicities",
                        "Hypertension", "COPD", "Kidneydialysis","HeartFailure", "Anemia",
                        "Chronickidneydisease", "CKD", "CKD3a", "CKD3b", "CKD4", "CKD5",
                        "DiabetesType", "Type1", "NIDDM", "IDDM", "Woundlocation_Anterior_Tibial_Artery",
                        "Woundlocation_Dorsalis_Pedis", "Woundlocation_Posterior_Tibial_Artery", 
                        "Woundlocation_Medial_Calcaneal", "Woundlocation_Medial_Plantar", 
                        "Woundlocation_Lateral_Calcaneal", "Woundlocation_Lateral_Plantar", 
                        "A1", "A2", "A3", "B1","B2","B3", "C1", "C2", "C3", "D1", "D2", "D3",
                        "Texas1", "Texas2", "Texas3", "TexasA",
                        "TexasB", "TexasC", "TexasD", "Ischemicandorneuropathicwound", "Neuropathic",
                        "Ischemic", "Neuroischemic", "WoundInfection", "TPaff50", "PAV", 
                        "Occlusion","UpperOcclusion","LowerOcclusion","BothOcclusions")

# Create dataframe with p values and variables
p_values <- list()
p_values_df <- data.frame(Variable = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# with chi-squared the variables are checked on feature importance
for (variable in variables_to_check_category) {
  data[[variable]] <- round(data[[variable]], digits = 0)
  print(variable)
  cross_table <- table(data$Target, data[[variable]])
  print(cross_table)
  chi_sq_test <- chisq.test(cross_table)
  p_values[[variable]] <- chi_sq_test$p.value
  p_values_df <- rbind(p_values_df, data.frame(Variable = variable, P_Value = chi_sq_test$p.value))
}
p_values_df$P_Value <- round(p_values_df$P_Value, digits = 5)

#################################################
# FEATURE IMPORTANCE NUMERIC
#################################################
variables_to_check_numeric <- c("Ageyears", "Amountofsigarettes", "Packyears", "Amountofalcohol",
                                "Averagesystolicbloodpressure", "Averagediastolicbloodpressure",
                                "AverageO2saturationlevel", "LVEF", "AverageserumHbA1c",
                                "AverageserumHb", "AverageserumeGFR", "Durationofdiabetes",
                                "Durationofinsulinuse", "Daysbeforetreatement",
                                "Woundareameasurement", "Degreeofsloughmeasurement",
                                "Degreeofnecrotictissuemeasurement", "Degreeofredtissue",
                                "Toepressureaffectedside", "Toepressurecontralateralside", 
                                "Bloodpressuredifference", "TBI")

# First it is necessary to check if variables are normally distributed
# the variables are being plotted in groups of 4

variables_to_check_numeric1 <- c("Ageyears", "Amountofsigarettes", "Packyears", "Amountofalcohol")
                                
variables_to_check_numeric2 <- c("Averagesystolicbloodpressure", "Averagediastolicbloodpressure",
                                "AverageO2saturationlevel",  "AverageserumHbA1c")
                                
variables_to_check_numeric3 <- c("AverageserumHb", "AverageserumeGFR", "Durationofdiabetes",
                                "Durationofinsulinuse")

variables_to_check_numeric4 <- c("Woundareameasurement", "TBI",
                                  "Degreeofsloughmeasurement","Degreeofnecrotictissuemeasurement")

variables_to_check_numeric5 <- c("Degreeofredtissue","Toepressureaffectedside", 
                                  "Toepressurecontralateralside", "Bloodpressuredifference")


plot.new()
par(mfrow=c(2, 2), mar=c(2, 2, 2, 2))
par(family = "serif") 
par(cex.lab = 1.5, cex.main = 1.5)

for (var in variables_to_check_numeric1) {
  plot(density(df[[var]]), main=paste("Density Plot of", var), xlab=var, 
       ylab="Density", col="royalblue1", lwd=3)
}

for (var in variables_to_check_numeric2) {
  plot(density(df[[var]]), main=paste("Density Plot of", var), xlab=var, 
       ylab="Density", col="royalblue1", lwd=3)
}

for (var in variables_to_check_numeric3) {
  plot(density(df[[var]]), main=paste("Density Plot of", var), xlab=var, 
       ylab="Density", col="royalblue1", lwd=2)
}

for (var in variables_to_check_numeric4) {
  plot(density(df[[var]]), main=paste("Density Plot of", var), xlab=var, 
       ylab="Density", col="royalblue1", lwd=3)
}

for (var in variables_to_check_numeric5) {
  plot(density(df[[var]]), main=paste("Density Plot of", var), xlab=var, 
       ylab="Density", col="royalblue1", lwd=3)
}

# A dataframe is created to store the ANOVA results and the shapiro results
anova_results <- list()
shapiro_wilk_results <- data.frame(Variable = character(),
                                   shapiro = numeric(),
                                   kruskal = numeric(),
                                   anova = numeric(),
                                   stringsAsFactors = FALSE)

# Loop over each variable
for (variable in variables_to_check_numeric) {
  # Perform Shapiro-Wilk test
  shapiro_test_result <- shapiro.test(df[[variable]])
  kruskal_test_result <- kruskal.test(df[[variable]] ~ factor(df$Target))
  
  # Perform ANOVA
  anova_result <- aov(as.formula(paste(variable, "~ Target")), data = df)
  anova_results[[variable]] <- anova_result
  
  # Extract F-value and p-value
  f_value <- summary(anova_result)[[1]]$F[1]
  p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
  
  # Store results in dataframe
  shapiro_wilk_results <- rbind(shapiro_wilk_results, 
                                data.frame(Variable = variable,
                                           
                                           shapiro = shapiro_test_result$p.value,
                                           kruskal = kruskal_test_result$p.value,
                                           anova = p_value))
}
shapiro_wilk_results$shapiro <- round(shapiro_wilk_results$shapiro, digits = 5)
shapiro_wilk_results$kruskal <- round(shapiro_wilk_results$kruskal, digits = 5)
shapiro_wilk_results$anova <- round(shapiro_wilk_results$anova, digits = 5)
shapiro_wilk_results$Is_Normal_Distributed <- ifelse(shapiro_wilk_results$shapiro >= 0.05, "1", "0")


#################################################
# TRANSFORMATIONS
#################################################

# Some variables are not normally distributed looking at the density plot and shapiro wilk scores
# transformations that can be done are:
# * log transformation (when distribution is left sided)
# * sqrt transformation (when distribution is left sided and contains 0 values)
# * box-cox transformation (when other two do not result in better results)

# with trial and error the best transformations are shown underneath
# not every variables is transformed since it did not lead to better normal distribution

#AgeYears
trans_df <- ImputedDatasetALL

trans_df$Ageyears <- sqrt(max(trans_df$Ageyears) - trans_df$Ageyears)
shapiro.test(trans_df$Ageyears)

#"AverageserumHbA1c"
trans_df$AverageserumHbA1c <- sqrt(trans_df$AverageserumHbA1c)
shapiro.test(trans_df$AverageserumHbA1c)
plot(density(trans_df[["AverageserumHbA1c"]]), main=paste("Density Plot of", "AverageserumHbA1c"), xlab=trans_df$AverageserumHbA1c, 
     ylab="Density", col="royalblue1", lwd=2)

#"AverageserumeGFR"
trans_df$AverageserumeGFR <- sqrt(trans_df$AverageserumeGFR)
shapiro.test(trans_df$AverageserumeGFR)
plot(density(trans_df[["AverageserumeGFR"]]), main=paste("Density Plot of", "AverageserumeGFR"), xlab=trans_df$AverageserumeGFR,
     ylab="Density", col="royalblue1", lwd=2)

#"Durationofdiabetes",
trans_df$Durationofdiabetes <- sqrt(trans_df$Durationofdiabetes)
shapiro.test(trans_df$Durationofdiabetes)
plot(density(trans_df[["Durationofdiabetes"]]), main=paste("Density Plot of", "Durationofdiabetes"), xlab=trans_df$Durationofdiabetes,
     ylab="Density", col="royalblue1", lwd=2)


#"Woundareameasurement", 
trans_df$Woundareameasurement <- sqrt(trans_df$Woundareameasurement)
shapiro.test(trans_df$Woundareameasurement)
plot(density(trans_df[["Woundareameasurement"]]), main=paste("Density Plot of", "Woundareameasurement"), xlab=trans_df$Woundareameasurement,
     ylab="Density", col="royalblue1", lwd=2)


transformed_variables <- c("Ageyears", "AverageserumHbA1c", "AverageserumeGFR",
                           "Durationofdiabetes", "Woundareameasurement")


plot.new()
par(mfrow=c(2, 3), mar=c(2, 2, 2, 2))  
par(family = "serif") 
par(cex.lab = 1.5, cex.main = 1.25)

for (var in transformed_variables) {
  density_plot <- density(trans_df[[var]])
  plot(density_plot, main=paste("Density Plot of transformed variable", var), xlab=var, 
       ylab="Density", col="royalblue4", lwd=3)
}


# perform the anova and shapiro wilk again after transformations
anova_results_trans <- list()
shapiro_wilk_results_trans <- data.frame(Variable = character(),
                                   shapiro = numeric(),
                                   kruskal = numeric(),
                                   anova = numeric(),
                                   stringsAsFactors = FALSE)

# Loop over each variable
for (variable in variables_to_check_numeric) {
  print(variable)
  # Perform Shapiro-Wilk test
  shapiro_test_result <- shapiro.test(trans_df[[variable]])
  kruskal_test_result <- kruskal.test(trans_df[[variable]] ~ factor(trans_df$Target))
  
  anova_result_trans <- aov(as.formula(paste(variable, "~ Target")), data = trans_df)
  
  # Save ANOVA result
  anova_results_trans[[variable]] <- anova_result_trans
  
  # Extract F-value and p-value
  f_value <- summary(anova_result_trans)[[1]]$F[1]
  p_value <- summary(anova_result_trans)[[1]]$"Pr(>F)"[1]
  
  # Store results in dataframe
  shapiro_wilk_results_trans <- rbind(shapiro_wilk_results_trans, 
                                data.frame(Variable = variable,
                                           
                                           shapiro = shapiro_test_result$p.value,
                                           kruskal = kruskal_test_result$p.value,
                                           anova = p_value))
}
shapiro_wilk_results_trans$shapiro <- round(shapiro_wilk_results_trans$shapiro, digits = 5)
shapiro_wilk_results_trans$kruskal <- round(shapiro_wilk_results_trans$kruskal, digits = 5)
shapiro_wilk_results_trans$anova <- round(shapiro_wilk_results_trans$anova, digits = 5)
shapiro_wilk_results_trans$Is_Normal_Distributed <- ifelse(shapiro_wilk_results_trans$shapiro >= 0.05, "1", "0")

length(trans_df$Ageyears)
length(trans_df$Target)

anova_results <- list()
anova_df <- data.frame(Variable = character(), F_Value = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
anova_df$P_Value <- round(anova_df$P_Value, digits = 5)

