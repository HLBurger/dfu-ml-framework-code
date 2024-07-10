#RandomForestClassifier predicting amputation level

library(randomForest)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(pROC)
library(themis)
library(ROSE)

dflevel <- ImputedDatasetALL[c("Ageyears", "COPD","Kidneydialysis", "Hypertension","HeartFailure", "CKD4",
                          "AverageserumHb", "AverageserumeGFR", "Averagediastolicbloodpressure",
                          "TBI", "Toepressureaffectedside","Woundlocation_Anterior_Tibial_Artery",
                          "Woundlocation_Dorsalis_Pedis", "WoundInfection",
                          "Woundlocation_Lateral_Plantar", "Woundlocation_Medial_Plantar",
                          "Texas2", "Texas3", "TexasB", "TexasC", "TexasD", "Neuropathic",
                          "Degreeofsloughmeasurement", "Degreeofnecrotictissuemeasurement",
                          "Woundareameasurement", "LowerOcclusion","UpperOcclusion","LowerStenosis",
                          "UpperStenosis","TargetLevel")]
# Convert Target column to factor with three levels
dflevel$TargetLevel <- factor(dflevel$TargetLevel, levels = c(0, 1, 2, 3, 4))


num_folds <- 5

accuracy_total_RF_Level <- auc_score_class0_total_RF_Level <- auc_score_class1_total_RF_Level <- auc_score_class2_total_RF_Level <- auc_score_class3_total_RF_Level <-auc_score_class4_total_RF_Level <-rep(0,num_folds)
precision_total_RF_Level <- recall_total_RF_Level <- auc_scores_total_RF_Level <- vector("list", 5)
for (i in 1:5) {
  precision_total_RF_Level[[i]] <- recall_total_RF_Level[[i]] <- auc_scores_total_RF_Level[[i]] <- rep(0, num_folds)
}


dflevel_class0_RF_Level <- dflevel_class1_RF_Level <- dflevel_class2_RF_Level <- dflevel_class3_RF_Level <- dflevel_class4_RF_Level <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_RF_Level <- calibration_curve_class1_RF_Level <- calibration_curve_class2_RF_Level <- calibration_curve_class3_RF_Level <- calibration_curve_class4_RF_Level <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_RF_Level <- data.frame(matrix(ncol = ncol(dflevel), nrow = 0))
colnames(feature_importance_RF_Level) <- colnames(dflevel)
feature_importance_RF_Level <- feature_importance_RF_Level[, !colnames(feature_importance_RF_Level) %in% c("TargetLevel")]


folds <- createFolds(dflevel$TargetLevel, k = num_folds, list = TRUE, returnTrain = FALSE)


#  cross-validation
for (i in 1:num_folds) {
  # Split data into train and test sets based on fold
  train_index <- unlist(folds[-i])
  train_data <- dflevel[train_index, ]
  test_data <- dflevel[folds[[i]], ]
  
  train_data <- process_data_level(train_data, 1)
  
  model_RF_Level <- randomForest(`TargetLevel` ~ ., data = train_data)
  
  # Predictions
  predictions_RF_Level <- predict(model_RF_Level, newdata = test_data)
  predicted_probabilities_RF_Level <- predict(model_RF_Level, newdata = test_data, type = "prob")
  
  # Confusion matrix
  conf_matrix_RF_Level <- table(test_data$TargetLevel, predictions_RF_Level)
  
  # Accuracy
  accuracy_total_RF_Level[i] <- sum(diag(conf_matrix_RF_Level)) / sum(conf_matrix_RF_Level)
  precision <- diag(conf_matrix_RF_Level) / rowSums(conf_matrix_RF_Level)
  recall <- diag(conf_matrix_RF_Level) / colSums(conf_matrix_RF_Level)
  
  class_probs_RF_Level <- as.data.frame(predicted_probabilities_RF_Level)
  class0_probs_RF_Level <- class_probs_RF_Level[, "0"]
  class1_probs_RF_Level <- class_probs_RF_Level[, "1"]
  class2_probs_RF_Level <- class_probs_RF_Level[, "2"]
  class3_probs_RF_Level <- class_probs_RF_Level[, "3"]
  class4_probs_RF_Level <- class_probs_RF_Level[, "4"]
  
  test_data$TargetLevel <- factor(test_data$TargetLevel, levels = c("0", "1", "2","3","4"))
  
  # ROC curves
  roc_curve_class0_RF_Level <- roc(ifelse(test_data$TargetLevel == "0", 1, 0), class0_probs_RF_Level)
  roc_curve_class1_RF_Level <- roc(ifelse(test_data$TargetLevel == "1", 1, 0), class1_probs_RF_Level)
  roc_curve_class2_RF_Level <- roc(ifelse(test_data$TargetLevel == "2", 1, 0), class2_probs_RF_Level)
  roc_curve_class3_RF_Level <- roc(ifelse(test_data$TargetLevel == "3", 1, 0), class3_probs_RF_Level)
  roc_curve_class4_RF_Level <- roc(ifelse(test_data$TargetLevel == "4", 1, 0), class4_probs_RF_Level)
  
  # AUC score
  
  auc_score_class0_total_RF_Level[i] <- auc(roc_curve_class0_RF_Level)
  auc_score_class1_total_RF_Level[i] <- auc(roc_curve_class1_RF_Level)
  auc_score_class2_total_RF_Level[i] <- auc(roc_curve_class2_RF_Level)
  auc_score_class3_total_RF_Level[i] <- auc(roc_curve_class3_RF_Level)
  auc_score_class4_total_RF_Level[i] <- auc(roc_curve_class4_RF_Level)
  
  dflevel_class0_RF_Level <- rbind(dflevel_class0_RF_Level, data.frame(sensitivity = roc_curve_class0_RF_Level$sensitivities, 
                                                             specificity = 1 - roc_curve_class0_RF_Level$specificities))
  dflevel_class1_RF_Level <- rbind(dflevel_class1_RF_Level, data.frame(sensitivity = roc_curve_class1_RF_Level$sensitivities, 
                                                             specificity = 1 - roc_curve_class1_RF_Level$specificities))
  dflevel_class2_RF_Level <- rbind(dflevel_class2_RF_Level, data.frame(sensitivity = roc_curve_class2_RF_Level$sensitivities, 
                                                             specificity = 1 - roc_curve_class2_RF_Level$specificities))
  dflevel_class3_RF_Level <- rbind(dflevel_class3_RF_Level, data.frame(sensitivity = roc_curve_class3_RF_Level$sensitivities, 
                                                             specificity = 1 - roc_curve_class3_RF_Level$specificities))
  dflevel_class4_RF_Level <- rbind(dflevel_class4_RF_Level, data.frame(sensitivity = roc_curve_class4_RF_Level$sensitivities, 
                                                             specificity = 1 - roc_curve_class4_RF_Level$specificities))
  
  calibration_curve_class0_RF_Level <- rbind(calibration_curve_class0_RF_Level, data.frame(predicted_prob = class_probs_RF_Level[, "0"], 
                                                                                           observed_outcome = ifelse(test_data$TargetLevel == "0", 1, 0)))
  calibration_curve_class1_RF_Level <- rbind(calibration_curve_class1_RF_Level, data.frame(predicted_prob = class_probs_RF_Level[, "1"], 
                                                                                           observed_outcome = ifelse(test_data$TargetLevel == "1", 1, 0)))
  calibration_curve_class2_RF_Level <- rbind(calibration_curve_class2_RF_Level, data.frame(predicted_prob = class_probs_RF_Level[, "2"], 
                                                                                           observed_outcome = ifelse(test_data$TargetLevel == "2", 1, 0)))
  calibration_curve_class3_RF_Level <- rbind(calibration_curve_class3_RF_Level, data.frame(predicted_prob = class_probs_RF_Level[, "3"], 
                                                                                           observed_outcome = ifelse(test_data$TargetLevel == "3", 1, 0)))
  calibration_curve_class4_RF_Level <- rbind(calibration_curve_class4_RF_Level, data.frame(predicted_prob = class_probs_RF_Level[, "4"], 
                                                                                           observed_outcome = ifelse(test_data$TargetLevel == "4", 1, 0)))
  
  for (class_val in 1:5) {
    # Precision
    precision_total_RF_Level[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_RF_Level[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- importance(model_RF_Level)
  feature_names <- rownames(feature_importance)
  
  temp_dflevel <- data.frame(matrix(0, ncol = ncol(feature_importance_RF_Level), nrow = 1))
  colnames(temp_dflevel) <- colnames(feature_importance_RF_Level)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_dflevel)) {
      temp_dflevel[1, col_name] <- feature_importance[i]
    }
  }
  feature_importance_RF_Level <- rbind(feature_importance_RF_Level, temp_dflevel)
}
##########################################################
# metrics
##########################################################
average_accuracy_RF_Level <- mean(unlist(accuracy_total_RF_Level))
average_auc0_RF_Level <- mean(unlist(auc_score_class0_total_RF_Level))
average_auc1_RF_Level <- mean(unlist(auc_score_class1_total_RF_Level))
average_auc2_RF_Level <- mean(unlist(auc_score_class2_total_RF_Level))
average_auc3_RF_Level <- mean(unlist(auc_score_class3_total_RF_Level))
average_auc4_RF_Level <- mean(unlist(auc_score_class4_total_RF_Level))
average_precision_RF_Level <- sapply(precision_total_RF_Level, mean)
average_recall_RF_Level <- sapply(recall_total_RF_Level, mean)

# Brier score
brier_score_class0_RF_Level <- mean((calibration_curve_class0_RF_Level$predicted_prob - calibration_curve_class0_RF_Level$observed_outcome)^2)
brier_score_class1_RF_Level <- mean((calibration_curve_class1_RF_Level$predicted_prob - calibration_curve_class1_RF_Level$observed_outcome)^2)
brier_score_class2_RF_Level <- mean((calibration_curve_class2_RF_Level$predicted_prob - calibration_curve_class2_RF_Level$observed_outcome)^2)
brier_score_class3_RF_Level <- mean((calibration_curve_class3_RF_Level$predicted_prob - calibration_curve_class2_RF_Level$observed_outcome)^2)
brier_score_class4_RF_Level <- mean((calibration_curve_class4_RF_Level$predicted_prob - calibration_curve_class2_RF_Level$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_RF_Level, 3)))
print(paste("Brier Score minor certain:", round(brier_score_class1_RF_Level, 3)))
print(paste("Brier Score minor uncertain:", round(brier_score_class2_RF_Level, 3)))
print(paste("Brier Score major:", round(brier_score_class3_RF_Level, 3)))
print(paste("Brier Score no wound healing:", round(brier_score_class4_RF_Level, 3)))

print(average_accuracy_RF_Level)
print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc0_RF_Level, 3)))
print(paste("Recall:", round(average_recall_RF_Level[1], 3)))
print(paste("Precision:", round(average_precision_RF_Level[1], 3)))

print("Scores of minor certain prediction")
print(paste("AUC:", round(average_auc1_RF_Level, 3)))
print(paste("Recall:", round(average_recall_RF_Level[2], 3)))
print(paste("Precision:", round(average_precision_RF_Level[2], 3)))

print("Scores of minor uncertain prediction")
print(paste("AUC:", round(average_auc2_RF_Level, 3)))
print(paste("Recall:", round(average_recall_RF_Level[3], 3)))
print(paste("Precision:", round(average_precision_RF_Level[3], 3)))

print("Scores of major prediction")
print(paste("AUC:", round(average_auc3_RF_Level, 3)))
print(paste("Recall:", round(average_recall_RF_Level[4], 3)))
print(paste("Precision:", round(average_precision_RF_Level[4], 3)))

print("Scores of no wound healing prediction")
print(paste("AUC:", round(average_auc4_RF_Level, 3)))
print(paste("Recall:", round(average_recall_RF_Level[5], 3)))
print(paste("Precision:", round(average_precision_RF_Level[5], 3)))

# 
# metrics_df_new[["RandomForestLevels"]] <- c(
#   average_accuracy_RF_Level, 
#   brier_score_class0_RF_Level, average_auc0_RF_Level, average_recall_RF_Level[1], average_precision_RF_Level[1], 
#   brier_score_class1_RF_Level, average_auc1_RF_Level, average_recall_RF_Level[2], average_precision_RF_Level[2],
#   brier_score_class2_RF_Level, average_auc2_RF_Level, average_recall_RF_Level[3], average_precision_RF_Level[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = dflevel_class0_RF_Level, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = dflevel_class1_RF_Level, aes(x = specificity, y = sensitivity, color = "Minor certain"), se = FALSE) +
  geom_smooth(data = dflevel_class2_RF_Level, aes(x = specificity, y = sensitivity, color = "Minor uncertain"), se = FALSE) +
  geom_smooth(data = dflevel_class3_RF_Level, aes(x = specificity, y = sensitivity, color = "Major"), se = FALSE) +
  geom_smooth(data = dflevel_class4_RF_Level, aes(x = specificity, y = sensitivity, color = "No wound healing"), se = FALSE) +
  labs(title = "ROC curve of Random Forest model",
       x = "False Positive Rate",
       y = "True Positive Rate",
       color = "Class") +
  scale_color_manual(values = c("No amputation" = "#BDEAB6", 
                                "Minor certain" = "#5CCFBE", 
                                "Minor uncertain" = "#0094C1",
                                "Major" = "#0060A8",
                                "No wound healing"= "#1A3493")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
        legend.text = element_text(size = 16, family="serif"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14, family="serif"),  
        axis.text.y = element_text(size = 14, family="serif"),
        axis.title.y = element_text(size = 16, family="serif"),
        axis.title.x = element_text(size = 16, family="serif"),
        legend.position = "top")

##########################################################
# Calibration plot
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = calibration_curve_class0_RF_Level, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_RF_Level, aes(x = predicted_prob, y = observed_outcome, color = "Minor certain"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_RF_Level, aes(x = predicted_prob, y = observed_outcome, color = "Minor uncertain"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class3_RF_Level, aes(x = predicted_prob, y = observed_outcome, color = "Major"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class4_RF_Level, aes(x = predicted_prob, y = observed_outcome, color = "No wound healing"), method = "loess", se = FALSE) +
  labs(title = "Calibration Curves of Random Forest model",
       x = "Predicted Probability",
       y = "Observed Outcome",
       color = "Class") +
  scale_color_manual(values = c("No amputation" = "#BDEAB6", 
                                "Minor certain" = "#5CCFBE", 
                                "Minor uncertain" = "#0094C1",
                                "Major" = "#0060A8",
                                "No wound healing"= "#1A3493")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"))



##########################################################
#feature importance
##########################################################
feature_importance_long <- gather(feature_importance_RF_Level, key = "Feature", value = "Importance")

feature_importance_RF_Level <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_RF_Level, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of the Random Forest Model"),
       x = "",
       y = "Mean Decrease in Gini score") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif")
  )+
  coord_flip() 


#Feature importance boxplot
Q1 <- quantile(feature_importance_RF_Level$Importance, 0.25)
Q3 <- quantile(feature_importance_RF_Level$Importance, 0.75)
IQR <- Q3 - Q1

# Calculate the upper bound
upper_bound <- Q3 + 1.5 * IQR
upper_bound
upperbound <- feature_importance_RF_Level %>%
  filter(Importance > upper_bound)
upperbound

Q3
Q3 <- feature_importance_RF_Level %>%
  filter(Importance > Q3)
Q3


