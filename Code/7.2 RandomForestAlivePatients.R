#RandomForestClassifier with only alive patients
library(randomForest)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(pROC)

dfAlive <- ImputedDatasetUndead[c("Ageyears", "COPD","Kidneydialysis", "HeartFailure",
                            "Type1", "IDDM", "AverageserumHb", "AverageserumeGFR", "Averagediastolicbloodpressure",
                            "TBI", "Toepressureaffectedside","Woundlocation_Anterior_Tibial_Artery",
                            "Woundlocation_Dorsalis_Pedis", "Woundlocation_Lateral_Calcaneal",
                            "Woundlocation_Lateral_Plantar", "Woundlocation_Medial_Calcaneal", "Woundlocation_Medial_Plantar",
                            "Woundlocation_Posterior_Tibial_Artery", 
                            "Texas2", "Texas3", "TexasB", "TexasC", "TexasD", "Neuropathic",
                            "Degreeofsloughmeasurement", "Degreeofnecrotictissuemeasurement",
                            "Woundareameasurement", "LowerOcclusion","UpperOcclusion","LowerStenosis",
                            "Target")]


# Convert Target column to factor with three levels
dfAlive$Target <- factor(dfAlive$Target, levels = c(0, 1, 2))


num_folds <- 5

accuracy_total_RF_Undead <- auc_score_class0_total_RF_Undead <- auc_score_class1_total_RF_Undead <- auc_score_class2_total_RF_Undead <- rep(0,num_folds)
precision_total_RF_Undead <- recall_total_RF_Undead <- auc_scores_total_RF_Undead <- vector("list", 3)
for (i in 1:3) {
  precision_total_RF_Undead[[i]] <- recall_total_RF_Undead[[i]] <- auc_scores_total_RF_Undead[[i]] <- rep(0, num_folds)
}


dfAlive_class0_RF_Undead <- dfAlive_class1_RF_Undead <- dfAlive_class2_RF_Undead <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_RF_Undead <- calibration_curve_class1_RF_Undead <- calibration_curve_class2_RF_Undead <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_RF_Undead <- data.frame(matrix(ncol = ncol(dfAlive), nrow = 0))
colnames(feature_importance_RF_Undead) <- colnames(dfAlive)
feature_importance_RF_Undead <- feature_importance_RF_Undead[, !colnames(feature_importance_RF_Undead) %in% c("Target")]


folds <- createFolds(dfAlive$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


#  cross-validation
for (i in 1:num_folds) {
  # Split data into train and test sets based on fold
  train_index <- unlist(folds[-i])
  train_data <- dfAlive[train_index, ]
  test_data <- dfAlive[folds[[i]], ]
  
  train_data <- process_data(train_data, 1)
  
  model_RF_Undead <- randomForest(`Target` ~ ., data = train_data)
  
  # Predictions
  predictions_RF_Undead <- predict(model_RF_Undead, newdata = test_data)
  predicted_probabilities_RF_Undead <- predict(model_RF_Undead, newdata = test_data, type = "prob")
  
  # Confusion matrix
  conf_matrix_RF_Undead <- table(test_data$Target, predictions_RF_Undead)
  
  # Accuracy
  accuracy_total_RF_Undead[i] <- sum(diag(conf_matrix_RF_Undead)) / sum(conf_matrix_RF_Undead)
  precision <- diag(conf_matrix_RF_Undead) / rowSums(conf_matrix_RF_Undead)
  recall <- diag(conf_matrix_RF_Undead) / colSums(conf_matrix_RF_Undead)
  
  class_probs_RF_Undead <- as.data.frame(predicted_probabilities_RF_Undead)
  class0_probs_RF_Undead <- class_probs_RF_Undead[, "0"]
  class1_probs_RF_Undead <- class_probs_RF_Undead[, "1"]
  class2_probs_RF_Undead <- class_probs_RF_Undead[, "2"]
  
  test_data$Target <- factor(test_data$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_RF_Undead <- roc(ifelse(test_data$Target == "0", 1, 0), class0_probs_RF_Undead)
  roc_curve_class1_RF_Undead <- roc(ifelse(test_data$Target == "1", 1, 0), class1_probs_RF_Undead)
  roc_curve_class2_RF_Undead <- roc(ifelse(test_data$Target == "2", 1, 0), class2_probs_RF_Undead)
  
  # AUC score
  
  auc_score_class0_total_RF_Undead[i] <- auc(roc_curve_class0_RF_Undead)
  auc_score_class1_total_RF_Undead[i] <- auc(roc_curve_class1_RF_Undead)
  auc_score_class2_total_RF_Undead[i] <- auc(roc_curve_class2_RF_Undead)
  
  dfAlive_class0_RF_Undead <- rbind(dfAlive_class0_RF_Undead, data.frame(sensitivity = roc_curve_class0_RF_Undead$sensitivities, 
                                                             specificity = 1 - roc_curve_class0_RF_Undead$specificities))
  dfAlive_class1_RF_Undead <- rbind(dfAlive_class1_RF_Undead, data.frame(sensitivity = roc_curve_class1_RF_Undead$sensitivities, 
                                                             specificity = 1 - roc_curve_class1_RF_Undead$specificities))
  dfAlive_class2_RF_Undead <- rbind(dfAlive_class2_RF_Undead, data.frame(sensitivity = roc_curve_class2_RF_Undead$sensitivities, 
                                                             specificity = 1 - roc_curve_class2_RF_Undead$specificities))
  
  calibration_curve_class0_RF_Undead <- rbind(calibration_curve_class0_RF_Undead, data.frame(predicted_prob = class_probs_RF_Undead[, "0"], 
                                                                                           observed_outcome = ifelse(test_data$Target == "0", 1, 0)))
  calibration_curve_class1_RF_Undead <- rbind(calibration_curve_class1_RF_Undead, data.frame(predicted_prob = class_probs_RF_Undead[, "1"], 
                                                                                           observed_outcome = ifelse(test_data$Target == "1", 1, 0)))
  calibration_curve_class2_RF_Undead <- rbind(calibration_curve_class2_RF_Undead, data.frame(predicted_prob = class_probs_RF_Undead[, "2"], 
                                                                                           observed_outcome = ifelse(test_data$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_RF_Undead[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_RF_Undead[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- importance(model_RF_Undead)
  feature_names <- rownames(feature_importance)
  
  temp_dfAlive <- data.frame(matrix(0, ncol = ncol(feature_importance_RF_Undead), nrow = 1))
  colnames(temp_dfAlive) <- colnames(feature_importance_RF_Undead)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_dfAlive)) {
      temp_dfAlive[1, col_name] <- feature_importance[i]
    }
  }
  feature_importance_RF_Undead <- rbind(feature_importance_RF_Undead, temp_dfAlive)
}
##########################################################
# metrics
##########################################################
average_accuracy_RF_Undead <- mean(unlist(accuracy_total_RF_Undead))
average_auc0_RF_Undead <- mean(unlist(auc_score_class0_total_RF_Undead))
average_auc1_RF_Undead <- mean(unlist(auc_score_class1_total_RF_Undead))
average_auc2_RF_Undead <- mean(unlist(auc_score_class2_total_RF_Undead))
average_precision_RF_Undead <- sapply(precision_total_RF_Undead, mean)
average_recall_RF_Undead <- sapply(recall_total_RF_Undead, mean)

# Brier score
brier_score_class0_RF_Undead <- mean((calibration_curve_class0_RF_Undead$predicted_prob - calibration_curve_class0_RF_Undead$observed_outcome)^2)
brier_score_class1_RF_Undead <- mean((calibration_curve_class1_RF_Undead$predicted_prob - calibration_curve_class1_RF_Undead$observed_outcome)^2)
brier_score_class2_RF_Undead <- mean((calibration_curve_class2_RF_Undead$predicted_prob - calibration_curve_class2_RF_Undead$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_RF_Undead, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_RF_Undead, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_RF_Undead, 3)))

print(average_accuracy_RF_Undead)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_RF_Undead, 3)))
print(paste("Recall:", round(average_recall_RF_Undead[1], 3)))
print(paste("Precision:", round(average_precision_RF_Undead[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_RF_Undead, 3)))
print(paste("Recall:", round(average_recall_RF_Undead[2], 3)))
print(paste("Precision:", round(average_precision_RF_Undead[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_RF_Undead, 3)))
print(paste("Recall:", round(average_recall_RF_Undead[3], 3)))
print(paste("Precision:", round(average_precision_RF_Undead[3], 3)))

# 
# metrics_df_new[["RandomForestAlive"]] <- c(
#   average_accuracy_RF_Undead, 
#   brier_score_class0_RF_Undead, average_auc0_RF_Undead, average_recall_RF_Undead[1], average_precision_RF_Undead[1], 
#   brier_score_class1_RF_Undead, average_auc1_RF_Undead, average_recall_RF_Undead[2], average_precision_RF_Undead[2],
#   brier_score_class2_RF_Undead, average_auc2_RF_Undead, average_recall_RF_Undead[3], average_precision_RF_Undead[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = dfAlive_class0_RF_Undead, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = dfAlive_class1_RF_Undead, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = dfAlive_class2_RF_Undead, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
  labs(title = "ROC curve of Logistic Regression model",
       x = "False Positive Rate",
       y = "True Positive Rate",
       color = "Class") +
  scale_color_manual(values = c("No amputation" = "royalblue1", "Amputation" = "royalblue4", "Uncertain" = "darkgrey")) +
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
  geom_smooth(data = calibration_curve_class0_RF_Undead, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_RF_Undead, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_RF_Undead, aes(x = predicted_prob, y = observed_outcome, color = "No wound healing"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class0_RF_Final, aes(x = predicted_prob, y = observed_outcome, color = "No amputation Original"), method = "loess", se = FALSE, linetype = "dashed") +
  geom_smooth(data = calibration_curve_class1_RF_Final, aes(x = predicted_prob, y = observed_outcome, color = "Amputation Original"), method = "loess", se = FALSE, linetype = "dashed") +
  geom_smooth(data = calibration_curve_class2_RF_Final, aes(x = predicted_prob, y = observed_outcome, color = "No wound healing Original"), method = "loess", se = FALSE, linetype = "dashed") +
  labs(title = "Calibration Curves of Random Forest model",
       x = "Predicted Probability",
       y = "Observed Outcome",
       color = "Class") +
  scale_color_manual(values = c("No amputation" = "royalblue1",
                                "Amputation" = "royalblue4",
                                "No wound healing" = "darkgrey",
                                "No amputation Original" = "royalblue1",
                                "Amputation Original" = "royalblue4",
                                "No wound healing Original" = "darkgrey" ),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "twodash", "solid", "twodash", "solid","twodash")))) +
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
feature_importance_long <- gather(feature_importance_RF_Undead, key = "Feature", value = "Importance")

feature_importance_RF_Undead <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_RF_Undead, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Decision Tree Model"),
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



################### Better Upper bound
Q1 <- quantile(feature_importance_RF_Undead_H$Importance, 0.25)
Q3 <- quantile(feature_importance_RF_Undead_H$Importance, 0.75)
IQR <- Q3 - Q1

# Calculate the upper bound
upper_bound <- Q3 + 1.5 * IQR
upper_bound
upperbound <- feature_importance_RF_Undead_H %>%
  filter(Importance > upper_bound)
upperbound

Q3
Q3 <- feature_importance_RF_Undead_H %>%
  filter(Importance > Q3)
Q3


