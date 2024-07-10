#RandomForestClassifier

library(randomForest)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)


num_folds <- 1

accuracy_total_RF <- auc_score_class0_total_RF <- auc_score_class1_total_RF <- auc_score_class2_total_RF <- rep(0,num_folds)
precision_total_RF <- recall_total_RF <- auc_scores_total_RF <- vector("list", 3)
for (i in 1:3) {
  precision_total_RF[[i]] <- recall_total_RF[[i]] <- auc_scores_total_RF[[i]] <- rep(0, num_folds)
}


df_class0_RF <- df_class1_RF <- df_class2_RF <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_RF <- calibration_curve_class1_RF <- calibration_curve_class2_RF <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_RF <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(feature_importance_RF) <- colnames(df)
feature_importance_RF <- feature_importance_RF[, !colnames(feature_importance_RF) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


# PeRForm cross-validation
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data, 1)
  
  model_RF <- randomForest(`Target` ~ ., data = train_data)
  
  # Predictions
  predictions_RF <- predict(model_RF, newdata = test_set)
  predicted_probabilities_RF <- predict(model_RF, newdata = test_set, type = "prob")
  
  # Confusion matrix
  conf_matrix_RF <- table(test_set$Target, predictions_RF)
  
  # Accuracy
  accuracy_total_RF[i] <- sum(diag(conf_matrix_RF)) / sum(conf_matrix_RF)
  precision <- diag(conf_matrix_RF) / rowSums(conf_matrix_RF)
  recall <- diag(conf_matrix_RF) / colSums(conf_matrix_RF)
  
  class_probs_RF <- as.data.frame(predicted_probabilities_RF)
  class0_probs_RF <- class_probs_RF[, "0"]
  class1_probs_RF <- class_probs_RF[, "1"]
  class2_probs_RF <- class_probs_RF[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_RF <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_RF)
  roc_curve_class1_RF <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_RF)
  roc_curve_class2_RF <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_RF)
  
  # AUC score
  
  auc_score_class0_total_RF[i] <- auc(roc_curve_class0_RF)
  auc_score_class1_total_RF[i] <- auc(roc_curve_class1_RF)
  auc_score_class2_total_RF[i] <- auc(roc_curve_class2_RF)
  
  df_class0_RF <- rbind(df_class0_RF, data.frame(sensitivity = roc_curve_class0_RF$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_RF$specificities))
  df_class1_RF <- rbind(df_class1_RF, data.frame(sensitivity = roc_curve_class1_RF$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_RF$specificities))
  df_class2_RF <- rbind(df_class2_RF, data.frame(sensitivity = roc_curve_class2_RF$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_RF$specificities))
  
  calibration_curve_class0_RF <- rbind(calibration_curve_class0_RF, data.frame(predicted_prob = class_probs_RF[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_RF <- rbind(calibration_curve_class1_RF, data.frame(predicted_prob = class_probs_RF[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_RF <- rbind(calibration_curve_class2_RF, data.frame(predicted_prob = class_probs_RF[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_RF[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_RF[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- importance(model_RF)
  feature_names <- rownames(feature_importance)
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_RF), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_RF)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- feature_importance[i]
    }
  }
  feature_importance_RF <- rbind(feature_importance_RF, temp_df)
}
##########################################################
# metrics
##########################################################
average_accuracy_RF <- mean(unlist(accuracy_total_RF))
average_auc0_RF <- mean(unlist(auc_score_class0_total_RF))
average_auc1_RF <- mean(unlist(auc_score_class1_total_RF))
average_auc2_RF <- mean(unlist(auc_score_class2_total_RF))
average_precision_RF <- sapply(precision_total_RF, mean)
average_recall_RF <- sapply(recall_total_RF, mean)

# Brier score
brier_score_class0_RF <- mean((calibration_curve_class0_RF$predicted_prob - calibration_curve_class0_RF$observed_outcome)^2)
brier_score_class1_RF <- mean((calibration_curve_class1_RF$predicted_prob - calibration_curve_class1_RF$observed_outcome)^2)
brier_score_class2_RF <- mean((calibration_curve_class2_RF$predicted_prob - calibration_curve_class2_RF$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_RF, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_RF, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_RF, 3)))

print(average_accuracy_RF)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_RF, 3)))
print(paste("Recall:", round(average_recall_RF[1], 3)))
print(paste("Precision:", round(average_precision_RF[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_RF, 3)))
print(paste("Recall:", round(average_recall_RF[2], 3)))
print(paste("Precision:", round(average_precision_RF[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_RF, 3)))
print(paste("Recall:", round(average_recall_RF[3], 3)))
print(paste("Precision:", round(average_precision_RF[3], 3)))

# 
# metrics_df_new[["RandomForest"]] <- c(
#   average_accuracy_RF, 
#   brier_score_class0_RF, average_auc0_RF, average_recall_RF[1], average_precision_RF[1], 
#   brier_score_class1_RF, average_auc1_RF, average_recall_RF[2], average_precision_RF[2],
#   brier_score_class2_RF, average_auc2_RF, average_recall_RF[3], average_precision_RF[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_RF, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_RF, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_RF, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_RF, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_RF, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_RF, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
  labs(title = "Calibration Curves of Logistic Regression model",
       x = "Predicted Probability",
       y = "Observed Outcome",
       color = "Class") +
  scale_color_manual(values = c("No amputation" = "royalblue1", "Amputation" = "royalblue4", "Uncertain" = "darkgrey")) +
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
feature_importance_long <- gather(feature_importance_RF, key = "Feature", value = "Importance")

feature_importance_RF <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_RF, aes(x = reorder(Feature, Importance), y = Importance)) +
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




