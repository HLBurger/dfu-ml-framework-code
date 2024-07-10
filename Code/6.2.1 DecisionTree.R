# DecisionTreeClassifier without tuned hyperparameters

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(rpart)


num_folds <- 1

accuracy_total_DT <- auc_score_class0_total_DT <- auc_score_class1_total_DT <- auc_score_class2_total_DT <- rep(0,num_folds)
precision_total_DT <- recall_total_DT <- auc_scores_total_DT <- vector("list", 3)
for (i in 1:3) {
  precision_total_DT[[i]] <- recall_total_DT[[i]] <- auc_scores_total_DT[[i]] <- rep(0, num_folds)
}


df_class0_DT <- df_class1_DT <- df_class2_DT <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_DT <- calibration_curve_class1_DT <- calibration_curve_class2_DT <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_DT <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(feature_importance_DT) <- colnames(df)
feature_importance_DT <- feature_importance_DT[, !colnames(feature_importance_DT) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

# Perform cross-validation
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data, 1)
  table(train_data$Target)
  
  model_DT <- rpart(Target ~ ., data = train_data, method = "class")
  
  # Predictions
  predictions_DT <- predict(model_DT, newdata = test_set, type="class")
  predicted_probabilities_DT <- predict(model_DT, newdata = test_set, type = "prob")
  
  # Confusion matrix
  conf_matrix_DT <- table(test_set$Target, predictions_DT)
  
  # Accuracy, precision and recall
  accuracy_total_DT[i] <- sum(diag(conf_matrix_DT)) / sum(conf_matrix_DT)
  precision <- diag(conf_matrix_DT) / rowSums(conf_matrix_DT)
  recall <- diag(conf_matrix_DT) / colSums(conf_matrix_DT)
  
  # predicted probabilities
  class_probs_DT <- as.data.frame(predicted_probabilities_DT)
  class0_probs_DT <- class_probs_DT[, "0"]
  class1_probs_DT <- class_probs_DT[, "1"]
  class2_probs_DT <- class_probs_DT[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_DT <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_DT)
  roc_curve_class1_DT <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_DT)
  roc_curve_class2_DT <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_DT)
  
  # AUC score
  auc_score_class0_total_DT[i] <- auc(roc_curve_class0_DT)
  auc_score_class1_total_DT[i] <- auc(roc_curve_class1_DT)
  auc_score_class2_total_DT[i] <- auc(roc_curve_class2_DT)
  
  df_class0_DT <- rbind(df_class0_DT, data.frame(sensitivity = roc_curve_class0_DT$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_DT$specificities))
  df_class1_DT <- rbind(df_class1_DT, data.frame(sensitivity = roc_curve_class1_DT$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_DT$specificities))
  df_class2_DT <- rbind(df_class2_DT, data.frame(sensitivity = roc_curve_class2_DT$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_DT$specificities))
  
  calibration_curve_class0_DT <- rbind(calibration_curve_class0_DT, data.frame(predicted_prob = class_probs_DT[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_DT <- rbind(calibration_curve_class1_DT, data.frame(predicted_prob = class_probs_DT[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_DT <- rbind(calibration_curve_class2_DT, data.frame(predicted_prob = class_probs_DT[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_DT[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_DT[[class_val]][i] <- recall[class_val]
  }
  
  feature_importance <- model_DT$variable.importance
  feature_names <- names(feature_importance)
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_DT), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_DT)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- feature_importance[col_name]
    }
  }
  feature_importance_DT <- rbind(feature_importance_DT, temp_df)

}

##########################################################
# Metrics
##########################################################
average_accuracy_DT <- mean(unlist(accuracy_total_DT))
average_auc0_DT <- mean(unlist(auc_score_class0_total_DT))
average_auc1_DT <- mean(unlist(auc_score_class1_total_DT))
average_auc2_DT <- mean(unlist(auc_score_class2_total_DT))
average_precision_DT <- sapply(precision_total_DT, mean)
average_recall_DT <- sapply(recall_total_DT, mean)

# Brier score
brier_score_class0_DT <- mean((calibration_curve_class0_DT$predicted_prob - calibration_curve_class0_DT$observed_outcome)^2)
brier_score_class1_DT <- mean((calibration_curve_class1_DT$predicted_prob - calibration_curve_class1_DT$observed_outcome)^2)
brier_score_class2_DT <- mean((calibration_curve_class2_DT$predicted_prob - calibration_curve_class2_DT$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_DT, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_DT, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_DT, 3)))
print(average_accuracy_DT)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_DT, 3)))
print(paste("Recall:", round(average_recall_DT[1], 3)))
print(paste("Precision:", round(average_precision_DT[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_DT, 3)))
print(paste("Recall:", round(average_recall_DT[2], 3)))
print(paste("Precision:", round(average_precision_DT[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_DT, 3)))
print(paste("Recall:", round(average_recall_DT[3], 3)))
print(paste("Precision:", round(average_precision_DT[3], 3)))

# metrics_df_new[["DecisionTree"]] <- c(
#   average_accuracy_DT,
#   brier_score_class0_DT, average_auc0_DT, average_recall_DT[1], average_precision_DT[1],
#   brier_score_class1_DT, average_auc1_DT, average_recall_DT[2], average_precision_DT[2],
#   brier_score_class2_DT, average_auc2_DT, average_recall_DT[3], average_precision_DT[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_DT, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_DT, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_DT, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_DT, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_DT, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_DT, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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

feature_importance_long <- gather(feature_importance_DT, key = "Feature", value = "Importance")

feature_importance_DT <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_DT_H, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Decision Tree Model"),
       x = "",
       y = "Importance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif")
        )+
  coord_flip() 


