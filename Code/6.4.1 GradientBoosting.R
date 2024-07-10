# Gradient Boosting

library(xgboost)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)


num_folds <- 1

accuracy_total_GB <- auc_score_class0_total_GB <- auc_score_class1_total_GB <- auc_score_class2_total_GB <- rep(0,num_folds)
precision_total_GB <- recall_total_GB <- auc_scores_total_GB <- vector("list", 3)
for (i in 1:3) {
  precision_total_GB[[i]] <- recall_total_GB[[i]] <- auc_scores_total_GB[[i]] <- rep(0, num_folds)
}


df_class0_GB <- df_class1_GB <- df_class2_GB <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_GB <- calibration_curve_class1_GB <- calibration_curve_class2_GB <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_GB <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(feature_importance_GB) <- colnames(df)
feature_importance_GB <- feature_importance_GB[, !colnames(feature_importance_GB) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

# cross-validation
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data,1)
  train_data$Target <- factor(train_data$Target, levels = c(0,1,2))
  train_data$Target <- as.numeric(as.character(train_data$Target))
  
  model_GB <- xgboost(data = as.matrix(train_data[, -ncol(train_data)]), 
                      nrounds = 100, 
                      label = train_data$Target,
                      objective = "multi:softmax", 
                      num_class = 3)
  
  model_prob_GB <- xgboost(data = as.matrix(train_data[, -ncol(train_data)]), 
                      nrounds = 100, 
                      label = train_data$Target,
                      objective = "multi:softprob", 
                      num_class = 3)
  

  # Predictions
  predictions_GB <- predict(model_GB, as.matrix(test_set[, -ncol(test_set)]), type="prob")
  predicted_probabilities_GB <- predict(model_prob_GB, as.matrix(test_set[, -ncol(test_set)]))
  
  predicted_probabilities_GB <- matrix(predicted_probabilities_GB, ncol = 3, byrow = TRUE)
  predicted_probabilities_GB <- round(predicted_probabilities_GB, 5)
  predicted_probabilities_GB <- as.data.frame(predicted_probabilities_GB)
  names(predicted_probabilities_GB) <- c("0", "1", "2")
  
  # Confusion matrix
  conf_matrix_GB <- table(test_set$Target, predictions_GB)
  
  # Accuracy
  accuracy_total_GB[i] <- sum(diag(conf_matrix_GB)) / sum(conf_matrix_GB)
  precision <- diag(conf_matrix_GB) / rowSums(conf_matrix_GB)
  recall <- diag(conf_matrix_GB) / colSums(conf_matrix_GB)
  
  class_probs_GB <- as.data.frame(predicted_probabilities_GB)
  class0_probs_GB <- class_probs_GB[, "0"]
  class1_probs_GB <- class_probs_GB[, "1"]
  class2_probs_GB <- class_probs_GB[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_GB <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_GB)
  roc_curve_class1_GB <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_GB)
  roc_curve_class2_GB <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_GB)
  
  # AUC score
  
  auc_score_class0_total_GB[i] <- auc(roc_curve_class0_GB)
  auc_score_class1_total_GB[i] <- auc(roc_curve_class1_GB)
  auc_score_class2_total_GB[i] <- auc(roc_curve_class2_GB)
  
  df_class0_GB <- rbind(df_class0_GB, data.frame(sensitivity = roc_curve_class0_GB$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_GB$specificities))
  df_class1_GB <- rbind(df_class1_GB, data.frame(sensitivity = roc_curve_class1_GB$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_GB$specificities))
  df_class2_GB <- rbind(df_class2_GB, data.frame(sensitivity = roc_curve_class2_GB$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_GB$specificities))
  
  calibration_curve_class0_GB <- rbind(calibration_curve_class0_GB, data.frame(predicted_prob = class_probs_GB[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_GB <- rbind(calibration_curve_class1_GB, data.frame(predicted_prob = class_probs_GB[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_GB <- rbind(calibration_curve_class2_GB, data.frame(predicted_prob = class_probs_GB[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_GB[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_GB[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- xgb.importance(feature_names = colnames(train_data[, -ncol(train_data)]), model = model_GB)
  feature <- feature_importance$Feature
  importance <- feature_importance$Gain
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_GB), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_GB)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature)) {
    col_name <- feature[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- importance[i]
    }
  }
  feature_importance_GB <- rbind(feature_importance_GB, temp_df)
  
}
##########################################################
# metrics
##########################################################
average_accuracy_GB <- mean(unlist(accuracy_total_GB))
average_auc0_GB <- mean(unlist(auc_score_class0_total_GB))
average_auc1_GB <- mean(unlist(auc_score_class1_total_GB))
average_auc2_GB <- mean(unlist(auc_score_class2_total_GB))
average_precision_GB <- sapply(precision_total_GB, mean)
average_recall_GB <- sapply(recall_total_GB, mean)

# Brier score
brier_score_class0_GB <- mean((calibration_curve_class0_GB$predicted_prob - calibration_curve_class0_GB$observed_outcome)^2)
brier_score_class1_GB <- mean((calibration_curve_class1_GB$predicted_prob - calibration_curve_class1_GB$observed_outcome)^2)
brier_score_class2_GB <- mean((calibration_curve_class2_GB$predicted_prob - calibration_curve_class2_GB$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_GB, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_GB, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_GB, 3)))

print(average_accuracy_GB)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_GB, 3)))
print(paste("Recall:", round(average_recall_GB[1], 3)))
print(paste("Precision:", round(average_precision_GB[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_GB, 3)))
print(paste("Recall:", round(average_recall_GB[2], 3)))
print(paste("Precision:", round(average_precision_GB[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_GB, 3)))
print(paste("Recall:", round(average_recall_GB[3], 3)))
print(paste("Precision:", round(average_precision_GB[3], 3)))

# metrics_df_new[["GradientBoosting"]] <- c(
#   average_accuracy_GB, 
#   brier_score_class0_GB, average_auc0_GB, average_recall_GB[1], average_precision_GB[1], 
#   brier_score_class1_GB, average_auc1_GB, average_recall_GB[2], average_precision_GB[2],
#   brier_score_class2_GB, average_auc2_GB, average_recall_GB[3], average_precision_GB[3]
# )


##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_GB, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_GB, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_GB, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_GB, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_GB, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_GB, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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
feature_importance_long <- gather(feature_importance_GB, key = "Feature", value = "Importance")

feature_importance_GB <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_GB, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Gradient Boosting Model"),
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
