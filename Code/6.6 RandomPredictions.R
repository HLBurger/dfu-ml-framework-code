# Random Predictions

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(pROC)

num_folds <- 5

accuracy_total_RAND <- auc_score_class0_total_RAND <- auc_score_class1_total_RAND <- auc_score_class2_total_RAND <- rep(0,num_folds)
precision_total_RAND <- recall_total_RAND <- auc_scores_total_RAND <- vector("list", 3)
for (i in 1:3) {
  precision_total_RAND[[i]] <- recall_total_RAND[[i]] <- auc_scores_total_RAND[[i]] <- rep(0, num_folds)
}


df_class0_RAND <- df_class1_RAND <- df_class2_RAND <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_RAND <- calibration_curve_class1_RAND <- calibration_curve_class2_RAND <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

# Perform multiple random predictions and average the results
for (i in 1:num_folds) {
  train_data <- df

  # Predictions
  num_predictions <- nrow(test_set)

  predicted_probabilities_RAND <- matrix(runif(num_predictions * 3), ncol = 3)
  predictions_RAND <- apply(predicted_probabilities_RAND, 1, which.max) - 1  # -1 to match with classes 0, 1, 2
  

  # Confusion matrix
  conf_matrix_RAND <- table(test_set$Target, predictions_RAND)
  
  # Accuracy
  accuracy_total_RAND[i] <- sum(diag(conf_matrix_RAND)) / sum(conf_matrix_RAND)
  precision <- diag(conf_matrix_RAND) / rowSums(conf_matrix_RAND)
  recall <- diag(conf_matrix_RAND) / colSums(conf_matrix_RAND)
  
  class_probs_RAND <- as.data.frame(predicted_probabilities_RAND)
  class0_probs_RAND <- class_probs_RAND[, "V1"]
  class1_probs_RAND <- class_probs_RAND[, "V2"]
  class2_probs_RAND <- class_probs_RAND[, "V3"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_RAND <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_RAND)
  roc_curve_class1_RAND <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_RAND)
  roc_curve_class2_RAND <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_RAND)
  
  # AUC score
  
  auc_score_class0_total_RAND[i] <- auc(roc_curve_class0_RAND)
  auc_score_class1_total_RAND[i] <- auc(roc_curve_class1_RAND)
  auc_score_class2_total_RAND[i] <- auc(roc_curve_class2_RAND)
  
  df_class0_RAND <- rbind(df_class0_RAND, data.frame(sensitivity = roc_curve_class0_RAND$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_RAND$specificities))
  df_class1_RAND <- rbind(df_class1_RAND, data.frame(sensitivity = roc_curve_class1_RAND$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_RAND$specificities))
  df_class2_RAND <- rbind(df_class2_RAND, data.frame(sensitivity = roc_curve_class2_RAND$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_RAND$specificities))
  
  calibration_curve_class0_RAND <- rbind(calibration_curve_class0_RAND, data.frame(predicted_prob = class_probs_RAND[, "V1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_RAND <- rbind(calibration_curve_class1_RAND, data.frame(predicted_prob = class_probs_RAND[, "V2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_RAND <- rbind(calibration_curve_class2_RAND, data.frame(predicted_prob = class_probs_RAND[, "V3"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_RAND[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_RAND[[class_val]][i] <- recall[class_val]
    
  }
}
#########################
# Metrics
#########################

# Calculate average evaluation metrics per class
average_accuracy_RAND <- mean(unlist(accuracy_total_RAND))
average_auc0_RAND <- mean(unlist(auc_score_class0_total_RAND))
average_auc1_RAND <- mean(unlist(auc_score_class1_total_RAND))
average_auc2_RAND <- mean(unlist(auc_score_class2_total_RAND))
average_precision_RAND <- sapply(precision_total_RAND, mean)
average_recall_RAND <- sapply(recall_total_RAND, mean)

# Brier score
brier_score_class0_RAND <- mean((calibration_curve_class0_RAND$predicted_prob - calibration_curve_class0_RAND$observed_outcome)^2)
brier_score_class1_RAND <- mean((calibration_curve_class1_RAND$predicted_prob - calibration_curve_class1_RAND$observed_outcome)^2)
brier_score_class2_RAND <- mean((calibration_curve_class2_RAND$predicted_prob - calibration_curve_class2_RAND$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_RAND, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_RAND, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_RAND, 3)))

print(average_accuracy_RAND)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_RAND, 3)))
print(paste("Recall:", round(average_recall_RAND[1], 3)))
print(paste("Precision:", round(average_precision_RAND[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_RAND, 3)))
print(paste("Recall:", round(average_recall_RAND[2], 3)))
print(paste("Precision:", round(average_precision_RAND[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_RAND, 3)))
print(paste("Recall:", round(average_recall_RAND[3], 3)))
print(paste("Precision:", round(average_precision_RAND[3], 3)))

# metrics_df_new[["Random"]] <- c(
#   average_accuracy_RAND, 
#   brier_score_class0_RAND, average_auc0_RAND, average_recall_RAND[1], average_precision_RAND[1], 
#   brier_score_class1_RAND, average_auc1_RAND, average_recall_RAND[2], average_precision_RAND[2],
#   brier_score_class2_RAND, average_auc2_RAND, average_recall_RAND[3], average_precision_RAND[3]
# )


#########################
# Visuals
#########################

# ROC curve
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_RAND, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_RAND, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_RAND, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
  labs(title = "ROC curve of Random Predictions model",
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


# Calibration plot
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = calibration_curve_class0_RAND, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_RAND, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_RAND, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
  labs(title = "Calibration Curves of Random Predictions model",
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

