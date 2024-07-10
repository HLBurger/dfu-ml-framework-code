# multi Layer Perceptron

library(neuralnet)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)

# Normalize numeric values

# Identify numeric columns
numeric_cols <- sapply(df, is.numeric)
numeric_cols_test <- sapply(test_set, is.numeric)

# Copy the original dataframe
normalized_df <- df 
normalized_test_set <- test_set

# Apply Min-Max scaling only to numeric columns
normalized_df[, numeric_cols] <- lapply(df[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
normalized_test_set[, numeric_cols_test] <- lapply(test_set[, numeric_cols_test], function(x) (x - min(x)) / (max(x) - min(x)))

# Reassign original values to the target column
normalized_df$Target <- df$Target
normalized_test_set$Target <- test_set$Target

normalized_df$Target <- factor(normalized_df$Target, levels = c(0, 1, 2))
normalized_test_set$Target <- factor(normalized_test_set$Target, levels = c(0, 1, 2))


num_folds <- 1

accuracy_total_MLP <- auc_score_class0_total_MLP <- auc_score_class1_total_MLP <- auc_score_class2_total_MLP <- rep(0,num_folds)
precision_total_MLP <- recall_total_MLP <- auc_scores_total_MLP <- vector("list", 3)
for (i in 1:3) {
  precision_total_MLP[[i]] <- recall_total_MLP[[i]] <- auc_scores_total_MLP[[i]] <- rep(0, num_folds)
}


df_class0_MLP <- df_class1_MLP <- df_class2_MLP <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_MLP <- calibration_curve_class1_MLP <- calibration_curve_class2_MLP <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

folds <- createFolds(normalized_df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


# cross-validation
for (i in 1:num_folds) {
  train_data <- normalized_df
  
  model_MLP <- neuralnet(
    Target ~ .,                    
    data = train_data,             
    hidden = c(ncol(df)-1),                
    linear.output = FALSE,         
    act.fct = "logistic",         
    lifesign = "full",  
    stepmax = 100000
  )
  
  plot(model_MLP,rep = "best")
  # Predictions
  predicted_probabilities_MLP <- predict(model_MLP, normalized_test_set)
  predictions_MLP <- apply(predicted_probabilities_MLP, 1, which.max) - 1
  
  # Confusion matrix
  conf_matrix_MLP <- table(normalized_test_set$Target, predictions_MLP)
  
  # Accuracy
  accuracy_total_MLP[i] <- sum(diag(conf_matrix_MLP)) / sum(conf_matrix_MLP)
  precision <- diag(conf_matrix_MLP) / rowSums(conf_matrix_MLP)
  recall <- diag(conf_matrix_MLP) / colSums(conf_matrix_MLP)
  
  class_probs_MLP <- as.data.frame(predicted_probabilities_MLP)
  class0_probs_MLP <- class_probs_MLP[, 1]
  class1_probs_MLP <- class_probs_MLP[, 2]
  class2_probs_MLP <- class_probs_MLP[, 3]
  
  normalized_test_set$Target <- factor(normalized_test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_MLP <- roc(ifelse(normalized_test_set$Target == "0", 1, 0), class0_probs_MLP)
  roc_curve_class1_MLP <- roc(ifelse(normalized_test_set$Target == "1", 1, 0), class1_probs_MLP)
  roc_curve_class2_MLP <- roc(ifelse(normalized_test_set$Target == "2", 1, 0), class2_probs_MLP)
  
  # AUC score
  
  auc_score_class0_total_MLP[i] <- auc(roc_curve_class0_MLP)
  auc_score_class1_total_MLP[i] <- auc(roc_curve_class1_MLP)
  auc_score_class2_total_MLP[i] <- auc(roc_curve_class2_MLP)
  
  df_class0_MLP <- rbind(df_class0_MLP, data.frame(sensitivity = roc_curve_class0_MLP$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_MLP$specificities))
  df_class1_MLP <- rbind(df_class1_MLP, data.frame(sensitivity = roc_curve_class1_MLP$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_MLP$specificities))
  df_class2_MLP <- rbind(df_class2_MLP, data.frame(sensitivity = roc_curve_class2_MLP$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_MLP$specificities))
  
  calibration_curve_class0_MLP <- rbind(calibration_curve_class0_MLP, data.frame(predicted_prob = class_probs_MLP[, "V1"], 
                                                                               observed_outcome = ifelse(normalized_test_set$Target == "0", 1, 0)))
  calibration_curve_class1_MLP <- rbind(calibration_curve_class1_MLP, data.frame(predicted_prob = class_probs_MLP[, "V2"], 
                                                                               observed_outcome = ifelse(normalized_test_set$Target == "1", 1, 0)))
  calibration_curve_class2_MLP <- rbind(calibration_curve_class2_MLP, data.frame(predicted_prob = class_probs_MLP[, "V3"], 
                                                                               observed_outcome = ifelse(normalized_test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_MLP[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_MLP[[class_val]][i] <- recall[class_val]
    
  }
}

# Calculate average evaluation metrics per class
average_accuracy_MLP <- mean(unlist(accuracy_total_MLP))
average_auc0_MLP <- mean(unlist(auc_score_class0_total_MLP))
average_auc1_MLP <- mean(unlist(auc_score_class1_total_MLP))
average_auc2_MLP <- mean(unlist(auc_score_class2_total_MLP))
average_precision_MLP <- sapply(precision_total_MLP, mean)
average_recall_MLP <- sapply(recall_total_MLP, mean)

# Brier score
brier_score_class0_MLP <- mean((calibration_curve_class0_MLP$predicted_prob - calibration_curve_class0_MLP$observed_outcome)^2)
brier_score_class1_MLP <- mean((calibration_curve_class1_MLP$predicted_prob - calibration_curve_class1_MLP$observed_outcome)^2)
brier_score_class2_MLP <- mean((calibration_curve_class2_MLP$predicted_prob - calibration_curve_class2_MLP$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_MLP, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_MLP, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_MLP, 3)))

print(average_accuracy_MLP)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_MLP, 3)))
print(paste("Recall:", round(average_recall_MLP[1], 3)))
print(paste("Precision:", round(average_precision_MLP[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_MLP, 3)))
print(paste("Recall:", round(average_recall_MLP[2], 3)))
print(paste("Precision:", round(average_precision_MLP[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_MLP, 3)))
print(paste("Recall:", round(average_recall_MLP[3], 3)))
print(paste("Precision:", round(average_precision_MLP[3], 3)))

# metrics_df_new[["MLP(30)"]] <- c(
#   average_accuracy_MLP, 
#   brier_score_class0_MLP, average_auc0_MLP, average_recall_MLP[1], average_precision_MLP[1], 
#   brier_score_class1_MLP, average_auc1_MLP, average_recall_MLP[2], average_precision_MLP[2],
#   brier_score_class2_MLP, average_auc2_MLP, average_recall_MLP[3], average_precision_MLP[3]
# )


##########################################################
# Visuals
##########################################################
# ROC curve
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_MLP, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_MLP, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_MLP, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
  labs(title = "ROC curve of Multi-Layer Perceptron model",
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
  geom_smooth(data = calibration_curve_class0_MLP, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_MLP, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_MLP, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
  labs(title = "Calibration Curves of Multi-Layer Perceptron model",
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
        axis.title.x = element_text(size = 16, family = "serif"))+
  xlim(0, 1) +
  ylim(0, 1)



