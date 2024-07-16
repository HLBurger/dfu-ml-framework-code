#Multi Layer Tuning

library(caret)
library(nnet)
library(mlr)
library(neuralnet)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)

#############################
# Normalize numeric values
#############################

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

###########################################
# Hyperparameter Tuning
###########################################
d.MLP.mlr <- makeClassifTask(data = normalized_df, target = "Target")

# For the grid I varies the len, the number of hidden layers, between 1L and 5L
# I did this with trial and error
# the amount of nodes is varies between 1 and the amount of variables
param_grid_MLP <- makeParamSet(
  makeIntegerVectorParam("size", len = 1L, lower = 1, upper = ncol(df)-1)
)

control_MLP <- makeTuneControlRandom(maxit = 1000)  
resample <- makeResampleDesc("CV", iters = 2)
measure <- acc

MLP_tuned <- tuneParams(
  learner = "classif.nnet",
  task = d.MLP.mlr,
  resampling = resample,
  measures = measure,
  par.set = param_grid_MLP,
  control = control_MLP,
  show.info = TRUE
)

# This returns the amount of nodes per layer

###########################################
# Evaluating Tuned model
###########################################
num_folds <- 1

accuracy_total_MLP_H <- auc_score_class0_total_MLP_H <- auc_score_class1_total_MLP_H <- auc_score_class2_total_MLP_H <- rep(0,num_folds)
precision_total_MLP_H <- recall_total_MLP_H <- auc_scores_total_MLP_H <- vector("list", 3)
for (i in 1:3) {
  precision_total_MLP_H[[i]] <- recall_total_MLP_H[[i]] <- auc_scores_total_MLP_H[[i]] <- rep(0, num_folds)
}


df_class0_MLP_H <- df_class1_MLP_H <- df_class2_MLP_H <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_MLP_H <- calibration_curve_class1_MLP_H <- calibration_curve_class2_MLP_H <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

folds <- createFolds(normalized_df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


for (i in 1:num_folds) {
  train_data <- normalized_df

  train_data <- process_data_MLP(train_data,1) 

  model_MLP_H <- neuralnet(
    Target ~ .,
    data = train_data,
    hidden = c(3),  
    linear.output = FALSE,
    act.fct = "logistic",
    lifesign = "full",
    stepmax = 100000
  )

  plot(model_MLP_H,rep = "best")
  # Predictions
  predicted_probabilities_MLP_H <- predict(model_MLP_H, normalized_test_set)
  predictions_MLP_H <- apply(predicted_probabilities_MLP_H, 1, which.max) - 1

  # Confusion matrix
  conf_matrix_MLP_H <- table(normalized_test_set$Target, predictions_MLP_H)

  # Accuracy
  accuracy_total_MLP_H[i] <- sum(diag(conf_matrix_MLP_H)) / sum(conf_matrix_MLP_H)
  precision <- diag(conf_matrix_MLP_H) / rowSums(conf_matrix_MLP_H)
  recall <- diag(conf_matrix_MLP_H) / colSums(conf_matrix_MLP_H)

  class_probs_MLP_H <- as.data.frame(predicted_probabilities_MLP_H)
  class0_probs_MLP_H <- class_probs_MLP_H[, 1]
  class1_probs_MLP_H <- class_probs_MLP_H[, 2]
  class2_probs_MLP_H <- class_probs_MLP_H[, 3]

  normalized_test_set$Target <- factor(normalized_test_set$Target, levels = c("0", "1", "2"))

  # ROC curves
  roc_curve_class0_MLP_H <- roc(ifelse(normalized_test_set$Target == "0", 1, 0), class0_probs_MLP_H)
  roc_curve_class1_MLP_H <- roc(ifelse(normalized_test_set$Target == "1", 1, 0), class1_probs_MLP_H)
  roc_curve_class2_MLP_H <- roc(ifelse(normalized_test_set$Target == "2", 1, 0), class2_probs_MLP_H)

  # AUC score

  auc_score_class0_total_MLP_H[i] <- auc(roc_curve_class0_MLP_H)
  auc_score_class1_total_MLP_H[i] <- auc(roc_curve_class1_MLP_H)
  auc_score_class2_total_MLP_H[i] <- auc(roc_curve_class2_MLP_H)

  df_class0_MLP_H <- rbind(df_class0_MLP_H, data.frame(sensitivity = roc_curve_class0_MLP_H$sensitivities,
                                                   specificity = 1 - roc_curve_class0_MLP_H$specificities))
  df_class1_MLP_H <- rbind(df_class1_MLP_H, data.frame(sensitivity = roc_curve_class1_MLP_H$sensitivities,
                                                   specificity = 1 - roc_curve_class1_MLP_H$specificities))
  df_class2_MLP_H <- rbind(df_class2_MLP_H, data.frame(sensitivity = roc_curve_class2_MLP_H$sensitivities,
                                                   specificity = 1 - roc_curve_class2_MLP_H$specificities))

  calibration_curve_class0_MLP_H <- rbind(calibration_curve_class0_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V1"],
                                                                                 observed_outcome = ifelse(normalized_test_set$Target == "0", 1, 0)))
  calibration_curve_class1_MLP_H <- rbind(calibration_curve_class1_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V2"],
                                                                                 observed_outcome = ifelse(normalized_test_set$Target == "1", 1, 0)))
  calibration_curve_class2_MLP_H <- rbind(calibration_curve_class2_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V3"],
                                                                                 observed_outcome = ifelse(normalized_test_set$Target == "2", 1, 0)))

  for (class_val in 1:3) {
    # Precision
    precision_total_MLP_H[[class_val]][i] <- precision[class_val]

    # Recall
    recall_total_MLP_H[[class_val]][i] <- recall[class_val]

  }
}
###########################################
# Metrics
###########################################

# Calculate average evaluation metrics per class
average_accuracy_MLP_H <- mean(unlist(accuracy_total_MLP_H))
average_auc0_MLP_H <- mean(unlist(auc_score_class0_total_MLP_H))
average_auc1_MLP_H <- mean(unlist(auc_score_class1_total_MLP_H))
average_auc2_MLP_H <- mean(unlist(auc_score_class2_total_MLP_H))
average_precision_MLP_H <- sapply(precision_total_MLP_H, mean)
average_recall_MLP_H <- sapply(recall_total_MLP_H, mean)

brier_score_class0_MLP_H <- mean((calibration_curve_class0_MLP_H$predicted_prob - calibration_curve_class0_MLP_H$observed_outcome)^2)
brier_score_class1_MLP_H <- mean((calibration_curve_class1_MLP_H$predicted_prob - calibration_curve_class1_MLP_H$observed_outcome)^2)
brier_score_class2_MLP_H <- mean((calibration_curve_class2_MLP_H$predicted_prob - calibration_curve_class2_MLP_H$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_MLP_H, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_MLP_H, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_MLP_H, 3)))


print(average_accuracy_MLP_H)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_MLP_H, 3)))
print(paste("Recall:", round(average_recall_MLP_H[1], 3)))
print(paste("Precision:", round(average_precision_MLP_H[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_MLP_H, 3)))
print(paste("Recall:", round(average_recall_MLP_H[2], 3)))
print(paste("Precision:", round(average_precision_MLP_H[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_MLP_H, 3)))
print(paste("Recall:", round(average_recall_MLP_H[3], 3)))
print(paste("Precision:", round(average_precision_MLP_H[3], 3)))

# metrics_df_new[["MLP(3)"]] <- c(
#   average_accuracy_MLP, 
#   brier_score_class0_MLP, average_auc0_MLP, average_recall_MLP[1], average_precision_MLP[1], 
#   brier_score_class1_MLP, average_auc1_MLP, average_recall_MLP[2], average_precision_MLP[2],
#   brier_score_class2_MLP, average_auc2_MLP, average_recall_MLP[3], average_precision_MLP[3]
# )

###########################################
# Visuals
###########################################

ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_MLP_H, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_MLP_H, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_MLP_H, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
  labs(title = "ROC curve of Multi-Layer Perceptron  model",
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
  geom_smooth(data = calibration_curve_class0_MLP_H, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_MLP_H, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_MLP_H, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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
