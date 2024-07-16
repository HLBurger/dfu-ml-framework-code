#Logistic Regression Bootstrap

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(pROC)


# First the variables that showed multicollinearity should be removed from the dataset
df_removed <- df[, !(names(df) %in% c("Woundlocation_Dorsalis_Pedis",
                                              "Toepressureaffectedside",
                                              "AverageserumHb",
                                              "Ageyears",
                                              "Averagediastolicbloodpressure",
                                              "Neuropathic",
                                              "TexasD",
                                              "TBI",
                                              "AverageserumeGFR"))]

test_set_removed_removed <- test_set[, !(names(test_set) %in% c("Woundlocation_Dorsalis_Pedis",
                                              "Toepressureaffectedside",
                                              "AverageserumHb",
                                              "Ageyears",
                                              "Averagediastolicbloodpressure",
                                              "Neuropathic",
                                              "TexasD",
                                              "TBI",
                                              "AverageserumeGFR"))]
num_folds <- 1

accuracy_total_LR <- auc_score_class0_total_LR <- auc_score_class1_total_LR <- auc_score_class2_total_LR <- rep(0,num_folds)
precision_total_LR <- recall_total_LR <- auc_scores_total_LR <- vector("list", 3)
for (i in 1:3) {
  precision_total_LR[[i]] <- recall_total_LR[[i]] <- auc_scores_total_LR[[i]] <- rep(0, num_folds)
}


df_class0_LR <- df_class1_LR <- df_class2_LR <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_LR <- calibration_curve_class1_LR <- calibration_curve_class2_LR <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_df1_LR <- data.frame(matrix(ncol = 30, nrow = 0))
colnames(feature_importance_df1_LR) <- paste0("V", 1:30)


feature_importance_df2_LR <- data.frame(matrix(ncol = 30, nrow = 0))
colnames(feature_importance_df2_LR) <- paste0("V", 1:30)

folds <- createFolds(df_removed$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


# Perform cross-validation
for (i in 1:num_folds) {
  train_data <- df_removed
  
  train_data <- process_data_logistic(train_data,1)
  model_LR <- multinom(Target ~ ., data = train_data)

  # Predictions
  predictions_LR <- predict(model_LR, newdata = test_set_removed)
  predicted_probabilities_LR <- predict(model_LR, newdata = test_set_removed, type = "probs")
  
  # Confusion matrix
  conf_matrix_LR <- table(test_set_removed$Target, predictions_LR)
  
  # Accuracy
  accuracy_total_LR[i] <- sum(diag(conf_matrix_LR)) / sum(conf_matrix_LR)
  precision <- diag(conf_matrix_LR) / rowSums(conf_matrix_LR)
  recall <- diag(conf_matrix_LR) / colSums(conf_matrix_LR)
  
  class_probs_LR <- as.data.frame(predicted_probabilities_LR)
  class0_probs_LR <- class_probs_LR[, "0"]
  class1_probs_LR <- class_probs_LR[, "1"]
  class2_probs_LR <- class_probs_LR[, "2"]
  
  test_set_removed$Target <- factor(test_set_removed$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_LR <- roc(ifelse(test_set_removed$Target == "0", 1, 0), class0_probs_LR)
  roc_curve_class1_LR <- roc(ifelse(test_set_removed$Target == "1", 1, 0), class1_probs_LR)
  roc_curve_class2_LR <- roc(ifelse(test_set_removed$Target == "2", 1, 0), class2_probs_LR)
  
  # AUC score
  
  auc_score_class0_total_LR[i] <- auc(roc_curve_class0_LR)
  auc_score_class1_total_LR[i] <- auc(roc_curve_class1_LR)
  auc_score_class2_total_LR[i] <- auc(roc_curve_class2_LR)
  
  df_class0_LR <- rbind(df_class0_LR, data.frame(sensitivity = roc_curve_class0_LR$sensitivities, 
                                           specificity = 1 - roc_curve_class0_LR$specificities))
  df_class1_LR <- rbind(df_class1_LR, data.frame(sensitivity = roc_curve_class1_LR$sensitivities, 
                                           specificity = 1 - roc_curve_class1_LR$specificities))
  df_class2_LR <- rbind(df_class2_LR, data.frame(sensitivity = roc_curve_class2_LR$sensitivities, 
                                           specificity = 1 - roc_curve_class2_LR$specificities))
  
  calibration_curve_class0_LR <- rbind(calibration_curve_class0_LR, data.frame(predicted_prob = class_probs_LR[, "0"], 
                                                                         observed_outcome = ifelse(test_set_removed$Target == "0", 1, 0)))
  calibration_curve_class1_LR <- rbind(calibration_curve_class1_LR, data.frame(predicted_prob = class_probs_LR[, "1"], 
                                                                         observed_outcome = ifelse(test_set_removed$Target == "1", 1, 0)))
  calibration_curve_class2_LR <- rbind(calibration_curve_class2_LR, data.frame(predicted_prob = class_probs_LR[, "2"], 
                                                                         observed_outcome = ifelse(test_set_removed$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_LR[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_LR[[class_val]][i] <- recall[class_val]

  }
  
  # get feature importance
  coefficients_LR <- coef(model_LR)
  feature_names <- colnames(coefficients_LR)
  
  for (class_idx in 1:2) {
    if (class_idx == 1) {
      feature_importance_df1_LR <- rbind(feature_importance_df1_LR, coefficients_LR[class_idx, ])
    } else if (class_idx == 2) {
      feature_importance_df2_LR <- rbind(feature_importance_df2_LR, coefficients_LR[class_idx, ])
    }
  }
}
colnames(feature_importance_df1_LR) <- feature_names
colnames(feature_importance_df2_LR) <- feature_names


##########################################################
# Metrics
##########################################################

average_accuracy_LR <- mean(unlist(accuracy_total_LR))
average_auc0_LR <- mean(unlist(auc_score_class0_total_LR))
average_auc1_LR <- mean(unlist(auc_score_class1_total_LR))
average_auc2_LR <- mean(unlist(auc_score_class2_total_LR))
average_precision_LR <- sapply(precision_total_LR, mean)
average_recall_LR <- sapply(recall_total_LR, mean)

# Brier score
brier_score_class0_LR <- mean((calibration_curve_class0_LR$predicted_prob - calibration_curve_class0_LR$observed_outcome)^2)
brier_score_class1_LR <- mean((calibration_curve_class1_LR$predicted_prob - calibration_curve_class1_LR$observed_outcome)^2)
brier_score_class2_LR <- mean((calibration_curve_class2_LR$predicted_prob - calibration_curve_class2_LR$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_LR, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_LR, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_LR, 3)))

print(average_accuracy_LR)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_LR, 3)))
print(paste("Recall:", round(average_recall_LR[1], 3)))
print(paste("Precision:", round(average_precision_LR[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_LR, 3)))
print(paste("Recall:", round(average_recall_LR[2], 3)))
print(paste("Precision:", round(average_precision_LR[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_LR, 3)))
print(paste("Recall:", round(average_recall_LR[3], 3)))
print(paste("Precision:", round(average_precision_LR[3], 3)))

# metrics_df_new[["LogisticRegression"]] <- c(
#   average_accuracy_LR,
#   brier_score_class0_LR, average_auc0_LR, average_recall_LR[1], average_precision_LR[1],
#   brier_score_class1_LR, average_auc1_LR, average_recall_LR[2], average_precision_LR[2],
#   brier_score_class2_LR, average_auc2_LR, average_recall_LR[3], average_precision_LR[3]
# )



##########################################################
# ROC Curve
##########################################################

ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_LR, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_LR, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_LR, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_LR, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_LR, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_LR, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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
#Feature Importance
##########################################################
feature_importance_long_df1 <- gather(feature_importance_df1_LR, key = "Feature", value = "Importance")
feature_importance_long_df2 <- gather(feature_importance_df2_LR, key = "Feature", value = "Importance")

feature_importance_df1_LR <- feature_importance_long_df1 %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

feature_importance_df2_LR <- feature_importance_long_df2 %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

# feature_importance_df1_LR <- feature_importance_df1_LR %>%
#   filter(Feature != "(Intercept)")
# 
# feature_importance_df2_LR <- feature_importance_df2_LR %>%
#   filter(Feature != "(Intercept)")


ggplot(data = feature_importance_df1_LR, aes(x = reorder(Feature, (Importance)), y = (Importance))) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Logistic Regression for amputation"),
       x = "",
       y = "Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 18, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 18, family = "serif"),
        axis.title.x = element_text(size = 18, family = "serif")
  )+
  coord_flip()

ggplot(data = feature_importance_df2_LR, aes(x = reorder(Feature, (Importance)), y = (Importance))) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Logistic Regression for no wound healing"),
       x = "",
       y = "Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 18, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 18, family = "serif"),
        axis.title.x = element_text(size = 18, family = "serif")
  )+
  coord_flip()

density_features_LR_1 <- data.frame(Importance = feature_importance_df1_LR$Importance)

Q1 <- quantile(feature_importance_df1_LR$Importance, 0.05)
Q2 <- quantile(feature_importance_df1_LR$Importance, 0.25)
Q3 <- quantile(feature_importance_df1_LR$Importance, 0.75)
Q4 <- quantile(feature_importance_df1_LR$Importance, 0.95)

most_important_Q1_LR_1 <- feature_importance_df1_LR %>%
  filter(Importance < Q1)

most_important_Q2_LR_1 <- feature_importance_df1_LR %>%
  filter(Importance < Q2)

most_important_Q3_LR_1 <- feature_importance_df1_LR %>%
  filter(Importance > Q3)

most_important_Q4_LR_1 <- feature_importance_df1_LR %>%
  filter(Importance > Q4)

most_important_Q1_LR_1
most_important_Q2_LR_1

most_important_Q3_LR_1
most_important_Q4_LR_1


density_features_LR_2 <- data.frame(Importance = feature_importance_df2_LR$Importance)

Q1 <- quantile(feature_importance_df2_LR$Importance, 0.05)
Q2 <- quantile(feature_importance_df2_LR$Importance, 0.25)
Q3 <- quantile(feature_importance_df2_LR$Importance, 0.75)
Q4 <- quantile(feature_importance_df2_LR$Importance, 0.95)


most_important_Q1_LR_2 <- feature_importance_df2_LR %>%
  filter(Importance < Q1)

most_important_Q2_LR_2 <- feature_importance_df2_LR %>%
  filter(Importance < Q2)

most_important_Q3_LR_2 <- feature_importance_df2_LR %>%
  filter(Importance > Q3)

most_important_Q4_LR_2 <- feature_importance_df2_LR %>%
  filter(Importance > Q4)

most_important_Q1_LR_2
most_important_Q2_LR_2

most_important_Q3_LR_2
most_important_Q4_LR_2

