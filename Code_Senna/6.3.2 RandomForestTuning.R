# RandomForest Tuning

library(mlr)
library(randomForest)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)

df_factor <- df
df_factor$Target <- factor(df_factor$Target, levels = c(0, 1, 2))

###########################################
# Tuning Hyperparameters
###########################################

d.RF_H.mlr <- makeClassifTask(data = df_factor, target = "Target")

param_grid_RF_H <- makeParamSet(
  makeDiscreteParam("ntree", values=seq(250, 350, by = 10)),
  makeDiscreteParam("mtry", values=seq(1, 29, by = 1)),
  makeDiscreteParam("nodesize", values=seq(1,20, by=1))
)
# k fold cross validation with 5 folds
control_RF_H <- makeTuneControlRandom(maxit = 300)  
resample <- makeResampleDesc("CV", iters = 5)
measure <- acc

RF_H_tuned <- tuneParams(
  learner = "classif.randomForest",
  task = d.RF_H.mlr,
  resampling = resample,
  measures = measure,
  par.set = param_grid_RF_H,
  control = control_RF_H,
  show.info = TRUE
)

###########################################
# Evaluating Tuned model
###########################################
num_folds <- 1

accuracy_total_RF_H <- auc_score_class0_total_RF_H <- auc_score_class1_total_RF_H <- auc_score_class2_total_RF_H <- rep(0,num_folds)
precision_total_RF_H <- recall_total_RF_H <- auc_scores_total_RF_H <- vector("list", 3)
for (i in 1:3) {
  precision_total_RF_H[[i]] <- recall_total_RF_H[[i]] <- auc_scores_total_RF_H[[i]] <- rep(0, num_folds)
}


df_class0_RF_H <- df_class1_RF_H <- df_class2_RF_H <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_RF_H <- calibration_curve_class1_RF_H <- calibration_curve_class2_RF_H <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_RF_H <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(feature_importance_RF_H) <- colnames(df)
feature_importance_RF_H <- feature_importance_RF_H[, !colnames(feature_importance_RF_H) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

nt = 320
mt = 9
ns = 11

# Test hyperparameters on data in test_set
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data,1)
  
  model_RF_H <- randomForest(`Target` ~ ., data = train_data,
                             ntree=nt, mtry=mt, nodesize=ns)
  
  # Predictions
  predictions_RF_H <- predict(model_RF_H, newdata = test_set)
  predicted_probabilities_RF_H <- predict(model_RF_H, newdata = test_set, type = "prob")
  
  # Confusion matrix
  conf_matrix_RF_H <- table(test_set$Target, predictions_RF_H)
  
  # Accuracy
  accuracy_total_RF_H[i] <- sum(diag(conf_matrix_RF_H)) / sum(conf_matrix_RF_H)
  precision <- diag(conf_matrix_RF_H) / rowSums(conf_matrix_RF_H)
  recall <- diag(conf_matrix_RF_H) / colSums(conf_matrix_RF_H)
  
  class_probs_RF_H <- as.data.frame(predicted_probabilities_RF_H)
  class0_probs_RF_H <- class_probs_RF_H[, "0"]
  class1_probs_RF_H <- class_probs_RF_H[, "1"]
  class2_probs_RF_H <- class_probs_RF_H[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_RF_H <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_RF_H)
  roc_curve_class1_RF_H <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_RF_H)
  roc_curve_class2_RF_H <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_RF_H)
  
  # AUC score
  
  auc_score_class0_total_RF_H[i] <- auc(roc_curve_class0_RF_H)
  auc_score_class1_total_RF_H[i] <- auc(roc_curve_class1_RF_H)
  auc_score_class2_total_RF_H[i] <- auc(roc_curve_class2_RF_H)
  
  df_class0_RF_H <- rbind(df_class0_RF_H, data.frame(sensitivity = roc_curve_class0_RF_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_RF_H$specificities))
  df_class1_RF_H <- rbind(df_class1_RF_H, data.frame(sensitivity = roc_curve_class1_RF_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_RF_H$specificities))
  df_class2_RF_H <- rbind(df_class2_RF_H, data.frame(sensitivity = roc_curve_class2_RF_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_RF_H$specificities))
  
  calibration_curve_class0_RF_H <- rbind(calibration_curve_class0_RF_H, data.frame(predicted_prob = class_probs_RF_H[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_RF_H <- rbind(calibration_curve_class1_RF_H, data.frame(predicted_prob = class_probs_RF_H[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_RF_H <- rbind(calibration_curve_class2_RF_H, data.frame(predicted_prob = class_probs_RF_H[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_RF_H[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_RF_H[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- importance(model_RF_H)
  feature_names <- rownames(feature_importance)
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_RF_H), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_RF_H)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- feature_importance[i]
    }
  }
  feature_importance_RF_H <- rbind(feature_importance_RF_H, temp_df)
}
##########################################################
# metrics
##########################################################
average_accuracy_RF_H <- mean(unlist(accuracy_total_RF_H))
average_auc0_RF_H <- mean(unlist(auc_score_class0_total_RF_H))
average_auc1_RF_H <- mean(unlist(auc_score_class1_total_RF_H))
average_auc2_RF_H <- mean(unlist(auc_score_class2_total_RF_H))
average_precision_RF_H <- sapply(precision_total_RF_H, mean)
average_recall_RF_H <- sapply(recall_total_RF_H, mean)

# Brier score
brier_score_class0_RF_H <- mean((calibration_curve_class0_RF_H$predicted_prob - calibration_curve_class0_RF_H$observed_outcome)^2)
brier_score_class1_RF_H <- mean((calibration_curve_class1_RF_H$predicted_prob - calibration_curve_class1_RF_H$observed_outcome)^2)
brier_score_class2_RF_H <- mean((calibration_curve_class2_RF_H$predicted_prob - calibration_curve_class2_RF_H$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_RF_H, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_RF_H, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_RF_H, 3)))

print(average_accuracy_RF_H)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_RF_H, 3)))
print(paste("Recall:", round(average_recall_RF_H[1], 3)))
print(paste("Precision:", round(average_precision_RF_H[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_RF_H, 3)))
print(paste("Recall:", round(average_recall_RF_H[2], 3)))
print(paste("Precision:", round(average_precision_RF_H[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_RF_H, 3)))
print(paste("Recall:", round(average_recall_RF_H[3], 3)))
print(paste("Precision:", round(average_precision_RF_H[3], 3)))

# metrics_df_new[["RandomForestTuning"]] <- c(
#   average_accuracy_RF_H,
#   brier_score_class0_RF_H, average_auc0_RF_H, average_recall_RF_H[1], average_precision_RF_H[1],
#   brier_score_class1_RF_H, average_auc1_RF_H, average_recall_RF_H[2], average_precision_RF_H[2],
#   brier_score_class2_RF_H, average_auc2_RF_H, average_recall_RF_H[3], average_precision_RF_H[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_RF_H, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_RF_H, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_RF_H, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_RF_H, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_RF_H, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_RF_H, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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
feature_importance_long <- gather(feature_importance_RF_H, key = "Feature", value = "Importance")

feature_importance_RF_H <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_RF_H, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of the tuned Random Forest model"),
       x = "",
       y = "Mean Decrease in Gini score") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 18, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 18, family = "serif"),
        axis.title.x = element_text(size = 18, family = "serif")
  )+
  coord_flip() 


#Upper bound
Q1 <- quantile(feature_importance_RF_H$Importance, 0.25)
Q3 <- quantile(feature_importance_RF_H$Importance, 0.75)
IQR <- Q3 - Q1

# Calculate the upper bound
upper_bound <- Q3 + 1.5 * IQR
upper_bound
upperbound <- feature_importance_RF_H %>%
  filter(Importance > upper_bound)
upperbound

Q3
Q3 <- feature_importance_RF_H %>%
  filter(Importance > Q3)
Q3