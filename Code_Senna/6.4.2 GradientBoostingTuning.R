#gradient boosting tuning

library(mlr)
library(gbm)
library(xgboost)
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

d.GB_H.mlr <- makeClassifTask(data = df_factor, target = "Target")

# Define parameter grid for Gradient Boosting
param_grid_GB_H <- makeParamSet(
  makeDiscreteParam("max_depth", values= seq(1,40, by=2)),
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower=0, upper=20),
  makeDiscreteParam("lambda", values = seq(0,1,by=1)),
  makeDiscreteParam("alpha", values = seq(0,1,by=1))
  
)

# Define tuning control
control_GB_H <- makeTuneControlRandom(maxit = 100)  

# Define resampling
resample <- makeResampleDesc("CV", iters = 5)
measure <- acc

# Perform hyperparameter tuning
GB_H_tuned <- tuneParams(
  learner = "classif.xgboost",
  task = d.GB_H.mlr,
  resampling = resample,
  measures = measure,
  par.set = param_grid_GB_H,
  control = control_GB_H,
  show.info = TRUE
)

#This returns the best hyperparameters

###########################################
# Evaluating Tuned model
###########################################
num_folds <- 1

accuracy_total_GB_H <- auc_score_class0_total_GB_H <- auc_score_class1_total_GB_H <- auc_score_class2_total_GB_H <- rep(0,num_folds)
precision_total_GB_H <- recall_total_GB_H <- auc_scores_total_GB_H <- vector("list", 3)
for (i in 1:3) {
  precision_total_GB_H[[i]] <- recall_total_GB_H[[i]] <- auc_scores_total_GB_H[[i]] <- rep(0, num_folds)
}


df_class0_GB_H <- df_class1_GB_H <- df_class2_GB_H <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_GB_H <- calibration_curve_class1_GB_H <- calibration_curve_class2_GB_H <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_GB_H <- data.frame(matrix(ncol = ncol(df_total), nrow = 0))
colnames(feature_importance_GB_H) <- colnames(df_total)
feature_importance_GB_H <- feature_importance_GB_H[, !colnames(feature_importance_GB_H) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

params <- list(eta=0.1, gamma=0, max_depth=4,
               lambda = 1, alpha = 0)

#  cross-validation
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data,1)
  train_data$Target <- factor(train_data$Target, levels = c(0,1,2))
  train_data$Target <- as.numeric(as.character(train_data$Target))
  
  model_GB_H <- xgboost(data = as.matrix(train_data[, -ncol(train_data)]), 
                      nrounds = 100, 
                      params = params,
                      label = train_data$Target,
                      objective = "multi:softmax", 
                      num_class = 3,
                      n.tree=nt,
                      shrinkage=s,
                      interaction.depth=id
                      )
  
  model_prob_GB_H <- xgboost(data = as.matrix(train_data[, -ncol(train_data)]), 
                           nrounds = 100, 
                           label = train_data$Target,
                           objective = "multi:softprob", 
                           num_class = 3,
                           n.tree=nt,
                           shrinkage=s,
                           interaction.depth=id)
  
  
  # Predictions
  predictions_GB_H <- predict(model_GB_H, as.matrix(test_set[, -ncol(test_set)]), type="prob")
  predicted_probabilities_GB_H <- predict(model_prob_GB_H, as.matrix(test_set[, -ncol(test_set)]))
  
  predicted_probabilities_GB_H <- matrix(predicted_probabilities_GB_H, ncol = 3, byrow = TRUE)
  predicted_probabilities_GB_H <- round(predicted_probabilities_GB_H, 5)
  predicted_probabilities_GB_H <- as.data.frame(predicted_probabilities_GB_H)
  names(predicted_probabilities_GB_H) <- c("0", "1", "2")
  
  # Confusion matrix
  conf_matrix_GB_H <- table(test_set$Target, predictions_GB_H)
  
  # Accuracy
  accuracy_total_GB_H[i] <- sum(diag(conf_matrix_GB_H)) / sum(conf_matrix_GB_H)
  precision <- diag(conf_matrix_GB_H) / rowSums(conf_matrix_GB_H)
  recall <- diag(conf_matrix_GB_H) / colSums(conf_matrix_GB_H)
  
  class_probs_GB_H <- as.data.frame(predicted_probabilities_GB_H)
  class0_probs_GB_H <- class_probs_GB_H[, "0"]
  class1_probs_GB_H <- class_probs_GB_H[, "1"]
  class2_probs_GB_H <- class_probs_GB_H[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_GB_H <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_GB_H)
  roc_curve_class1_GB_H <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_GB_H)
  roc_curve_class2_GB_H <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_GB_H)
  
  # AUC score
  
  auc_score_class0_total_GB_H[i] <- auc(roc_curve_class0_GB_H)
  auc_score_class1_total_GB_H[i] <- auc(roc_curve_class1_GB_H)
  auc_score_class2_total_GB_H[i] <- auc(roc_curve_class2_GB_H)
  
  df_class0_GB_H <- rbind(df_class0_GB_H, data.frame(sensitivity = roc_curve_class0_GB_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_GB_H$specificities))
  df_class1_GB_H <- rbind(df_class1_GB_H, data.frame(sensitivity = roc_curve_class1_GB_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_GB_H$specificities))
  df_class2_GB_H <- rbind(df_class2_GB_H, data.frame(sensitivity = roc_curve_class2_GB_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_GB_H$specificities))
  
  calibration_curve_class0_GB_H <- rbind(calibration_curve_class0_GB_H, data.frame(predicted_prob = class_probs_GB_H[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_GB_H <- rbind(calibration_curve_class1_GB_H, data.frame(predicted_prob = class_probs_GB_H[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_GB_H <- rbind(calibration_curve_class2_GB_H, data.frame(predicted_prob = class_probs_GB_H[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_GB_H[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_GB_H[[class_val]][i] <- recall[class_val]
    
  }
  feature_importance <- xgb.importance(feature_names = colnames(train_data[, -ncol(train_data)]), model = model_GB_H)
  feature <- feature_importance$Feature
  importance <- feature_importance$Gain
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_GB_H), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_GB_H)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature)) {
    col_name <- feature[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- importance[i]
    }
  }
  feature_importance_GB_H <- rbind(feature_importance_GB_H, temp_df)
  
}
##########################################################
# metrics
##########################################################
average_accuracy_GB_H <- mean(unlist(accuracy_total_GB_H))
average_auc0_GB_H <- mean(unlist(auc_score_class0_total_GB_H))
average_auc1_GB_H <- mean(unlist(auc_score_class1_total_GB_H))
average_auc2_GB_H <- mean(unlist(auc_score_class2_total_GB_H))
average_precision_GB_H <- sapply(precision_total_GB_H, mean)
average_recall_GB_H <- sapply(recall_total_GB_H, mean)

# Brier score
brier_score_class0_GB_H <- mean((calibration_curve_class0_GB_H$predicted_prob - calibration_curve_class0_GB_H$observed_outcome)^2)
brier_score_class1_GB_H <- mean((calibration_curve_class1_GB_H$predicted_prob - calibration_curve_class1_GB_H$observed_outcome)^2)
brier_score_class2_GB_H <- mean((calibration_curve_class2_GB_H$predicted_prob - calibration_curve_class2_GB_H$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_GB_H, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_GB_H, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_GB_H, 3)))

print(average_accuracy_GB_H)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_GB_H, 3)))
print(paste("Recall:", round(average_recall_GB_H[1], 3)))
print(paste("Precision:", round(average_precision_GB_H[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_GB_H, 3)))
print(paste("Recall:", round(average_recall_GB_H[2], 3)))
print(paste("Precision:", round(average_precision_GB_H[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_GB_H, 3)))
print(paste("Recall:", round(average_recall_GB_H[3], 3)))
print(paste("Precision:", round(average_precision_GB_H[3], 3)))

# metrics_df_new[["GradientBoostingTuning"]] <- c(
#   average_accuracy_GB_H,
#   brier_score_class0_GB_H, average_auc0_GB_H, average_recall_GB_H[1], average_precision_GB_H[1],
#   brier_score_class1_GB_H, average_auc1_GB_H, average_recall_GB_H[2], average_precision_GB_H[2],
#   brier_score_class2_GB_H, average_auc2_GB_H, average_recall_GB_H[3], average_precision_GB_H[3]
# )


##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_GB_H, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_GB_H, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_GB_H, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_GB_H, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_GB_H, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_GB_H, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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
feature_importance_long <- gather(feature_importance_GB_H, key = "Feature", value = "Importance")

feature_importance_GB_H <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_GB_H, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of Gradient Boosting Model"),
       x = "",
       y = "Mean improvement in accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 18, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 18, family = "serif"),
        axis.title.x = element_text(size = 18, family = "serif")
  )+
  coord_flip() 


#Feature importance boxplot
Q1 <- quantile(feature_importance_GB_H$Importance, 0.25)
Q3 <- quantile(feature_importance_GB_H$Importance, 0.75)
IQR <- Q3 - Q1

# Calculate the upper bound
upper_bound <- Q3 + 1.5 * IQR
upper_bound
upperbound <- feature_importance_GB_H %>%
  filter(Importance > upper_bound)
upperbound

Q3
Q3 <- feature_importance_GB_H %>%
  filter(Importance > Q3)
Q3

