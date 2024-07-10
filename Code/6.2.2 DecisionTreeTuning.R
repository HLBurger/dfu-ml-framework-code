# DecisionTreeClassifier Tuning

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(rpart)
library(mlr)


df_factor <- df
df_factor$Target <- factor(df_factor$Target, levels = c(0, 1, 2))

# make a model for tuning
d.tree.mlr <- makeClassifTask(
  data=df_factor, 
  target="Target"
)

# gridsearch
param_grid_dt <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=seq(6, 40, by = 2)),
  makeDiscreteParam("minbucket", values=seq(2,30, by=2)),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeDiscreteParam("minsplit", values=seq(2, 30, by = 2))
)

# kfoldcross with 5 folds
control_grid <- makeTuneControlRandom(maxit = 300)  
resample = makeResampleDesc("CV", iters = 5)

# optimize accuracy, unfortunatly it is not possible to optimize brier, precision or recall
measure = acc

dt_tuneparam <- tuneParams(learner='classif.rpart', 
                           task=d.tree.mlr, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid_dt, 
                           control=control_grid, 
                           show.info = TRUE)


num_folds <- 1

accuracy_total_DT_H <- auc_score_class0_total_DT_H <- auc_score_class1_total_DT_H <- auc_score_class2_total_DT_H <- rep(0,num_folds)
precision_total_DT_H <- recall_total_DT_H <- auc_scores_total_DT_H <- vector("list", 3)
for (i in 1:3) {
  precision_total_DT_H[[i]] <- recall_total_DT_H[[i]] <- auc_scores_total_DT_H[[i]] <- rep(0, num_folds)
}


df_class0_DT_H <- df_class1_DT_H <- df_class2_DT_H <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_DT_H <- calibration_curve_class1_DT_H <- calibration_curve_class2_DT_H <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

feature_importance_DT_H <- data.frame(matrix(ncol = ncol(df), nrow = 0))
colnames(feature_importance_DT_H) <- colnames(df)
feature_importance_DT_H <- feature_importance_DT_H[, !colnames(feature_importance_DT_H) %in% c("Target")]


folds <- createFolds(df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)

md = 10
mb = 18
c_p = 0.009
ms = 22

# Test hyperparameters on test_set (data the model has not seen before)
for (i in 1:num_folds) {
  train_data <- df
  
  train_data <- process_data(train_data,1)
  
  model_DT_H <- rpart(Target ~ ., data = train_data, 
                      maxdepth=md, minbucket=mb,
                      cp=c_p, minsplit=ms,method = "class")

  # Predictions
  predictions_DT_H <- predict(model_DT_H, newdata = test_set, type="class")
  predicted_probabilities_DT_H <- predict(model_DT_H, newdata = test_set, type = "prob")
  
  # Confusion matrix
  conf_matrix_DT_H <- table(test_set$Target, predictions_DT_H)
  
  # Accuracy
  accuracy_total_DT_H[i] <- sum(diag(conf_matrix_DT_H)) / sum(conf_matrix_DT_H)
  precision <- diag(conf_matrix_DT_H) / rowSums(conf_matrix_DT_H)
  recall <- diag(conf_matrix_DT_H) / colSums(conf_matrix_DT_H)
  
  class_probs_DT_H <- as.data.frame(predicted_probabilities_DT_H)
  class0_probs_DT_H <- class_probs_DT_H[, "0"]
  class1_probs_DT_H <- class_probs_DT_H[, "1"]
  class2_probs_DT_H <- class_probs_DT_H[, "2"]
  
  test_set$Target <- factor(test_set$Target, levels = c("0", "1", "2"))
  
  # ROC curves
  roc_curve_class0_DT_H <- roc(ifelse(test_set$Target == "0", 1, 0), class0_probs_DT_H)
  roc_curve_class1_DT_H <- roc(ifelse(test_set$Target == "1", 1, 0), class1_probs_DT_H)
  roc_curve_class2_DT_H <- roc(ifelse(test_set$Target == "2", 1, 0), class2_probs_DT_H)
  
  # AUC score
  
  auc_score_class0_total_DT_H[i] <- auc(roc_curve_class0_DT_H)
  auc_score_class1_total_DT_H[i] <- auc(roc_curve_class1_DT_H)
  auc_score_class2_total_DT_H[i] <- auc(roc_curve_class2_DT_H)
  
  df_class0_DT_H <- rbind(df_class0_DT_H, data.frame(sensitivity = roc_curve_class0_DT_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class0_DT_H$specificities))
  df_class1_DT_H <- rbind(df_class1_DT_H, data.frame(sensitivity = roc_curve_class1_DT_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class1_DT_H$specificities))
  df_class2_DT_H <- rbind(df_class2_DT_H, data.frame(sensitivity = roc_curve_class2_DT_H$sensitivities, 
                                                 specificity = 1 - roc_curve_class2_DT_H$specificities))
  
  calibration_curve_class0_DT_H <- rbind(calibration_curve_class0_DT_H, data.frame(predicted_prob = class_probs_DT_H[, "0"], 
                                                                               observed_outcome = ifelse(test_set$Target == "0", 1, 0)))
  calibration_curve_class1_DT_H <- rbind(calibration_curve_class1_DT_H, data.frame(predicted_prob = class_probs_DT_H[, "1"], 
                                                                               observed_outcome = ifelse(test_set$Target == "1", 1, 0)))
  calibration_curve_class2_DT_H <- rbind(calibration_curve_class2_DT_H, data.frame(predicted_prob = class_probs_DT_H[, "2"], 
                                                                               observed_outcome = ifelse(test_set$Target == "2", 1, 0)))
  
  for (class_val in 1:3) {
    # Precision
    precision_total_DT_H[[class_val]][i] <- precision[class_val]
    
    # Recall
    recall_total_DT_H[[class_val]][i] <- recall[class_val]
  }
  
  feature_importance <- model_DT_H$variable.importance
  feature_names <- names(feature_importance)
  
  temp_df <- data.frame(matrix(0, ncol = ncol(feature_importance_DT_H), nrow = 1))
  colnames(temp_df) <- colnames(feature_importance_DT_H)
  
  # Match column names and add feature importances to the correct columns
  for (i in 1:length(feature_names)) {
    col_name <- feature_names[i]
    if (col_name %in% colnames(temp_df)) {
      temp_df[1, col_name] <- feature_importance[col_name]
    }
  }
  feature_importance_DT_H <- rbind(feature_importance_DT_H, temp_df)
  
}

##########################################################
# Metrics
##########################################################
average_accuracy_DT_H <- mean(unlist(accuracy_total_DT_H))
average_auc0_DT_H <- mean(unlist(auc_score_class0_total_DT_H))
average_auc1_DT_H <- mean(unlist(auc_score_class1_total_DT_H))
average_auc2_DT_H <- mean(unlist(auc_score_class2_total_DT_H))
average_precision_DT_H <- sapply(precision_total_DT_H, mean)
average_recall_DT_H <- sapply(recall_total_DT_H, mean)

# Brier score
brier_score_class0_DT_H <- mean((calibration_curve_class0_DT_H$predicted_prob - calibration_curve_class0_DT_H$observed_outcome)^2)
brier_score_class1_DT_H <- mean((calibration_curve_class1_DT_H$predicted_prob - calibration_curve_class1_DT_H$observed_outcome)^2)
brier_score_class2_DT_H <- mean((calibration_curve_class2_DT_H$predicted_prob - calibration_curve_class2_DT_H$observed_outcome)^2)

print(paste("Brier Score no amputation:", round(brier_score_class0_DT_H, 3)))
print(paste("Brier Score amputation:", round(brier_score_class1_DT_H, 3)))
print(paste("Brier Score uncertain:", round(brier_score_class2_DT_H, 3)))

print(average_accuracy_DT_H)
print("Scores of amputation prediction")
print(paste("AUC:", round(average_auc0_DT_H, 3)))
print(paste("Recall:", round(average_recall_DT_H[1], 3)))
print(paste("Precision:", round(average_precision_DT_H[1], 3)))

print("Scores of no amputation prediction")
print(paste("AUC:", round(average_auc1_DT_H, 3)))
print(paste("Recall:", round(average_recall_DT_H[2], 3)))
print(paste("Precision:", round(average_precision_DT_H[2], 3)))

print("Scores of uncertain prediction")
print(paste("AUC:", round(average_auc2_DT_H, 3)))
print(paste("Recall:", round(average_recall_DT_H[3], 3)))
print(paste("Precision:", round(average_precision_DT_H[3], 3)))

# metrics_df_new[["DecisionTreeTuning"]] <- c(
#   average_accuracy_DT_H,
#   brier_score_class0_DT_H, average_auc0_DT_H, average_recall_DT_H[1], average_precision_DT_H[1],
#   brier_score_class1_DT_H, average_auc1_DT_H, average_recall_DT_H[2], average_precision_DT_H[2],
#   brier_score_class2_DT_H, average_auc2_DT_H, average_recall_DT_H[3], average_precision_DT_H[3]
# )



##########################################################
# ROC curve
##########################################################
ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(data = df_class0_DT_H, aes(x = specificity, y = sensitivity, color = "No amputation"), se = FALSE) +
  geom_smooth(data = df_class1_DT_H, aes(x = specificity, y = sensitivity, color = "Amputation"), se = FALSE) +
  geom_smooth(data = df_class2_DT_H, aes(x = specificity, y = sensitivity, color = "Uncertain"), se = FALSE) +
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
  geom_smooth(data = calibration_curve_class0_DT_H, aes(x = predicted_prob, y = observed_outcome, color = "No amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class1_DT_H, aes(x = predicted_prob, y = observed_outcome, color = "Amputation"), method = "loess", se = FALSE) +
  geom_smooth(data = calibration_curve_class2_DT_H, aes(x = predicted_prob, y = observed_outcome, color = "Uncertain"), method = "loess", se = FALSE) +
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

feature_importance_long <- gather(feature_importance_DT_H, key = "Feature", value = "Importance")

feature_importance_DT_H <- feature_importance_long %>%
  group_by(Feature) %>%
  summarize(Importance = mean(Importance))

ggplot(data = feature_importance_DT_H, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = paste("Feature Importance of the tuned Decision Tree Model"),
       x = "",
       y = "Mean decrease in Gini score") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 18, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 18, family = "serif"),
        axis.title.x = element_text(size = 18, family = "serif")
  )+
  coord_flip() 

# Get most important features
density_features_DT_H <- data.frame(Importance = feature_importance_DT_H$Importance)

Q1 <- quantile(feature_importance_DT_H$Importance, 0.25)
Q3 <- quantile(feature_importance_DT_H$Importance, 0.75)
IQR <- Q3 - Q1

# Calculate the upper bound
upper_bound <- Q3 + 1.5 * IQR
upper_bound
upperbound <- feature_importance_DT_H %>%
  filter(Importance > upper_bound)
upperbound

Q3
Q3 <- feature_importance_DT_H %>%
  filter(Importance > Q3)
Q3
