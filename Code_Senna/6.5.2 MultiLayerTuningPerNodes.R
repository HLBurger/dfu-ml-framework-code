# Multi Layer Tuning Per amount of nodes

library(caret)
library(nnet)
library(mlr)
library(neuralnet)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)

################################
# Normalize dataframe
################################

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

###############################
# Tune for one hidden layer, with amount of nodes going from 1 to 40.

nodes <- 1:4
num_folds <- 2

accuracy <- auc_score_class0_total_MLP_H <- auc_score_class1_total_MLP_H <- auc_score_class2_total_MLP_H <- rep(0,num_folds)
precision_total_MLP_H <- recall_total_MLP_H <- auc_scores_total_MLP_H <- vector("list", 3)
for (i in 1:3) {
  precision_total_MLP_H[[i]] <- recall_total_MLP_H[[i]] <- auc_scores_total_MLP_H[[i]] <- rep(0, num_folds)
}

df_class0_MLP_H <- df_class1_MLP_H <- df_class2_MLP_H <- data.frame(sensitivity = numeric(0), specificity = numeric(0))
calibration_curve_class0_MLP_H <- calibration_curve_class1_MLP_H <- calibration_curve_class2_MLP_H <- data.frame(predicted_prob = numeric(0), observed_outcome = numeric(0))

folds <- createFolds(normalized_df$Target, k = num_folds, list = TRUE, returnTrain = FALSE)


# Initialize data structures to store metric scores per number of nodes and per class
metrics_per_node_class <- data.frame(
  node = integer(), 
  class = integer(), 
  brier = numeric(),
  auc = numeric(),
  recall_score =numeric(),
  precision_score =numeric(),
  accuracy_score =numeric()
  )

for (num_nodes in nodes) {
  # Initialize metrics for this node
  brier_scores <- rep(0,3)
  #accuracy_fold <- precision_fold <- recall_fold <- auc_fold <- rep(0, 3)
  
  for (i in 1:num_folds) {
    # Split data into train and test sets based on fold
    train_index <- unlist(folds[-i])
    train_data <- normalized_df[train_index, ]
    test_data <- normalized_df[folds[[i]], ]
    
    # Model training
    model_MLP_H <- neuralnet(
      Target ~ .,                    
      data = train_data,             
      hidden = c(num_nodes),   
      linear.output = FALSE,         
      act.fct = "logistic",         
      lifesign = "full",  
      stepmax = 100000
    )
    
    # Predictions
    predicted_probabilities_MLP_H <- predict(model_MLP_H, test_data)
    predictions_MLP_H <- apply(predicted_probabilities_MLP_H, 1, which.max) - 1
    
    # Confusion matrix
    conf_matrix_MLP_H <- table(test_data$Target, predictions_MLP_H)
    
    # Accuracy
    accuracy[i] <- sum(diag(conf_matrix_MLP_H)) / sum(conf_matrix_MLP_H)
    precision <- diag(conf_matrix_MLP_H) / rowSums(conf_matrix_MLP_H)
    recall <- diag(conf_matrix_MLP_H) / colSums(conf_matrix_MLP_H)
    
    class_probs_MLP_H <- as.data.frame(predicted_probabilities_MLP_H)
    class0_probs_MLP_H <- class_probs_MLP_H[, 1]
    class1_probs_MLP_H <- class_probs_MLP_H[, 2]
    class2_probs_MLP_H <- class_probs_MLP_H[, 3]
    
    calibration_curve_class0_MLP_H <- rbind(calibration_curve_class0_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V1"], 
                                                                                       observed_outcome = ifelse(test_data$Target == "0", 1, 0)))
    calibration_curve_class1_MLP_H <- rbind(calibration_curve_class1_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V2"], 
                                                                                       observed_outcome = ifelse(test_data$Target == "1", 1, 0)))
    calibration_curve_class2_MLP_H <- rbind(calibration_curve_class2_MLP_H, data.frame(predicted_prob = class_probs_MLP_H[, "V3"], 
                                                                                       observed_outcome = ifelse(test_data$Target == "2", 1, 0)))
    
    brier_score_class0_MLP_H <- mean((calibration_curve_class0_MLP_H$predicted_prob - calibration_curve_class0_MLP_H$observed_outcome)^2)
    brier_score_class1_MLP_H <- mean((calibration_curve_class1_MLP_H$predicted_prob - calibration_curve_class1_MLP_H$observed_outcome)^2)
    brier_score_class2_MLP_H <- mean((calibration_curve_class2_MLP_H$predicted_prob - calibration_curve_class2_MLP_H$observed_outcome)^2)
    
    brier_scores[1] <- brier_score_class0_MLP_H
    brier_scores[2] <- brier_score_class1_MLP_H
    brier_scores[3] <- brier_score_class2_MLP_H
    
    auc_scores <- c()
    
    for (class_val in 1:3) {
      roc_curve <- roc(ifelse(test_data$Target == class_val - 1, 1, 0), class_probs_MLP_H[, class_val])
      auc_score <- auc(roc_curve)
      auc_scores <- c(auc_scores, auc_score)
    }
  }
  
  accuracy <- mean(accuracy)

  for (class_val in 1:3) {
    metrics_per_node_class <- rbind(metrics_per_node_class, data.frame(
      node = num_nodes, 
      class = class_val, 
      brier = brier_scores[class_val], 
      auc = auc_scores[class_val],
      recall_score = recall[[class_val]],
      precision_score = precision[[class_val]], 
      accuracy_score = accuracy
      ))
  }
  

}

metrics_MLP0 <- metrics_per_node_class[metrics_per_node_class$class == 1, ]
metrics_MLP1 <- metrics_per_node_class[metrics_per_node_class$class == 2, ]
metrics_MLP2 <- metrics_per_node_class[metrics_per_node_class$class == 3, ]

min_brier_MLP0 <- metrics_MLP0[which.min(metrics_MLP0$brier), ]
min_brier_MLP1 <- metrics_MLP1[which.min(metrics_MLP1$brier), ]
min_brier_MLP2 <- metrics_MLP2[which.min(metrics_MLP2$brier), ]

#################################
# Visuals
#################################

# Minimum brier score per class
ggplot() +
  geom_vline(xintercept = c(1, 2), linetype = "dashed", color = "black") +  
  geom_line(data = metrics_MLP0, aes(x = node, y = brier, color = "No Amputation")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = brier, color = "Amputation")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = brier, color = "No Healing")) +
  geom_point(data = min_brier_MLP0, aes(x = node, y = brier, color = "No Amputation"), size = 4) +
  geom_point(data = min_brier_MLP1, aes(x = node, y = brier, color = "Amputation"), size = 4) +
  geom_point(data = min_brier_MLP2, aes(x = node, y = brier, color = "No Healing"), size = 4) +
  labs(title = "Brier score per node in Multi Layer Perceptron per class",
       x = "Amount of nodes",
       y = "Brier Score",
       color = "Class") +
  scale_color_manual(values = c("No Amputation" = "#c44e30", "Amputation" = "#55a868", "No Healing" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.major = element_blank())+
  #panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = unique(metrics_MLP0$node))

# Max AUC score per class
max_auc_MLP0 <- metrics_MLP0[which.max(metrics_MLP0$auc), ]
max_auc_MLP1 <- metrics_MLP1[which.max(metrics_MLP1$auc), ]
max_auc_MLP2 <- metrics_MLP2[which.max(metrics_MLP2$auc), ]

ggplot() +
  geom_vline(xintercept = c(3,4), linetype = "dashed", color = "black") +  
  geom_line(data = metrics_MLP0, aes(x = node, y = auc, color = "No Amputation")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = auc, color = "Amputation")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = auc, color = "No Healing")) +
  geom_point(data = max_auc_MLP0, aes(x = node, y = auc, color = "No Amputation", label = "Max AUC"), size = 4) +
  geom_point(data = max_auc_MLP1, aes(x = node, y = auc, color = "Amputation", label = "Max AUC"), size = 4) +
  geom_point(data = max_auc_MLP2, aes(x = node, y = auc, color = "No Healing", label = "Max AUC"), size = 4) +
  labs(title = "AUC score per node in Multi Layer Perceptron per class",
       x = "Amount of nodes",
       y = "AUC Score",
       color = "Class") +
  scale_color_manual(values = c("No Amputation" = "#c44e30", "Amputation" = "#55a868", "No Healing" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        #panel.grid.major = element_blank())+
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = unique(metrics_MLP0$node))

# Max recall score per class
max_rec_MLP0 <- metrics_MLP0[which.max(metrics_MLP0$recall), ]
max_rec_MLP1 <- metrics_MLP1[which.max(metrics_MLP1$recall), ]
max_rec_MLP2 <- metrics_MLP2[which.max(metrics_MLP2$recall), ]

ggplot() +
  geom_vline(xintercept = c(3, 4, 5), linetype = "dashed", color = "black") +  
  geom_line(data = metrics_MLP0, aes(x = node, y = recall_score, color = "No Amputation")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = recall_score, color = "Amputation")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = recall_score, color = "No Healing")) +
  geom_point(data = max_rec_MLP0, aes(x = node, y = recall_score, color = "No Amputation"), size = 4) +
  geom_point(data = max_rec_MLP1, aes(x = node, y = recall_score, color = "Amputation"), size = 4) +
  geom_point(data = max_rec_MLP2, aes(x = node, y = recall_score, color = "No Healing"), size = 4) +
  labs(title = "Recall score per node in Multi Layer Perceptron per class",
       x = "Amount of nodes",
       y = "Recall Score",
       color = "Class") +
  scale_color_manual(values = c("No Amputation" = "#c44e30", "Amputation" = "#55a868", "No Healing" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = unique(metrics_MLP0$node))


# precision score per class per node
max_prec_MLP0 <- metrics_MLP0[which.max(metrics_MLP0$precision), ]
max_prec_MLP1 <- metrics_MLP1[which.max(metrics_MLP1$precision), ]
max_prec_MLP2 <- metrics_MLP2[which.max(metrics_MLP2$precision), ]

ggplot() +
  geom_line(data = metrics_MLP0, aes(x = node, y = precision_score, color = "No Amputation")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = precision_score, color = "Amputation")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = precision_score, color = "No Healing")) +
  geom_point(data = max_prec_MLP0, aes(x = node, y = precision_score, color = "No Amputation"), size = 4) +
  geom_point(data = max_prec_MLP1, aes(x = node, y = precision_score, color = "Amputation"), size = 4) +
  geom_point(data = max_prec_MLP2, aes(x = node, y = precision_score, color = "No Healing"), size = 4) +
  labs(title = "Precision score per node in Multi Layer Perceptron per class",
       x = "Amount of nodes",
       y = "AUC Score",
       color = "Class") +
  scale_color_manual(values = c("No Amputation" = "#c44e30", "Amputation" = "#55a868", "No Healing" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.major = element_blank())+
  #panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = unique(metrics_MLP0$node))



# All metrics per class for each amount of nodes in the hidden layer
# for the no amputation class
ggplot() +
  geom_line(data = metrics_MLP0, aes(x = node, y = auc, color = "AUC")) +
  geom_line(data = metrics_MLP0, aes(x = node, y = accuracy, color = "Accuracy")) +
  geom_line(data = metrics_MLP0, aes(x = node, y = recall_score, color = "Recall")) +
  geom_line(data = metrics_MLP0, aes(x = node, y = precision_score, color = "Precision")) +
  labs(title = "Metrics per node in Multi Layer Perceptron for no amputation",
       x = "Amount of nodes",
       y = "Score",
       color = "Class") +
  scale_color_manual(values = c("AUC" = "#c44e30", "Accuracy" = "#55a868", "Recall" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.major = element_blank())+
  scale_x_continuous(breaks = unique(metrics_MLP0$node))

# All metrics per class for each amount of nodes in the hidden layer
# for the amputation class

ggplot() +
  geom_line(data = metrics_MLP1, aes(x = node, y = auc, color = "AUC")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = accuracy, color = "Accuracy")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = recall_score, color = "Recall")) +
  geom_line(data = metrics_MLP1, aes(x = node, y = precision_score, color = "Precision")) +
  labs(title = "Metrics per node in Multi Layer Perceptron for amputation",
       x = "Amount of nodes",
       y = "Score",
       color = "Class") +
  scale_color_manual(values = c("AUC" = "#c44e30", "Accuracy" = "#55a868", "Recall" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.major = element_blank())+
  scale_x_continuous(breaks = unique(metrics_MLP0$node))

# All metrics per class for each amount of nodes in the hidden layer
# for the no wound healing class

ggplot() +
  geom_line(data = metrics_MLP2, aes(x = node, y = auc, color = "AUC")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = accuracy, color = "Accuracy")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = recall_score, color = "Recall")) +
  geom_line(data = metrics_MLP2, aes(x = node, y = precision_score, color = "Precision")) +
  labs(title = "Metrics per node in Multi Layer Perceptron for no wound healing",
       x = "Amount of nodes",
       y = "Score",
       color = "Class") +
  scale_color_manual(values = c("AUC" = "#c44e30", "Accuracy" = "#55a868", "Recall" = "#4c72f2", "Precision"="gold3")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "serif"),
        legend.text = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 14, family = "serif"),
        axis.text.y = element_text(size = 14, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(size = 16, family = "serif"),
        panel.grid.major = element_blank())+
  scale_x_continuous(breaks = unique(metrics_MLP0$node))


