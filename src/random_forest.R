########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library("randomForest")#for random forest algorithm
library(Metrics)

comb <- list()
N_treeVector <- c(seq(10, 300, 10))
mtryVector <- c(1:length(predictorset))
Max_DepthVector <- c(seq(10, 100, 10))

####################################################################
#Code for hyperparameter tuning
####################################################################
for (i in 1:1000){
  ntree <- sample(N_treeVector, 1)
  mtry <- sample(mtryVector, 1)
  max_depth <- sample(Max_DepthVector, 1)
  
  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = ntree, mtry = mtry, max_depth = max_depth )
  prediction <- predict(randomforest, X_test, type = "response")
  Confusion_matrix <- table(prediction, y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(ntree, mtry, max_depth, acc)))

}

#Ntrees hyperparameter
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to amount of trees",
     xlab = "Amount of decision trees used", ylab = "General model accuracy")

#Feature amount hyperparameter
plot(sapply(comb, function(tuple)  tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the amount of features",
     xlab = "Feature amount", ylab = "General model accuracy")

#Depth hyperparameter
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the maximum depth of a decision tree",
     xlab = "max depth", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
ntree <- best_hyperparameters[1]
mtry <- best_hyperparameters[2]
max_depth <- best_hyperparameters[3]

importance_list <- list()
conf1 <- c()
conf2 <- c()
conf3 <- c()
conf4 <- c()
for (i in 1:1000){
  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train = subset(predictorset, sample == TRUE)
  test = subset(predictorset, sample == FALSE)
  
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = ntree, mtry = mtry, max_depth = max_depth )
  prediction <- predict(randomforest, X_test, type = "response")
  Confusion_matrix <- table(prediction, y_test)
  importance_list <- c(importance_list, list(randomforest$importance))
  
  conf1 <- append(conf1, Confusion_matrix[1])
  conf2 <- append(conf2, Confusion_matrix[2])
  conf3 <- append(conf3, Confusion_matrix[3])
  conf4 <- append(conf4, Confusion_matrix[4])
}

importance_list <- sapply(seq_along(importance_list[[1]]), function(i) mean(sapply(importance_list, '[', i)))
importance_list <- data.frame(coefficients = importance_list, features = names(train)[-length(names(train))])
importance_list <- importance_list[order(importance_list$coefficients),]

ggplot(importance_list, aes(x = reorder(features, -coefficients), y = coefficients)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("coefficients") + ylab("Features")


print(paste0("specificity: ", round(mean(conf1)/(mean(conf1) + mean(conf2)), 3)) )
print(paste0("Overall Accuracy: ", round((mean(conf1) + mean(conf4))/sum(mean(conf1 + conf2 + conf3 + conf4)), 3)) )
print(paste0("Precision: ", round((mean(conf4)/(mean(conf2) + mean(conf4))), 3)) )
print(paste0("Recall: ", round(mean(conf4)/(mean(conf4) + mean(conf3)), 3)) )

####################################################################
#Code for metric increase over size
####################################################################

n <- 1
i <- 1
metrics <- data.frame(
  iteration = integer(),
  n_split = numeric(),
  accuracy = numeric(),
  precision = numeric(),
  recall = numeric(),
  auc = numeric(),
  specificity = numeric()
)
for (n in 1:10) {
  for (i in 1:100){
    sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
    sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
    sample <- append(sample0, sample1)
    train = subset(predictorset, sample == TRUE)
    test = subset(predictorset, sample == FALSE)
    sample0 <- sample(nrow(train), size = round(nrow(train)*n/10))
    train <- train[sample0,]
    sample1 <- sample(nrow(test), size = round(nrow(test)*n/10))
    test <- test[sample1,]
    
    y_test <- test$Completewoundhealing
    X_test <- subset(test, select = -c(Completewoundhealing))
    
    randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = ntree, mtry = mtry, max_depth = max_depth )
    prediction <- predict(randomforest, X_test, type = "response")
    tp <- sum(prediction == 1 & y_test == 1)
    tn <- sum(prediction == 0 & y_test == 0)
    fp <- sum(prediction == 1 & y_test == 0)
    fn <- sum(prediction == 0 & y_test == 1)

    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA)
    specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA)
    auc <- auc(y_test, prediction)

    # Store metrics
    metrics <- rbind(metrics, data.frame(
      iteration = i,
      n_split = n,
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      auc = auc,
      specificity = specificity
    ))
  }
}

write.csv(metrics, "metrics_results_RF.csv", row.names = FALSE)

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(randomforest, file = paste0(directory, "/models/RF_model.rds"))

