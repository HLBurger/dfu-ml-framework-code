########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library("e1071") #for support vector classifier
library(Metrics)

comb <- list()
degree_vector <- c(1:5)
cost_vector <- c((10^-1):(10^2))
gamma_vector <- c((10^-1):(10))

####################################################################
#Code for hyperparameter tuning
####################################################################
for (i in 1:1000){
  degree <- sample(degree_vector, 1)
  cost <- sample(cost_vector, 1)
  gamma <- sample(gamma_vector, 1)

  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_train <- train$Completewoundhealing
  X_train <- subset(train, select = -c(Completewoundhealing))
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))

  SupportVectorMachine <- svm(x = X_train, y = y_train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
  prediction <- predict(SupportVectorMachine, X_test, type = "response")
  Confusion_matrix <- table(prediction, y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(degree, cost, gamma, acc)))
}

#Degree hyperparameter
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to degree-values",
     xlab = "degree", ylab = "General model accuracy")

#Cost hyperparameter
plot(sapply(comb, function(tuple)  tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to cost-values",
     xlab = "cost", ylab = "General model accuracy")

#Gamma hyperparameter
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to gamma-values",
     xlab = "gamma", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]

degree <- best_hyperparameters[1]
cost <- best_hyperparameters[2]
gamma <- best_hyperparameters[3]

####################################################################
#Code for average performance over 1000 iterations
####################################################################
conf1 <- c()
conf2 <- c()
conf3 <- c()
conf4 <- c()
for (i in 1:1000){
  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_train <- train$Completewoundhealing
  X_train <- subset(train, select = -c(Completewoundhealing))
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  SupportVectorMachine <- svm(x = X_train, y = y_train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
  
  prediction <- predict(SupportVectorMachine, X_test )
  Confusion_matrix <- table(prediction, y_test)
  conf1 <- append(conf1, Confusion_matrix[1])
  conf2 <- append(conf2, Confusion_matrix[2])
  conf3 <- append(conf3, Confusion_matrix[3])
  conf4 <- append(conf4, Confusion_matrix[4])
}
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
    train <- subset(predictorset, sample == TRUE)
    test <- subset(predictorset, sample == FALSE)
    sample0 <- sample(nrow(train), size = round(nrow(train)*n/10))
    train <- train[sample0,]
    sample1 <- sample(nrow(test), size = round(nrow(test)*n/10))
    test <- test[sample1,]

    y_train <- train$Completewoundhealing
    X_train <- subset(train, select = -c(Completewoundhealing))
    y_test <- test$Completewoundhealing
    X_test <- subset(test, select = -c(Completewoundhealing))
    
    SupportVectorMachine <- svm(x = X_train, y = y_train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
    
    prediction <- predict(SupportVectorMachine, X_test)
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

write.csv(metrics, "metrics_results_SVM.csv", row.names = FALSE)

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(SupportVectorMachine, file = paste0(directory, "/SVM_model.rds"))