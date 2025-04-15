########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

#install.packages("caTools")
#install.packages("xgboost")

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library("xgboost")#for extreme gradient boost
library(Metrics)

comb <- list()
nroundsVector <- c(seq(10,300,10))
max_depthVector <- c(seq(10,100,1))
etaVector <- c(seq(0.01,0.4,0.01))
gammaVector <- c(seq(0.01,0.2,0.01))
lambdaVector <- c(seq(0.1,2,0.1))
alphaVector <- c(seq(0.1,2,0.1))

####################################################################
#Code for hyperparameter tuning
####################################################################
for (i in 1:1000){
  nrounds <- sample(nroundsVector, 1)
  max_depth <- sample(max_depthVector, 1)
  eta <- sample(etaVector, 1)
  gamma <- sample(gammaVector, 1)
  lambda <- sample(lambdaVector, 1)
  alpha <- sample(alphaVector, 1)

  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
  
  XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds, max_depth = max_depth, eta = eta, gamma = gamma, lambda = lambda, alpha = alpha, objective = "binary:logistic")
  prediction <- predict(XGBoost, X_test, type = "response")
  Confusion_matrix <- table(round(prediction), y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(nrounds, max_depth, eta, gamma, lambda, alpha, acc)))
}

#Nrounds hyperparameter
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to amount of iterations",
     xlab = "Nrounds of iterations", ylab = "General model accuracy")
#Max depth hyperparameter
plot(sapply(comb, function(tuple) tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to max depth",
     xlab = "Max Depth", ylab = "General model accuracy")

#Eta hyperparameter
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the learning rate",
     xlab = "eta value", ylab = "General model accuracy")

#Gamma hyperparameter
plot(sapply(comb, function(tuple)  tuple[4]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the gamma value",
     xlab = "gamma value", ylab = "General model accuracy")

#Lambda hyperparmeter
plot(sapply(comb, function(tuple)  tuple[5]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to lambda",
     xlab = "lambda", ylab = "General model accuracy")

#Alpha hyperparameter
plot(sapply(comb, function(tuple)  tuple[6]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to alpha",
     xlab = "alpha", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
nrounds <- best_hyperparameters[1]
max_depth <- best_hyperparameters[2]
eta <- best_hyperparameters[3]
gamma <- best_hyperparameters[4]
lambda <- best_hyperparameters[5]
alpha <- best_hyperparameters[6]

####################################################################
#Code for average performance over 1000 iterations
####################################################################
importance_list <- list()
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

  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))



  XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds,
                     max_depth = max_depth, eta = eta, gamma = gamma,
                     lambda = lambda, alpha = alpha,
                     objective = "binary:logistic")
  prediction <- predict(XGBoost, X_test, type = "response")
  Confusion_matrix <- table(round(prediction), y_test)
  importance_list <- c(importance_list, list(xgb.importance(model = XGBoost)))
  
  conf1 <- append(conf1, Confusion_matrix[1])
  conf2 <- append(conf2, Confusion_matrix[2])
  conf3 <- append(conf3, Confusion_matrix[3])
  conf4 <- append(conf4, Confusion_matrix[4])
  
}
print(paste0("specificity: ", round(mean(conf1)/(mean(conf1) + mean(conf2)), 3)) )
print(paste0("Overall Accuracy: ", round((mean(conf1) + mean(conf4))/sum(mean(conf1 + conf2 + conf3 + conf4)), 3)) )
print(paste0("Precision: ", round((mean(conf4)/(mean(conf2) + mean(conf4))), 3)) )
print(paste0("Recall: ", round(mean(conf4)/(mean(conf4) + mean(conf3)), 3)) )

importance_list <- sapply(seq_along(importance_list[[1]]), function(i) mean(sapply(importance_list, '[', i)))
importance_list <- data.frame(coefficients = importance_list, features = names(train)[-length(names(train))])
importance_list <- importance_list[order(importance_list$coefficients),]

#importance plot for frequency
ggplot(importance_list, aes(x = reorder(Feature, -Frequency), y = Frequency)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Frequency (%)") 

#importance plot for information gain
ggplot(importance_list, aes(x = reorder(Feature, -Gain), y = Gain)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Gain (%)") 

#importance plot for cover value 
ggplot(importance_list, aes(x = reorder(Feature, -Cover), y = Cover)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Cover (%)") 

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
    
    y_train <- as.matrix(train$Completewoundhealing)
    X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
    y_test <- as.matrix(test$Completewoundhealing)
    X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
    
    XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds, max_depth = max_depth, eta = eta, gamma = gamma, lambda = lambda, alpha = alpha, objective = "binary:logistic")
    prediction <- round(predict(XGBoost, X_test, type = "response"))
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

write.csv(metrics, "metrics_results_XGBoost.csv", row.names = FALSE)


#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(XGmodel, file = paste0(directory, "/Extreme_gradient_model.rds"))
