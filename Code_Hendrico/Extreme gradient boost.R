########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

#install.packages("caTools")
#install.packages("xgboost")

url_excelfile <- "C:/Users/hburger/Downloads/predictorset.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

#changes characters to integers
for (colname in names(predictorset)){
  if (class(predictorset[,colname]) == "character"){
    class(predictorset[,colname]) <- "numeric"
  }
}

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library("xgboost")#for extreme gradient boost 

sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
sample <- append(sample0, sample1)
train = subset(predictorset, sample == TRUE)
test = subset(predictorset, sample == FALSE)
y_train <- as.matrix(train$Completewoundhealing)
X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
y_test <- as.matrix(test$Completewoundhealing)
X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))

comb <- list()
nroundsVector <- c(seq(10,300,10))
max_depthVector <- c(seq(10,100,1))
etaVector <- c(seq(0.01,0.4,0.01))
gammaVector <- c(seq(0.01,0.2,0.01))
lambdaVector <- c(seq(0.1,2,0.1))
alphaVector <- c(seq(0.1,2,0.1))


#Hyperparameter tuning
for (i in 1:1000){
  nrounds <- sample(nroundsVector, 1)
  max_depth <- sample(max_depthVector, 1)
  eta <- sample(etaVector, 1)
  gamma <- sample(gammaVector, 1)
  lambda <- sample(lambdaVector, 1)
  alpha <- sample(alphaVector, 1)
  
  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
  
  XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds, max_depth = max_depth, eta = eta, gamma = gamma, lambda = lambda, alpha = alpha, objective = "binary:logistic")
  prediction <- predict(XGBoost, X_test, type = "response")
  Confusion_matrix <- table(round(prediction), y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(nrounds, max_depth, eta, gamma, lambda, alpha, acc)) )
}

#Shows that model accuracy increases as the amount of iterations increase (200)
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to amount of iterations",
     xlab = "Nrounds of iterations", ylab = "General model accuracy")
#Shows max depth (shows slightly lower values at < 20, but afterwards stays consistant) 50
plot(sapply(comb, function(tuple) tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to max depth",
     xlab = "Max Depth", ylab = "General model accuracy")

#Shows parabolic behavior. (local?) maximum around .3 learning rate
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the learning rate",
     xlab = "eta value", ylab = "General model accuracy")

#Doesn't really matter. Stays mostly constant .1 should be fine
plot(sapply(comb, function(tuple)  tuple[4]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the gamma value",
     xlab = "gamma value", ylab = "General model accuracy")

#Shows slight decrease in accuracy when lambda increases. (.3)
plot(sapply(comb, function(tuple)  tuple[5]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to lambda",
     xlab = "lambda", ylab = "General model accuracy")

#Shows slight decrease in accuracy when alpha increases. (.3)
plot(sapply(comb, function(tuple)  tuple[6]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to alpha",
     xlab = "alpha", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
nrounds <- best_hyperparameters[1]
max_depth <- best_hyperparameters[2]
eta <- best_hyperparameters[3]
gamma <- best_hyperparameters[4]
lambda <- best_hyperparameters[5]
alpha <- best_hyperparameters[6]

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
  
  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
  
  XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds, max_depth = max_depth, eta = eta, gamma = gamma, lambda = lambda, alpha = alpha, objective = "binary:logistic")
  prediction <- predict(XGBoost, X_test, type = "response")
  Confusion_matrix <- table(round(prediction), y_test)
  
  conf1 <- append(conf1, Confusion_matrix[1])
  conf2 <- append(conf2, Confusion_matrix[2])
  conf3 <- append(conf3, Confusion_matrix[3])
  conf4 <- append(conf4, Confusion_matrix[4])
  
}
print(paste0("specificity: ", round(mean(conf1)/(mean(conf1) + mean(conf2)), 3)) )
print(paste0("Overall Accuracy: ", round((mean(conf1) + mean(conf4))/sum(mean(conf1 + conf2 + conf3 + conf4)), 3)) )
print(paste0("Precision: ", round((mean(conf4)/(mean(conf2) + mean(conf4))), 3)) )
print(paste0("Recall: ", round(mean(conf4)/(mean(conf4) + mean(conf3)), 3)) )

Specificity <- conf1/(conf1 + conf2)
Accuracy <- (conf1 + conf4)/(conf1 + conf2 + conf3 + conf4)
Precision <- conf4/(conf2 + conf4)
Recall <- conf4/(conf3 + conf4)

for (metric in c("Specificity", "Accuracy", "Precision", "Recall")){
  
  histogram <- hist(get(metric), main = paste0(metric, " distribution"), sub = "XGBoost", xlab = paste0(metric, " (%)"))
  histogram
  abline(v = mean(get(metric)) + 3*sd(get(metric)), lty = 2, lwd = 2)
  abline(v = mean(get(metric)) - 3*sd(get(metric)), lty = 2, lwd = 2)
  text(x = mean(get(metric)) + 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) + 3.0*sd(get(metric)), 3)))
  text(x = mean(get(metric)) - 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) - 3.0*sd(get(metric)), 3)))
}

XGmodel <- XGBoost

importance_list <- xgb.importance(model = XGBoost)

#importance plot for frequency
ggplot(importance_list, aes(x = reorder(Feature, -Frequency), y = Frequency)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Frequency (%)") 

#importance plot for information gain
ggplot(importance_list, aes(x = reorder(Feature, -Gain), y = Gain)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Gain (%)") 

#importance plot for cover value 
ggplot(importance_list, aes(x = reorder(Feature, -Cover), y = Cover)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features") + ylab("Cover (%)") 


#Test size increase per accuracy 
accuracy_size <- c()
accuracy_score <- c()
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
    
    y_train <- as.matrix(train$Completewoundhealing)
    X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
    y_test <- as.matrix(test$Completewoundhealing)
    X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
    
    XGBoost <- xgboost(data = X_train, label = y_train, nrounds = nrounds, max_depth = max_depth, eta = eta, gamma = gamma, lambda = lambda, alpha = alpha, objective = "binary:logistic")
    prediction <- predict(XGBoost, X_test, type = "response")
    Confusion_matrix <- table(round(prediction), y_test)
    accuracy_score <- append(accuracy_score, (Confusion_matrix[1] + Confusion_matrix[4])/(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4]))
    accuracy_size <- append(accuracy_size, nrow(predictorset)*n/10)
  }
}

plot(accuracy_size, accuracy_score, main = "Increase in accuracy throughout different sizes", sub = "Extreme gradient boost")

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(XGmodel, file = paste0(directory, "/Extreme_gradient_model.rds"))