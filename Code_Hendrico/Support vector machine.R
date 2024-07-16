########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

#install.packages("caTools")

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
library("e1071") #for support vector classifier

sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
sample <- append(sample0, sample1)

train = subset(predictorset, sample == TRUE)
test = subset(predictorset, sample == FALSE)
y_train <- train$Completewoundhealing
X_train <- subset(train, select = -c(Completewoundhealing))
y_test <- test$Completewoundhealing
X_test <- subset(test, select = -c(Completewoundhealing))

comb <- list()
degree_vector <- c(1:5)
cost_vector <- c((10^-1):(10^2))
gamma_vector <- c((10^-1):(10))

#Hyperparameter tuning
for (i in 1:1000){
  degree <- sample(degree_vector, 1)
  cost <- sample(cost_vector, 1)
  gamma <- sample(gamma_vector, 1)
  
  SupportVectorMachine <- svm(x = X_train, y = y_train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
  #SupportVectorMachine <- svm(Completewoundhealing ~ ., data = train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
  prediction <- predict(SupportVectorMachine, X_test, type = "response" )
  Confusion_matrix <- table(prediction, y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  
  comb <- c(comb, list(c(degree, cost, gamma, acc) ))
}

#Shows that degree remains roughly the same 
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to degree-values",
     xlab = "degree", ylab = "General model accuracy")

#Shows that accuracy lowers with low cost and equalizes when cost > 10
plot(sapply(comb, function(tuple)  tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to cost-values",
     xlab = "cost", ylab = "General model accuracy")

#Shows that gamma tops at gamma = 1 and subsequently lowers afterwards
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to gamma-values",
     xlab = "gamma", ylab = "General model accuracy")

#These decisions are made based on general accuracy of the model. This changes depending on the situation (discuss with hani and veerle)
best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]

degree = best_hyperparameters[1]
cost = best_hyperparameters[2]
gamma = best_hyperparameters[3]

#bootstrapping for 1000 times average (size depends on conversion rate)
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

Specificity <- conf1/(conf1 + conf2)
Accuracy <- (conf1 + conf4)/(conf1 + conf2 + conf3 + conf4)
Precision <- conf4/(conf2 + conf4)
Recall <- conf4/(conf3 + conf4)

for (metric in c("Specificity", "Accuracy", "Precision", "Recall")){
  
  histogram <- hist(get(metric), main = paste0(metric, " distribution"), sub = "Support vector machine", xlab = paste0(metric, " (%)"))
  histogram
  abline(v = mean(get(metric)) + 3*sd(get(metric)), lty = 2, lwd = 2)
  abline(v = mean(get(metric)) - 3*sd(get(metric)), lty = 2, lwd = 2)
  text(x = mean(get(metric)) + 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) + 3.0*sd(get(metric)), 3)))
  text(x = mean(get(metric)) - 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) - 3.0*sd(get(metric)), 3)))
}

SupportModel <- SupportVectorMachine

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
    
    y_train <- train$Completewoundhealing
    X_train <- subset(train, select = -c(Completewoundhealing))
    y_test <- test$Completewoundhealing
    X_test <- subset(test, select = -c(Completewoundhealing))
    
    SupportVectorMachine <- svm(x = X_train, y = y_train, cost = cost, degree = degree, gamma = gamma, type = "C-classification")
    
    prediction <- predict(SupportVectorMachine, X_test )
    Confusion_matrix <- table(prediction, y_test)
    accuracy_score <- append(accuracy_score, (Confusion_matrix[1] + Confusion_matrix[4])/(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4]))
    accuracy_size <- append(accuracy_size, nrow(predictorset)*n/10)
  }
}

plot(accuracy_size, accuracy_score, main = "Increase in accuracy throughout different sizes", sub = "Support vector machine")

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(SupportModel, file = paste0(directory, "/Support_vector_model.rds"))