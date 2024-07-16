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
library(class) #for k-nearest neighbor

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
K_vector <- c(1:10)
#Hyperparameter tuning
for (i in 1:200){
  k <- sample(K_vector, 1)
  
  y_train <- train$Completewoundhealing
  X_train <- subset(train, select = -c(Completewoundhealing))
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  K_nearest <- knn(X_train, X_test, cl = y_train, k=k)
  Confusion_matrix <- table(K_nearest, y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(k, acc)) )
}

#Shows decrease in accuracy when k-value increases
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to k-values",
     xlab = "K-amount of nearest neighbors", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
k = best_hyperparameters[1]

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
  
  K_nearest <- knn(X_train, X_test, cl = y_train, k = k)
  
  Confusion_matrix <- table(K_nearest, y_test)
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
  
  histogram <- hist(get(metric), main = paste0(metric, " distribution"), sub = "K-nearest neighbor", xlab = paste0(metric, " (%)"))
  histogram
  abline(v = mean(get(metric)) + 3*sd(get(metric)), lty = 2, lwd = 2)
  abline(v = mean(get(metric)) - 3*sd(get(metric)), lty = 2, lwd = 2)
  text(x = mean(get(metric)) + 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) + 3.0*sd(get(metric)), 3)))
  text(x = mean(get(metric)) - 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) - 3.0*sd(get(metric)), 3)))
}

Kmodel <- K_nearest

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
    
    K_nearest <- knn(X_train, X_test, cl = y_train, k = k)
    
    Confusion_matrix <- table(K_nearest, y_test)
    accuracy_score <- append(accuracy_score, (Confusion_matrix[1] + Confusion_matrix[4])/(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4]))
    accuracy_size <- append(accuracy_size, nrow(predictorset)*n/10)
  }
}

plot(accuracy_size, accuracy_score, main = "Increase in accuracy throughout different sizes", sub = "K-nearest neighbor")

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(Kmodel, file = paste0(directory, "/K_nearest_model.rds"))


