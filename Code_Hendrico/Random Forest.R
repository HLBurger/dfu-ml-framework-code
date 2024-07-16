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
library("randomForest")#for random forest algorithm

sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
sample <- append(sample0, sample1)
train = subset(predictorset, sample == TRUE)
test = subset(predictorset, sample == FALSE)

comb <- list()
N_treeVector <- c(seq(100,1000,10))
mtryVector <- c(1:length(predictorset))
Max_DepthVector <- c(seq(10,100,10))

#Hyperparameter tuning
for (i in 1:1000){
  ntree <- sample(N_treeVector, 1)
  mtry <- sample(mtryVector, 1)
  max_depth <- sample(Max_DepthVector, 1)
  
  y_test <- test$Completewoundhealing
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = ntree, mtry = mtry, max_depth = max_depth )
  prediction <- predict(randomforest, X_test, type = "response")
  Confusion_matrix <- table(prediction, y_test)
  
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4]))/sum(mean(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(ntree, mtry, max_depth, acc)) )
  
}

#Shows that there is a probable local optimum at [200, 400] trees.
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to amount of trees",
     xlab = "Amount of decision trees used", ylab = "General model accuracy")

#The increase in features shows a decrease in accuracy
plot(sapply(comb, function(tuple)  tuple[2]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the amount of features",
     xlab = "Feature amount", ylab = "General model accuracy")

#Accuracy shows slight increase in accuracy, although this makes the model more complex. Preferred size is about 10
plot(sapply(comb, function(tuple)  tuple[3]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to the maximum depth of a decision tree",
     xlab = "max depth", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
ntree = best_hyperparameters[1]
mtry = best_hyperparameters[2]
max_depth = best_hyperparameters[3]

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

Specificity <- conf1/(conf1 + conf2)
Accuracy <- (conf1 + conf4)/(conf1 + conf2 + conf3 + conf4)
Precision <- conf4/(conf2 + conf4)
Recall <- conf4/(conf3 + conf4)

for (metric in c("Specificity", "Accuracy", "Precision", "Recall")){
  
  histogram <- hist(get(metric), main = paste0(metric, " distribution"), sub = "Random Forest", xlab = paste0(metric, " (%)"))
  histogram
  abline(v = mean(get(metric)) + 3*sd(get(metric)), lty = 2, lwd = 2)
  abline(v = mean(get(metric)) - 3*sd(get(metric)), lty = 2, lwd = 2)
  text(x = mean(get(metric)) + 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) + 3.0*sd(get(metric)), 3)))
  text(x = mean(get(metric)) - 2.2*sd(get(metric)), y = max(histogram$counts)*2/3 , as.character(round(mean(get(metric)) - 3.0*sd(get(metric)), 3)))
}

Randommodel <- randomforest

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
    
    y_test <- test$Completewoundhealing
    X_test <- subset(test, select = -c(Completewoundhealing))
    
    randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = ntree, mtry = mtry, max_depth = max_depth )
    prediction <- predict(randomforest, X_test, type = "response")
    Confusion_matrix <- table(prediction, y_test)
    accuracy_score <- append(accuracy_score, (Confusion_matrix[1] + Confusion_matrix[4])/(Confusion_matrix[1] + Confusion_matrix[2] + Confusion_matrix[3] + Confusion_matrix[4]))
    accuracy_size <- append(accuracy_size, nrow(predictorset)*n/10)
  }
}

plot(accuracy_size, accuracy_score, main = "Increase in accuracy throughout different sizes", sub = "Random forest algorithm")

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(Randommodel, file = paste0(directory, "/Random_forest_model.rds"))

