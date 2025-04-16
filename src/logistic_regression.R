########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

library("caTools") #for sampling train/test set
library(ggplot2)  #better viz in general
library(Metrics)
library(dcurves)

comb <- list()
conf1 <- c()
conf2 <- c()
conf3 <- c()
conf4 <- c()
coef_list <- list()

####################################################################
#Code for average performance over 1000 iterations
####################################################################
for (i in 1:1000){
  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_test <- as.factor(test$Completewoundhealing)
  X_test <- subset(test, select = -c(Completewoundhealing))
  
  LinearModel <- glm(Completewoundhealing ~ ., family = binomial(link = "logit"), data = train)
  
  prediction <- predict(LinearModel, X_test, type = "response")
  Confusion_matrix <- table(round(prediction), y_test)
  conf1 <- append(conf1, Confusion_matrix[1])
  conf2 <- append(conf2, Confusion_matrix[2])
  conf3 <- append(conf3, Confusion_matrix[3])
  conf4 <- append(conf4, Confusion_matrix[4])
  coef_list <- append(coef_list, c(list(coef(LinearModel))))
}

coef_list <- lapply(coef_list, function(x) replace(x, is.na(x), 0))
coef_list <- sapply(seq_along(coef_list[[1]]), function(i) mean(sapply(coef_list, '[', i)))
coef_list <- data.frame(coefficients = coef_list[-1], features = names(train)[-length(names(train))])
coef_list <- coef_list[order(coef_list$coefficients),]

#feature importance
ggplot(coef_list, aes(x = reorder(features, -coefficients), y = coefficients)) + geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + xlab("Features")

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

    y_test <- as.factor(test$Completewoundhealing)
    X_test <- subset(test, select = -c(Completewoundhealing))

    LinearModel <- glm(Completewoundhealing ~ ., family = binomial(link = "logit"), data = train)
    prediction <- round(predict(LinearModel, X_test, type = "response" ))
    tp <- sum(prediction == 1 & y_test == 1)
    tn <- sum(prediction == 0 & y_test == 0)
    fp <- sum(prediction == 1 & y_test == 0)
    fn <- sum(prediction == 0 & y_test == 1)

    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA)
    specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA)
    auc <- auc(y_test, round(prediction))

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

write.csv(metrics, "metrics_results_LR.csv", row.names = FALSE)

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(LinearModel, file = paste0(directory, "/models/LR_model.rds"))
