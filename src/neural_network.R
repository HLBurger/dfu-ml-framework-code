########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

# Load the library
library(neuralnet)
library("caTools") #for sampling train/test set
library(ggplot2)
library(Metrics)
library("DescTools")

numeric_cols <- c("Ageyears", "Averagesystolicbloodpressure",
                  "Averagediastolicbloodpressure", "AverageO2saturationlevel",
                  "AverageserumHbA1c", "AverageserumHb", "AverageserumeGFR",
                  "Toepressureaffectedside", "Woundareameasurement")
conf1 <- c()
conf2 <- c()
conf3 <- c()
conf4 <- c()
auc <- c()
brier <- c()

####################################################################
#Code for average performance over 200 iterations
####################################################################
for (i in 1:200){
  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  train[, numeric_cols] <- lapply(train[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
  test[, numeric_cols] <- lapply(test[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
  train[, "Completewoundhealing"] <- factor(train[, "Completewoundhealing"])
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
  
  model_MLP <- neuralnet(
    Completewoundhealing ~ .,
    data = train,
    hidden = c(16, 8),
    linear.output = FALSE,
    act.fct = "logistic",
    lifesign = "full",
    stepmax = 100000,
    threshold = 0.01,
    rep = 5,
  )

  predicted_probabilities_MLP <- predict(model_MLP, X_test)
  predictions_MLP <- apply(predicted_probabilities_MLP, 1, which.max) - 1
  conf_matrix_MLP <- table(y_test, predictions_MLP)
  conf1 <- append(conf1, conf_matrix_MLP[1])
  conf2 <- append(conf2, conf_matrix_MLP[2])
  conf3 <- append(conf3, conf_matrix_MLP[3])
  conf4 <- append(conf4, conf_matrix_MLP[4])
  auc <- append(auc, auc(y_test, predictions_MLP))
  brier <- append(brier, BrierScore(y_test, predicted_probabilities_MLP[, 2]))
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
    train[, numeric_cols] <- lapply(train[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
    test[, numeric_cols] <- lapply(test[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
    train[, "Completewoundhealing"] <- factor(train[, "Completewoundhealing"])
    sample0 <- sample(nrow(train), size = round(nrow(train)*n/10))
    train <- train[sample0,]
    sample1 <- sample(nrow(test), size = round(nrow(test)*n/10))
    test <- test[sample1,]
    y_test <- as.matrix(test$Completewoundhealing)
    X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))
    
    model_MLP <- neuralnet(
      Completewoundhealing ~ .,
      data = train,
      hidden = c(16, 8),
      linear.output = FALSE,
      act.fct = "logistic",
      lifesign = "full",
      stepmax = 1000000,
      threshold = 0.1,
      rep = 1,
    )

    predicted_probabilities_MLP <- predict(model_MLP, X_test)
    predictions_binary <- apply(predicted_probabilities_MLP, 1, which.max) - 1

    # Calculate metrics
    tp <- sum(predictions_binary == 1 & y_test == 1)
    tn <- sum(predictions_binary == 0 & y_test == 0)
    fp <- sum(predictions_binary == 1 & y_test == 0)
    fn <- sum(predictions_binary == 0 & y_test == 1)
    
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA)
    specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA)
    auc <- auc(y_test, predictions_binary)

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

write.csv(metrics, "metrics_results_ANN.csv", row.names = FALSE)

#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(model_MLP, file = paste0(directory, "/models/NN_model.rds"))
