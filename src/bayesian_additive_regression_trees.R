########################################################################
#Insert excel location path between the " "
#install the required packages if not yet done
########################################################################

url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

# Load the library
library(BART)
library("caTools") #for sampling train/test set
library(ggplot2)
library(Metrics)

comb <- list()
n_tree_vector <- c(seq(10, 100, 10))
n_skip_vector <- c(seq(500, 1000, 50))

####################################################################
#Code for hyperparameter tuning
####################################################################
for (i in 1:1000){
  ntree <- sample(n_tree_vector, 1)
  nskip <- sample(n_skip_vector, 1)

  sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
  sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
  sample <- append(sample0, sample1)
  train <- subset(predictorset, sample == TRUE)
  test <- subset(predictorset, sample == FALSE)
  y_train <- as.matrix(train$Completewoundhealing)
  X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
  y_test <- as.matrix(test$Completewoundhealing)
  X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))

  bart_model <- pbart(X_train, y_train, X_test, ntree = ntree, nskip = nskip)
  bart_model$prob.test <- pnorm(bart_model$yhat.test)
  bart_model$prob.test.mean <- apply(bart_model$prob.test, 2, mean)
  Confusion_matrix <- table(round(bart_model$prob.test.mean), y_test)
  acc <- round((mean(Confusion_matrix[1]) + mean(Confusion_matrix[4])) /
                 sum(mean(Confusion_matrix[1] + Confusion_matrix[2] +
                            Confusion_matrix[3] + Confusion_matrix[4])), 3)
  comb <- c(comb, list(c(ntree, nskip, acc)))

}

#Nrounds hyperparameter
plot(sapply(comb, function(tuple)  tuple[1]), sapply(comb, function(tuple) tuple[length(tuple)]), main = "Model accuracy compared to amount of iterations",
     xlab = "Nrounds of iterations", ylab = "General model accuracy")

best_hyperparameters <- comb[[which.max((sapply(comb, function(x) x[length(x)])))]]
ntree <- best_hyperparameters[1]
nskip <- best_hyperparameters[2]

####################################################################
#Code for average performance over 1000 iterations
####################################################################
conf1 <- c()
conf2 <- c()
conf3 <- c()
conf4 <- c()
auc <- c()
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

    bart_model <- pbart(X_train, y_train, X_test, ntree = ntree, nskip = nskip)
    # Normalize z-values into probabilities
    bart_model$prob.test <- pnorm(bart_model$yhat.test)
    bart_model$prob.test.mean <- apply(bart_model$prob.test, 2, mean)



    predictions <- predict(bart_model, X_test)
    Confusion_matrix <- table(round(predictions$prob.test.mean), y_test)
    conf1 <- append(conf1, Confusion_matrix[1])
    conf2 <- append(conf2, Confusion_matrix[2])
    conf3 <- append(conf3, Confusion_matrix[3])
    conf4 <- append(conf4, Confusion_matrix[4])
    auc <- append(auc, auc(y_test, round(predictions$prob.test.mean)) )
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
    y_train <- as.matrix(train$Completewoundhealing)
    X_train <- as.matrix(subset(train, select = -c(Completewoundhealing)))
    y_test <- as.matrix(test$Completewoundhealing)
    X_test <- as.matrix(subset(test, select = -c(Completewoundhealing)))

    bart_model <- pbart(X_train, y_train, X_test, ntree = ntree, nskip = nskip)
    # Normalize z-values into probabilities
    bart_model$prob.test <- pnorm(bart_model$yhat.test)
    bart_model$prob.test.mean <- apply(bart_model$prob.test, 2, mean)

    predictions <- predict(bart_model, X_test)
    prediction <- round(predictions$prob.test.mean)

# Calculate metrics
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

write.csv(metrics, "metrics_results_BART.csv", row.names = FALSE)


#save the model for further use
directory <- dirname(normalizePath(url_excelfile))
saveRDS(bart_model, file = paste0(directory, "/models/BART_model.rds"))


####################################################################
#Density plot for paper
####################################################################
model_prediction <- data.frame("prediction" = bart_model$prob.test[, 23])
density_data <- density(model_prediction$prediction)
y_at_0.5 <- approx(density_data$x, density_data$y, xout = 0.5)$y

ggplot(model_prediction, aes(x = prediction)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = y_at_0.5),
               linetype = "dashed", color = "red", size = 1) +
  labs(x = "Prediction", y = "Density") +
  theme_minimal(base_family = "serif") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(size = 10)
        ) +
  xlim(0, 1)
