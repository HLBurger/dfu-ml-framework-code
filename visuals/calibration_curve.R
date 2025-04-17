url_excelfile <- "processed_data.csv"
predictorset <- read.csv2(url_excelfile, sep = ',')

#changes characters to integers
for (colname in names(predictorset)){
  if (class(predictorset[,colname]) == "character"){
    class(predictorset[,colname]) <- "numeric"
  }
}

library("caTools")
library(class) #for k-nearest neighbor
library(ggplot2)
library("e1071") #for support vector classifier
library("randomForest")#for random forest algorithm
library("xgboost")#for extreme gradient boost
library(BART)
library(neuralnet)
library(dplyr)
library(tidyr)

sample0 <- sample.split(predictorset[predictorset$Completewoundhealing == 0,]$Completewoundhealing, SplitRatio = .7)
sample1 <- sample.split(predictorset[predictorset$Completewoundhealing == 1,]$Completewoundhealing, SplitRatio = .7)
sample <- append(sample0, sample1)
train <- subset(predictorset, sample == TRUE)
test <- subset(predictorset, sample == FALSE)
X_train <- subset(train, select = -c(Completewoundhealing))
y_train <- as.factor(train$Completewoundhealing)
y_test <- as.factor(test$Completewoundhealing)
X_test <- subset(test, select = -c(Completewoundhealing))

LinearModel <- glm(Completewoundhealing ~ ., family = binomial(link = "logit"), data = train)
lin_pred <- predict(LinearModel, X_test, type = "response")

knn_pred <- as.integer(knn(X_train, X_test, cl = y_train, k = 1)) - 1

SupportVectorMachine <- svm(x = X_train, y = y_train, cost = 7.1, degree = 3, gamma = 0.1, probability = TRUE)
svm_pred <- predict(SupportVectorMachine, X_test, probability = TRUE)
svm_pred <- attr(svm_pred, "probabilities")[, "1"]

randomforest <- randomForest(as.factor(Completewoundhealing) ~ ., data = train, ntree = 230, mtry = 3, max_depth = 90 )
rf_pred <- predict(randomforest, X_test, type = "prob", probability = TRUE)[, 2]


y_train_matrix <- as.matrix(train$Completewoundhealing)
X_train_matrix <- as.matrix(subset(train, select = -c(Completewoundhealing)))
y_test_matrix <- as.matrix(test$Completewoundhealing)
X_test_matrix <- as.matrix(subset(test, select = -c(Completewoundhealing)))

XGBoost <- xgboost(data = X_train_matrix, label = y_train_matrix, nrounds = 30,
                   max_depth = 16, eta = 0.23, gamma = 0.19,
                   lambda = 1.60, alpha = 0.30,
                   objective = "binary:logistic")
xgboost_pred <- predict(XGBoost, X_test_matrix, type = "probability")

bart_model <- pbart(X_train_matrix, y_train_matrix, X_test_matrix)
# Normalize z-values into probabilities
bart_model$prob.test <- pnorm(bart_model$yhat.test)
bart_model$prob.test.mean <- apply(bart_model$prob.test, 2, mean)

# Predict with uncertainty for new data
bart_pred <- predict(bart_model, X_test)$prob.test.mean

numeric_cols <- c("Ageyears", "Averagesystolicbloodpressure",
                  "Averagediastolicbloodpressure", "AverageO2saturationlevel",
                  "AverageserumHbA1c", "AverageserumHb", "AverageserumeGFR",
                  "Toepressureaffectedside", "Woundareameasurement")

train[, numeric_cols] <- lapply(train[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
test[, numeric_cols] <- lapply(test[, numeric_cols], function(x) (x - min(x)) / (max(x) - min(x)))
train[, "Completewoundhealing"] <- factor(train[, "Completewoundhealing"])

model_MLP <- neuralnet(
Completewoundhealing ~ .,
data = train,
hidden = c(16, 8),
linear.output = FALSE,
act.fct = "logistic",
lifesign = "full",
stepmax = 100000,
threshold = 0.001,
rep = 5,
)

NN_pred <- predict(model_MLP, X_test, type = "prob", probability = TRUE)[, 2]

cal_data <- data.frame(
  Completewoundhealing = test$Completewoundhealing,
  `Logistic regression` = lin_pred,
  `K-nearest neighbor` = knn_pred,
  `Support vector machine` = svm_pred,
  `Random forest` = rf_pred,
  `Extreme gradient boost` = xgboost_pred,
  `Bayesian additive regression trees` = bart_pred,
  `Artificial neural networks` = NN_pred
)

cal_data_long <- cal_data %>%
  pivot_longer(
    cols = c(Logistic.regression, K.nearest.neighbor, Support.vector.machine,
      Random.forest, Extreme.gradient.boost,
      Bayesian.additive.regression.trees, Artificial.neural.networks
    ),
    names_to = "model",
    values_to = "prob"
  )

plot_data <- cal_data_long %>%
  mutate(bin = cut(prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
  group_by(model, bin) %>%
  summarise(
    mean_pred = mean(prob, na.rm = TRUE),
    obs_freq = mean(Completewoundhealing, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_data, aes(x = mean_pred, y = obs_freq, color = model)) +
  geom_line(size = 0.75) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Frequency"
  ) +
  theme_minimal(base_family = "serif") +
  scale_color_manual(
    values = c(
      "K.nearest.neighbor" = "#56B4E9",
      "Logistic.regression" = "#E69F00",
      "Random.forest" = "#009E73",
      "Support.vector.machine" = "#D55E00",
      "Extreme.gradient.boost" = "#CC79A7",
      "Bayesian.additive.regression.trees" = "#999999",
      "Artificial.neural.networks" = "#a81d0d"
    ),
    labels = c(
      "Artificial.neural.networks" = "Artificial neural networks",
      "Bayesian.additive.regression.trees" = "Bayesian additive regression trees",
      "Extreme.gradient.boost" = "Extreme gradient boost",
      "K.nearest.neighbor" = "K nearest neighbor",
      "Logistic.regression" = "Logistic regression",
      "Random.forest" = "Random forest",
      "Support.vector.machine" = "Support vector machine"
    )
  ) +
  theme(
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = c(0.20, 0.85),
    legend.box = "horizon",
    legend.text = element_text(size = 10),
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(size = 10, hjust = 1, vjust = 1),
    plot.caption.position = "plot"
  )


ggsave("CC_plot.png", width = 6.667, height = 5.334, dpi = 300)
