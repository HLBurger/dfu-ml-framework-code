library(ggplot2)
library(tidyr)
library(dplyr)


LR_data <- read.csv2("metrics_results_LR.csv", sep = ',')
SVM_data <- read.csv2("metrics_results_SVM.csv", sep = ',')
KNN_data <- read.csv2("metrics_results_KNN.csv", sep = ',')
RF_data <- read.csv2("metrics_results_RF.csv", sep = ',')
XGBoost_data <- read.csv2("metrics_results_XGBoost.csv", sep = ',')
BART_data <- read.csv2("metrics_results_BART.csv", sep = ',')
DNN_data <- read.csv2("metrics_results_DNN.csv", sep = ',')

LR_data$model <- "Logistic regression"
SVM_data$model <- "Support vector machine"
KNN_data$model <- "K-nearest neighbor"
RF_data$model <- "Random forest"
XGBoost_data$model <- "Extreme gradient boost"
BART_data$model <- "Bayesian additive regression trees"
DNN_data$model <- "Artificial neural networks"

LR_data$number_of_patients <- 900 * LR_data$n_split / 10
SVM_data$number_of_patients <- 900 * SVM_data$n_split / 10
KNN_data$number_of_patients <- 900 * KNN_data$n_split / 10
RF_data$number_of_patients <- 900 * RF_data$n_split / 10
XGBoost_data$number_of_patients <- 900 * XGBoost_data$n_split / 10
BART_data$number_of_patients <- 900 * BART_data$n_split / 10
DNN_data$number_of_patients <- 900 * DNN_data$n_split / 10

data <- rbind(LR_data, SVM_data, KNN_data, RF_data, XGBoost_data, BART_data, DNN_data)

data_long <- data %>%
  pivot_longer(
    cols = c(accuracy, precision, recall, auc, specificity),
    names_to = "Metric",
    values_to = "Metric_Value"
  )

data_long$Metric <- factor(data_long$Metric,
                      levels = c("accuracy","specificity", "precision", "recall", "auc" ))

data_long$Metric_Value <- as.numeric(data_long$Metric_Value)

Metric_names <- c(
  "accuracy" = bquote("Accuracy"),
  "auc" = bquote("AUC"),
  "precision" = bquote("Precision"),
  "recall" = bquote("Recall"),
  "specificity" = bquote("Specificity")
)

# Create the plot
plot <- ggplot(data_long, aes(x = number_of_patients, y = Metric_Value, color = model)) +
  geom_ribbon(aes(ymin = 0.6, ymax = 0.9), alpha = 0, color = NA) +
  geom_smooth(linewidth = 1, se = FALSE, alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free", ncol = 3,
             labeller = as_labeller(Metric_names)) +
  theme_minimal(
    base_family = "serif"
  ) +
  theme(
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = c(0.83, 0.2),
    legend.box = "horizon",
    legend.text = element_text(size = 10),
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(size = 10, hjust = 1, vjust = 1),
    plot.caption.position = "plot"
  ) +
  labs(
    x = "Number of patients",
    y = "Metric value",
    color = "Models",
    linetype = "Models",
    shape = "Models"
  ) +
  scale_color_manual(values = c(
    "K-nearest neighbor" = "#56B4E9",
    "Logistic regression" = "#E69F00",
    "Random forest" = "#009E73",
    "Support vector machine" = "#D55E00",
    "Extreme gradient boost" = "#CC79A7",
    "Bayesian additive regression trees" = "#999999",
    "Artificial neural networks" = "#a81d0d"
  ))


# Display the plot
print(plot)
ggsave("Metric_plot.png", width = 6.667, height = 5.334, dpi = 300)
