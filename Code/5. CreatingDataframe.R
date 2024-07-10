library(caret)

#Create Test-set to validate all models

df <- ImputedDatasetALL[c("Ageyears", "COPD","Kidneydialysis", "HeartFailure",
                            "Type1", "IDDM", "AverageserumHb", "AverageserumeGFR", "Averagediastolicbloodpressure",
                            "TBI", "Toepressureaffectedside","Woundlocation_Anterior_Tibial_Artery",
                            "Woundlocation_Dorsalis_Pedis", "Woundlocation_Lateral_Calcaneal",
                            "Woundlocation_Lateral_Plantar", "Woundlocation_Medial_Calcaneal", "Woundlocation_Medial_Plantar",
                            "Woundlocation_Posterior_Tibial_Artery",
                            "Texas2", "Texas3", "TexasB", "TexasC", "TexasD", "Neuropathic",
                            "Degreeofsloughmeasurement", "Degreeofnecrotictissuemeasurement",
                            "Woundareameasurement", "LowerOcclusion","UpperOcclusion","LowerStenosis",
                            "Target")]

# function to split a train and test set regarding the original distribution of the Target group
split_data <- function(data, size) {
  set.seed(42)
  test_index <- createDataPartition(data$Target, p = size / nrow(data), list = FALSE, times = 1)
  test_set <- data[test_index, ]
  train_set <- data[-test_index, ]
  return(list(train = train_set, test = test_set))
}

test_size <- 99

train_temp_split <- split_data(df, test_size)

test_set <- train_temp_split$test
table(test_set$Target)

# Test set can now be used as final set to test how well the models perform on data it has not seen before
#test_set$Target <- factor(test_set$Target, levels = c(0, 1, 2))

# df does not contain the test set anymore, so the models can not be trained on data in the test set
df <- train_temp_split$train
table(df$Target)

# A dataframe to save the evaluation metrics scores
metrics_df_new <- data.frame(
  row.names = c("Accuracy", "Brier0","AUC0", "Recall0", "Precision0", "Brier1","AUC1", "Recall1", "Precision1", "Brier2","AUC2", "Recall2", "Precision2")
)
metrics_df_new <- round(metrics_df_new, 3)
