library(nnet)

# Check VIF values

df_VIF <- ImputedDatasetALL[c("Ageyears", "COPD","Kidneydialysis", "HeartFailure",
                            "Type1", "IDDM", "AverageserumHb", "AverageserumeGFR", "Averagediastolicbloodpressure",
                            "TBI", "Toepressureaffectedside","Woundlocation_Anterior_Tibial_Artery",
                            "Woundlocation_Dorsalis_Pedis", "Woundlocation_Lateral_Calcaneal",
                            "Woundlocation_Lateral_Plantar", "Woundlocation_Medial_Calcaneal", "Woundlocation_Medial_Plantar",
                            "Woundlocation_Posterior_Tibial_Artery",
                            "Texas2", "Texas3", "TexasB", "TexasC", "TexasD", "Neuropathic",
                            "Degreeofsloughmeasurement", "Degreeofnecrotictissuemeasurement",
                            "Woundareameasurement", "LowerOcclusion","UpperOcclusion","LowerStenosis",
                            "Target")]

model <- multinom(Target ~ ., data = df_VIF)
vif <- car::vif(model)
vif <- format(vif, scientific = FALSE)
vif_dataframe <- data.frame(Predictor = names(vif), VIF = vif)
vif_dataframe 

# Variables were removed one by one in the order shown in the c list.
df_removed <- df_VIF[, !(names(df_VIF) %in% c("Woundlocation_Dorsalis_Pedis",
                                      "Toepressureaffectedside",
                                      "AverageserumHb",
                                      "Ageyears",
                                      "Averagediastolicbloodpressure",
                                      "Neuropathic",
                                      "TexasD",
                                      "TBI",
                                      "AverageserumeGFR"))]

model <- multinom(Target ~ ., data = df_removed)
vif <- car::vif(model)
vif <- format(vif, scientific = FALSE)
vif_dataframe <- data.frame(Predictor = names(vif), VIF = vif)
vif_dataframe 
