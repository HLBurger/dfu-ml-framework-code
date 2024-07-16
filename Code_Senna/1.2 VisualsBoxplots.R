library(tidyverse)
library(ggplot2)

# This code provides the visuals of the numeric variables

# the dataset used is the one were patients with unknown wound healing 
# and patients who had a toe pressure lower than 51 and did not have a CT scan
# were excluded from the dataset
# in total there are:
# 344 patients without amputation (within a year)
# 164 with amputation (within a year)
# 144 with no wound healing (within a year)

df <- ImputedDatasetALL


#############################################################################
# BOXPLOTS
#############################################################################

#_____________________________________________
# Age
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Ageyears)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif")
  ) +
  ggtitle("Boxplot of age") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Age in years")

#_____________________________________________
# Amount of alcohol
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Amountofalcohol)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of amount of alcohol") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Amount of alcohol usage")

#_____________________________________________
# Duration
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Durationofdiabetes)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of duration of diabetes") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Duration of diabetes")



#_____________________________________________
# Insulin use
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Durationofinsulinuse)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of duration of insulin use") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Duration of insulin use")


#_____________________________________________
# Systolic bloodspressure boxplot or histogram
# ____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Averagesystolicbloodpressure)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
   ) +
  ggtitle("Boxplot of the average systolic bloodpressure") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average systolic bloodpressure")


#_____________________________________________
# Diastolic blood pressure
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Averagediastolicbloodpressure)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif")
  ) +
  ggtitle("Boxplot of average diastolic blood pressure") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average diastolic blood pressure")


#_____________________________________________
# 02 saturation
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=AverageO2saturationlevel)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of O2 saturation") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("O2 saturation")


#_____________________________________________
# LVEF
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=LVEF)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of LVEF level") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("LVEF level")


#_______________________________________________
# HbA1C
#______________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=AverageserumHbA1c)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of HbA1c level") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average HbA1c level")


#_____________________________________________
# Hb
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=AverageserumHb)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif")
  ) +
  ggtitle("Boxplot of average Hb level") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average Hb level")


#_____________________________________________
# AverageserumeGFR
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=AverageserumeGFR)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of average eGFR level") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average eGFR level")

#_____________________________________________
# Daysbeforetreatment
#_____________________________________________

df %>%
  ggplot( aes(x=factor(Target), y=Daysbeforetreatement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the days before treatment") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Days before treatment")

#without outliers
sorted_df <- df[order(df$Daysbeforetreatement, decreasing = TRUE), ]
filtered_df <- sorted_df[-(1:30), ]

filtered_df %>%
  ggplot(aes(x = factor(Target), y = Daysbeforetreatement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, family = "serif", face = "bold"),
    axis.text.x = element_text(size = 16, family = "serif"),
    axis.text.y = element_text(size = 14, family = "serif"),
    axis.title.y = element_text(size = 16, family = "serif")
  ) +
  ggtitle("Boxplot of the days before treatment") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Days before treatment")

#_________________________________________________
# Wound area
#_________________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Woundareameasurement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the wound area ") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Wound area in cm2")

sorted_df <- df %>% arrange(desc(Woundareameasurement))

# Remove the two highest values
filtered_df <- sorted_df %>% slice(-c(1:3))

# Plot the boxplot with filtered data
filtered_df %>%
  ggplot(aes(x = factor(Target), y = Woundareameasurement)) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, family = "serif", face = "bold"),
    axis.text.x = element_text(size = 16, family = "serif"),
    axis.text.y = element_text(size = 14, family = "serif"),
    axis.title.y = element_text(size = 16, family = "serif")
  ) +
  ggtitle("Boxplot of the wound area") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Wound area in cm2")


#_____________________________________________
# Toepressure
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Toepressureaffectedside)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif")
  ) +
  ggtitle("Boxplot of the toe systolic blood pressure") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average systolic blood pressure")

#_____________________________________________
# Toepressure andere kant
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Toepressurecontralateralside)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the toe systolic blood pressure of the contralateral side") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Average systolic blood pressure")


#_____________________________________________
# Bloodpressuredifference
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Bloodpressuredifference)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the systolic blood pressure difference") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Systolic blood pressure difference")

#_____________________________________________
# TBI arm leg index
#_____________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=TBI)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the toe brachial index") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Toe brachial index")

#_________________________________________________
# Slough tissue
#_________________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Degreeofsloughmeasurement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of the degree of slough ") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Degree of slough")


#_________________________________________________
# Necrotic tissue
#_________________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Degreeofnecrotictissuemeasurement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of degree of necrotic tissue") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Degree of necrotic tissue")

#_________________________________________________
# Red tissue
#_________________________________________________
df %>%
  ggplot( aes(x=factor(Target), y=Degreeofredtissue)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif")
  ) +
  ggtitle("Boxplot of degree of red tissue") +
  scale_x_discrete(labels = c("No amputation", "Amputation", "No wound healing")) +
  xlab("") +
  ylab("Degree of red tissue")


