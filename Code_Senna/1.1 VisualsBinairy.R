library(ggplot2)
# This code provides the visuals of the binairy variables

# the dataset used is the one were patients with unknown wound healing 
# and patients who had a toe pressure lower than 51 and did not have a CT scan
# were excluded from the dataset
# in total there are:
# 344 patients without amputation (within a year)
# 164 with amputation (within a year)
# 144 with no wound healing (within a year)

df <- ImputedDatasetALL

#############################################################################
# BARCHARTS
#############################################################################

#___________________________________________
# Gender
#___________________________________________
ggplot(df, aes(x = factor(Target), fill = factor(Gender))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("Male", "Female")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Gender distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")


#___________________________________________
# Hypertension
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(Hypertension))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No hypertension", "Hypertension")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Hypertension distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# COPD
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(COPD))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No COPD", "COPD")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("COPD distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")



#___________________________________________
# HeartFailure
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(HeartFailure))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No heart failure", "heart failure")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Heart failure distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# Anemia
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(Anemia))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No anemia", "Anemia")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Anemia distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")


#___________________________________________
# neuroischemie
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(Ischemicandorneuropathicwound))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("darkgrey", "royalblue1", "royalblue4"), labels = c("Neuropathy", "Ischemia", "Both")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Wound stage distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# Infection
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(WoundInfection))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 7, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No infection", "Infection")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Infection distribution") +
  ylab("Total count of patients") +
  xlab("")




#___________________________________________
# Tpaff
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(TPaff50))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 7, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("Toe pressure above 50", "Toe pressure below 51")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Toe pressure distribution") +
  ylab("Total count of patients") +
  xlab("")


#___________________________________________
# PAV
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(PAV))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 7, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No PAV", "PAV")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 18, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),  
    axis.title.y = element_text(size = 18, family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("PAV distribution") +
  ylab("Total count of patients") +
  xlab("")



#___________________________________________
# Oclussion
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(Occlusion))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No Occlusion", "Occlusion")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Occlusion distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# Oclussion in upper arteries
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(UpperOcclusion))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No upper occlusion", "Upper occlusion")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Upper occlusion distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# lower Oclussion
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(LowerOcclusion))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No lower occlusion", "Lower occlusion")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Lower occlusion distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# Occlusion in both parts
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(BothOcclusions))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No Occlusion in both", "Occlusion in upper and lower")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Occlusion in both upper and lower arteries distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# Stenosis
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(Stenosis))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No Stenosis", "Stenosis")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Stenosis distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# UpperStenosis
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(UpperStenosis))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No upper stenosis", "Upper stenosis")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Upper stenosis distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")


#___________________________________________
# LowerStenosis
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(LowerStenosis))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No lower stenosis", "Lower stenosis")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("Lower stenosis distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")

#___________________________________________
# CT scan
#___________________________________________

ggplot(df, aes(x = factor(Target), fill = factor(CTA))) +
  geom_bar(position = "stack") +  # Changed position to "stack"
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, family="serif", color = "white") +  # Add count labels
  scale_fill_manual(values = c("royalblue1", "royalblue4"), labels = c("No CT scan", "CT scan")) +
  theme_minimal() +
  labs(
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family="serif", face="bold"),  
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),  
    axis.title.y = element_text(size = 16, family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  scale_x_discrete(labels = c("No Amputation", "Amputation", "No wound healing")) +
  ggtitle("CT scan distribution across Amputation groups") +
  ylab("Total count of patients") +
  xlab("")
