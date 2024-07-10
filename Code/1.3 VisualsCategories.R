library(tidyverse)
library(ggplot2)

# This code provides the visuals of the category variables

# the dataset used is the one were patients with unknown wound healing 
# and patients who had a toe pressure lower than 51 and did not have a CT scan
# were excluded from the dataset
# in total there are:
# 344 patients without amputation (within a year)
# 164 with amputation (within a year)
# 144 with no wound healing (within a year)

df <- ImputedDatasetALL
df_amputations <- df[df$Target == 1, ]
df_no_amputations <- df[df$Target == 0, ]
df_uncertain <- df[df$Target == 2, ] #uncertain is the no wound healing group


###########################################################################
# MULTIPLE CATEGORIES
#############################################################################

#_______________________________________
# Texas Wound Classification AMPUTATION/NO AMPUTATION/UNCERTAIN
#______________________________________

total_counts_texas <- colSums(df[, c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3")])

# Calculate total counts of amputations 
amputation_counts_texas <- colSums(df_amputations[, c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3")])

# Calculate percentage of amputations 
amputation_percentages_texas <- amputation_counts_texas / total_counts_texas * 100

# Create data frames for total counts and amputation percentages
total_data_texas <- data.frame(Category = names(total_counts_texas), Count = total_counts_texas)
amputation_data_texas <- data.frame(Category = names(amputation_counts_texas), Count = amputation_counts_texas, Percentage = amputation_percentages_texas)


ggplot() +
  geom_bar(data = total_data_texas, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_texas, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_texas, aes(x = Category, y = Count, label = paste0(round(Percentage, 0), "%")), vjust = -0.5, face="bold",size = 6, family = "serif",color = "black") +
  scale_fill_manual(values = c("royalblue4", "grey"), labels = c("With Amputation", "Total")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),
    axis.title.y = element_text(size = 18, family="serif"),
    axis.title.x = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Severity") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of amputations per texas wound classification")
#__________________________________________________________________
# Calculate total counts of amputations 
no_amputation_counts_texas <- colSums(df_no_amputations[, c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3")])

# Calculate percentage of amputations 
no_amputation_percentages_texas <- no_amputation_counts_texas / total_counts_texas * 100

# Create data frames for total counts and amputation percentages
total_data_texas <- data.frame(Category = names(total_counts_texas), Count = total_counts_texas)
no_amputation_data_texas <- data.frame(Category = names(no_amputation_counts_texas), Count = no_amputation_counts_texas, Percentage = no_amputation_percentages_texas)

ggplot() +
  geom_bar(data = total_data_texas, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_texas, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_texas, aes(x = Category, y = Count, label = paste0(round(Percentage, 0), "%")), vjust = -0.5, face="bold",size = 6, family = "serif",color = "black") +
  scale_fill_manual(values = c("royalblue1", "grey"), labels = c("Without Amputation", "Total")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),
    axis.title.y = element_text(size = 18, family="serif"),
    axis.title.x = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Severity") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of no amputations per texas wound classification")
#______________________________________________________________________
# Calculate total counts of amputations 
uncertain_counts_texas <- colSums(df_uncertain[, c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3")])

# Calculate percentage of amputations 
uncertain_percentages_texas <- uncertain_counts_texas / total_counts_texas * 100

# Create data frames for total counts and amputation percentages
total_data_texas <- data.frame(Category = names(total_counts_texas), Count = total_counts_texas)
uncertain_data_texas <- data.frame(Category = names(uncertain_counts_texas), Count = uncertain_counts_texas, Percentage = uncertain_percentages_texas)

ggplot() +
  geom_bar(data = total_data_texas, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_texas, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_texas, aes(x = Category, y = Count, label = paste0(round(Percentage, 0), "%")), vjust = -0.5, face="bold",size = 6, family = "serif",color = "black") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"), labels = c("No wound healing", "Total")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 18, family="serif"),
    axis.text.x = element_text(size = 16, family="serif"),  
    axis.text.y = element_text(size = 16, family="serif"),
    axis.title.y = element_text(size = 18, family="serif"),
    axis.title.x = element_text(size = 18, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Severity") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of no wound healing per texas wound classification score")


#_______________________________________
# Texas Wound Classification GROUPS 123
#______________________________________

total_counts_texas123 <- colSums(df[, c("Texas1", "Texas2", "Texas3")])

amputation_counts_texas123 <- colSums(df_amputations[, c("Texas1", "Texas2", "Texas3")])
amputation_percentages_texas123 <- amputation_counts_texas123 / total_counts_texas123 * 100
total_data_texas123 <- data.frame(Category = names(total_counts_texas123), Count = total_counts_texas123)
amputation_data_texas123 <- data.frame(Category = names(amputation_counts_texas123), Count = amputation_counts_texas123, Percentage = amputation_percentages_texas123)

ggplot() +
  geom_bar(data = total_data_texas123, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_texas123, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_texas123, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5,face="bold",size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Depth") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per wound depth")
#______________________________________________________________________

no_amputation_counts_texas123 <- colSums(df_no_amputations[, c("Texas1", "Texas2", "Texas3")])
no_amputation_percentages_texas123 <- no_amputation_counts_texas123 / total_counts_texas123 * 100
total_data_texas123 <- data.frame(Category = names(total_counts_texas123), Count = total_counts_texas123)
no_amputation_data_texas123 <- data.frame(Category = names(no_amputation_counts_texas123), Count = no_amputation_counts_texas123, Percentage = no_amputation_percentages_texas123)

ggplot() +
  geom_bar(data = total_data_texas123, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_texas123, aes(x = Category, y = Count, fill = "No Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_texas123, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, face="bold",size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Depth") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per wound depth")
#____________________________________________________________________

uncertain_counts_texas123 <- colSums(df_uncertain[, c("Texas1", "Texas2", "Texas3")])
uncertain_percentages_texas123 <- uncertain_counts_texas123 / total_counts_texas123 * 100
total_data_texas123 <- data.frame(Category = names(total_counts_texas123), Count = total_counts_texas123)
uncertain_data_texas123 <- data.frame(Category = names(uncertain_counts_texas123), Count = uncertain_counts_texas123, Percentage = uncertain_percentages_texas123)

ggplot() +
  geom_bar(data = total_data_texas123, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_texas123, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_texas123, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Depth") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcome per wound depth")

#_______________________________________
# Texas Wound Classification GROUPS ABCD
#______________________________________

total_counts_texasABCD <- colSums(df[, c("TexasA", "TexasB", "TexasC", "TexasD")])

amputation_counts_texasABCD <- colSums(df_amputations[, c("TexasA", "TexasB", "TexasC", "TexasD")])
amputation_percentages_texasABCD <- amputation_counts_texasABCD / total_counts_texasABCD * 100
total_data_texasABCD <- data.frame(Category = names(total_counts_texasABCD), Count = total_counts_texasABCD)
amputation_data_texasABCD <- data.frame(Category = names(amputation_counts_texasABCD), Count = amputation_counts_texasABCD, Percentage = amputation_percentages_texasABCD)

ggplot() +
  geom_bar(data = total_data_texasABCD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_texasABCD, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_texasABCD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per wound stage")
#___________________________________________________________________

no_amputation_counts_texasABCD <- colSums(df_no_amputations[, c("TexasA", "TexasB", "TexasC", "TexasD")])
no_amputation_percentages_texasABCD <- no_amputation_counts_texasABCD / total_counts_texasABCD * 100
total_data_texasABCD <- data.frame(Category = names(total_counts_texasABCD), Count = total_counts_texasABCD)
no_amputation_data_texasABCD <- data.frame(Category = names(no_amputation_counts_texasABCD), Count = no_amputation_counts_texasABCD, Percentage = no_amputation_percentages_texasABCD)

ggplot() +
  geom_bar(data = total_data_texasABCD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_texasABCD, aes(x = Category, y = Count, fill = "No Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_texasABCD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per wound stage")
#___________________________________________________________________
uncertain_counts_texasABCD <- colSums(df_uncertain[, c("TexasA", "TexasB", "TexasC", "TexasD")])
uncertain_percentages_texasABCD <- uncertain_counts_texasABCD / total_counts_texasABCD * 100
total_data_texasABCD <- data.frame(Category = names(total_counts_texasABCD), Count = total_counts_texasABCD)
uncertain_data_texasABCD <- data.frame(Category = names(uncertain_counts_texasABCD), Count = uncertain_counts_texasABCD, Percentage = uncertain_percentages_texasABCD)

ggplot() +
  geom_bar(data = total_data_texasABCD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_texasABCD, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_texasABCD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family = "serif",color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcome per wound stage")

##########################################
# ETHNICITY
############################################

total_counts_ethnicity <- colSums(df[, c("Ethnicity_1", "Ethnicity_2", "Ethnicity_3", "Ethnicity_4", "Ethnicity_5", "Ethnicity_6", "Ethnicity_7", "Ethnicity_8")])

amputation_counts_ethnicity <- colSums(df_amputations[, c("Ethnicity_1", "Ethnicity_2", "Ethnicity_3", "Ethnicity_4", "Ethnicity_5", "Ethnicity_6", "Ethnicity_7", "Ethnicity_8")])
amputation_percentages_ethnicity <- amputation_counts_ethnicity / total_counts_ethnicity * 100
total_data_ethnicity <- data.frame(Category = names(total_counts_ethnicity), Count = total_counts_ethnicity)
amputation_data_ethnicity <- data.frame(Category = names(amputation_counts_ethnicity), Count = amputation_counts_ethnicity, Percentage = amputation_percentages_ethnicity)

category_mapping <- c("Ethnicity_1" = "1", 
                      "Ethnicity_2" = "2",
                      "Ethnicity_3" = "3",
                      "Ethnicity_4" = "4",
                      "Ethnicity_5" = "5",
                      "Ethnicity_6" = "6",
                      "Ethnicity_7" = "7",
                      "Ethnicity_8" = "8")

total_data_ethnicity$Category <- category_mapping[total_data_ethnicity$Category]
amputation_data_ethnicity$Category <- category_mapping[amputation_data_ethnicity$Category]

ggplot() +
  geom_bar(data = total_data_ethnicity, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_ethnicity, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_ethnicity, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family = "serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Ethnicity group") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per ethnicity group")
#_____________________________________________________

no_amputation_counts_ethnicity <- colSums(df_no_amputations[, c("Ethnicity_1", "Ethnicity_2", "Ethnicity_3", "Ethnicity_4", "Ethnicity_5", "Ethnicity_6", "Ethnicity_7", "Ethnicity_8")])
no_amputation_percentages_ethnicity <- no_amputation_counts_ethnicity / total_counts_ethnicity * 100
total_data_ethnicity <- data.frame(Category = names(total_counts_ethnicity), Count = total_counts_ethnicity)
no_amputation_data_ethnicity <- data.frame(Category = names(no_amputation_counts_ethnicity), Count = no_amputation_counts_ethnicity, Percentage = no_amputation_percentages_ethnicity)

category_mapping <- c("Ethnicity_1" = "1", 
                      "Ethnicity_2" = "2",
                      "Ethnicity_3" = "3",
                      "Ethnicity_4" = "4",
                      "Ethnicity_5" = "5",
                      "Ethnicity_6" = "6",
                      "Ethnicity_7" = "7",
                      "Ethnicity_8" = "8")

total_data_ethnicity$Category <- category_mapping[total_data_ethnicity$Category]
no_amputation_data_ethnicity$Category <- category_mapping[no_amputation_data_ethnicity$Category]

ggplot() +
  geom_bar(data = total_data_ethnicity, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_ethnicity, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_ethnicity, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Ethnicity group") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per ethnicity group")
#_____________________________________________________

uncertain_counts_ethnicity <- colSums(df_uncertain[, c("Ethnicity_1", "Ethnicity_2", "Ethnicity_3", "Ethnicity_4", "Ethnicity_5", "Ethnicity_6", "Ethnicity_7", "Ethnicity_8")])
uncertain_percentages_ethnicity <- uncertain_counts_ethnicity / total_counts_ethnicity * 100
total_data_ethnicity <- data.frame(Category = names(total_counts_ethnicity), Count = total_counts_ethnicity)
uncertain_data_ethnicity <- data.frame(Category = names(uncertain_counts_ethnicity), Count = uncertain_counts_ethnicity, Percentage = uncertain_percentages_ethnicity)

category_mapping <- c("Ethnicity_1" = "1", 
                      "Ethnicity_2" = "2",
                      "Ethnicity_3" = "3",
                      "Ethnicity_4" = "4",
                      "Ethnicity_5" = "5",
                      "Ethnicity_6" = "6",
                      "Ethnicity_7" = "7",
                      "Ethnicity_8" = "8")

total_data_ethnicity$Category <- category_mapping[total_data_ethnicity$Category]
uncertain_data_ethnicity$Category <- category_mapping[uncertain_data_ethnicity$Category]

ggplot() +
  geom_bar(data = total_data_ethnicity, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_ethnicity, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_ethnicity, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Ethnicity group") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per ethnicity group")
#_________________________________________________
###################################################
#DIABETES TYPE
####################################################
total_counts_type <- df %>%
  summarise(
    Type_1 = sum(DiabetesType == 1),
    Type_2_without_insulin = sum(DiabetesType == 2),
    Type_2_with_insulin = sum(DiabetesType == 3)
  )

amputation_counts_type <- df_amputations %>%
  summarise(
    Type_1 = sum(DiabetesType == 1),
    Type_2_without_insulin = sum(DiabetesType == 2),
    Type_2_with_insulin = sum(DiabetesType == 3)
  )

amputation_percentages_type <- amputation_counts_type / total_counts_type * 100
total_counts_long_type <- tidyr::pivot_longer(total_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
amputation_counts_long_type <- tidyr::pivot_longer(amputation_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
amputation_percentage_long_type <- tidyr::pivot_longer(amputation_percentages_type, cols = everything(), names_to = "Category", values_to = "Percentage")

amputation_data_type <- merge(amputation_counts_long_type, amputation_percentage_long_type, by = "Category")

category_mapping <- c("Type_1" = "Type 1", "Type_2_without_insulin" = "Type 2 without insulin", "Type_2_with_insulin" = "Type 2 with insulin")

total_counts_long_type$Category <- category_mapping[total_counts_long_type$Category]
amputation_data_type$Category <- category_mapping[amputation_data_type$Category]

ggplot() +
  geom_bar(data = total_counts_long_type, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_type, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_type, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5,family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Diabetes Type") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per diabetes type")
#_____________________________________________________________________

no_amputation_counts_type <- df_no_amputations %>%
  summarise(
    Type_1 = sum(DiabetesType == 1),
    Type_2_without_insulin = sum(DiabetesType == 2),
    Type_2_with_insulin = sum(DiabetesType == 3)
  )


no_amputation_percentages_type <- no_amputation_counts_type / total_counts_type * 100total_counts_long_type <- tidyr::pivot_longer(total_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_counts_long_type <- tidyr::pivot_longer(no_amputation_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_percentage_long_type <- tidyr::pivot_longer(no_amputation_percentages_type, cols = everything(), names_to = "Category", values_to = "Percentage")

no_amputation_data_type <- merge(no_amputation_counts_long_type, no_amputation_percentage_long_type, by = "Category")

category_mapping <- c("Type_1" = "Type 1", "Type_2_without_insulin" = "Type 2 without insulin", "Type_2_with_insulin" = "Type 2 with insulin")

total_counts_long_type$Category <- category_mapping[total_counts_long_type$Category]
no_amputation_data_type$Category <- category_mapping[no_amputation_data_type$Category]

ggplot() +
  geom_bar(data = total_counts_long_type, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_type, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_type, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5,family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Diabetes Type") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per diabetes type")
#___________________________________________________________________

uncertain_counts_type <- df_uncertain %>%
  summarise(
    Type_1 = sum(DiabetesType == 1),
    Type_2_without_insulin = sum(DiabetesType == 2),
    Type_2_with_insulin = sum(DiabetesType == 3)
  )

uncertain_percentages_type <- uncertain_counts_type / total_counts_type * 100
total_counts_long_type <- tidyr::pivot_longer(total_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_counts_long_type <- tidyr::pivot_longer(uncertain_counts_type, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_percentage_long_type <- tidyr::pivot_longer(uncertain_percentages_type, cols = everything(), names_to = "Category", values_to = "Percentage")

uncertain_data_type <- merge(uncertain_counts_long_type, uncertain_percentage_long_type, by = "Category")

category_mapping <- c("Type_1" = "Type 1", "Type_2_without_insulin" = "Type 2 without insulin", "Type_2_with_insulin" = "Type 2 with insulin")

total_counts_long_type$Category <- category_mapping[total_counts_long_type$Category]
uncertain_data_type$Category <- category_mapping[uncertain_data_type$Category]

ggplot() +
  geom_bar(data = total_counts_long_type, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_type, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_type, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Diabetes Type") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per diabetes type")

###########################################
# CRONIC KIDNEY DISEASE
#############################################

total_counts_CKD <- colSums(df[, c("CKD3a", "CKD3b", "CKD4", "CKD5")])

amputation_counts_CKD <- colSums(df_amputations[, c("CKD3a", "CKD3b", "CKD4", "CKD5")])
amputation_percentages_CKD <- amputation_counts_CKD / total_counts_CKD * 100
total_data_CKD <- data.frame(Category = names(total_counts_CKD), Count = total_counts_CKD)
amputation_data_CKD <- data.frame(Category = names(amputation_counts_CKD), Count = amputation_counts_CKD, Percentage = amputation_percentages_CKD)

ggplot() +
  geom_bar(data = total_data_CKD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_CKD, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_CKD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5,family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Cronic Kidney Disease stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per kidney disease stage")
#___________________________________________________________________

no_amputation_counts_CKD <- colSums(df_no_amputations[, c("CKD3a", "CKD3b", "CKD4", "CKD5")])
no_amputation_percentages_CKD <- no_amputation_counts_CKD / total_counts_CKD * 100
total_data_CKD <- data.frame(Category = names(total_counts_CKD), Count = total_counts_CKD)
no_amputation_data_CKD <- data.frame(Category = names(no_amputation_counts_CKD), Count = no_amputation_counts_CKD, Percentage = no_amputation_percentages_CKD)

ggplot() +
  geom_bar(data = total_data_CKD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_CKD, aes(x = Category, y = Count, fill = "No Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_CKD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Chronic Kidney Disease stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per kdiney disease stage")
#___________________________________________________________________
uncertain_counts_CKD <- colSums(df_uncertain[, c("CKD3a", "CKD3b", "CKD4", "CKD5")])
uncertain_percentages_CKD <- uncertain_counts_CKD / total_counts_CKD * 100
total_data_CKD <- data.frame(Category = names(total_counts_CKD), Count = total_counts_CKD)
uncertain_data_CKD <- data.frame(Category = names(uncertain_counts_CKD), Count = uncertain_counts_CKD, Percentage = uncertain_percentages_CKD)

ggplot() +
  geom_bar(data = total_data_CKD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_CKD, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_CKD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5,family="serif", color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Chronic Kidney Disease stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per kidney disease stage")

####################################################################
# COPD
####################################################################
total_counts_COPD <- colSums(df[, c("COPDGOLD1", "COPDGOLD2", "COPDGOLD3", "COPDGOLD4")])

amputation_counts_COPD <- colSums(df_amputations[, c("COPDGOLD1", "COPDGOLD2", "COPDGOLD3", "COPDGOLD4")])
amputation_percentages_COPD <- amputation_counts_COPD / total_counts_COPD * 100
total_data_COPD <- data.frame(Category = names(total_counts_COPD), Count = total_counts_COPD)
amputation_data_COPD <- data.frame(Category = names(amputation_counts_COPD), Count = amputation_counts_COPD, Percentage = amputation_percentages_COPD)

ggplot() +
  geom_bar(data = total_data_COPD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_COPD, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_COPD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("COPD Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per COPD stage")
#___________________________________________________________________
no_amputation_counts_COPD <- colSums(df_no_amputations[, c("COPDGOLD1", "COPDGOLD2", "COPDGOLD3", "COPDGOLD4")])
no_amputation_percentages_COPD <- no_amputation_counts_COPD / total_counts_COPD * 100
total_data_COPD <- data.frame(Category = names(total_counts_COPD), Count = total_counts_COPD)
no_amputation_data_COPD <- data.frame(Category = names(no_amputation_counts_COPD), Count = no_amputation_counts_COPD, Percentage = no_amputation_percentages_COPD)

ggplot() +
  geom_bar(data = total_data_COPD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_COPD, aes(x = Category, y = Count, fill = "No Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_COPD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("COPD Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per COPD stage")
#___________________________________________________________________
uncertain_counts_COPD <- colSums(df_uncertain[, c("COPDGOLD1", "COPDGOLD2", "COPDGOLD3", "COPDGOLD4")])
uncertain_percentages_COPD <- uncertain_counts_COPD / total_counts_COPD * 100
total_data_COPD <- data.frame(Category = names(total_counts_COPD), Count = total_counts_COPD)
uncertain_data_COPD <- data.frame(Category = names(uncertain_counts_COPD), Count = uncertain_counts_COPD, Percentage = uncertain_percentages_COPD)

ggplot() +
  geom_bar(data = total_data_COPD, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_COPD, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_COPD, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5,family="serif", color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("COPD Stage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per COPD stage")


##########################################
# WOUND LOCATION
##########################################

total_counts_location <- colSums(df[, c("Woundlocation#Anterior_Tibial_Artery", 
                                         "Woundlocation#Dorsalis_Pedis", 
                                         "Woundlocation#Posterior_Tibial_Artery", 
                                         "Woundlocation#Medial_Calcaneal",
                                         "Woundlocation#Medial_Plantar",
                                         "Woundlocation#Lateral_Calcaneal",
                                         "Woundlocation#Lateral_Plantar")])

amputation_counts_location <- colSums(df_amputations[, c("Woundlocation#Anterior_Tibial_Artery", 
                                                          "Woundlocation#Dorsalis_Pedis", 
                                                          "Woundlocation#Posterior_Tibial_Artery", 
                                                          "Woundlocation#Medial_Calcaneal",
                                                          "Woundlocation#Medial_Plantar",
                                                          "Woundlocation#Lateral_Calcaneal",
                                                          "Woundlocation#Lateral_Plantar")])

amputation_percentages_location <- amputation_counts_location / total_counts_location * 100
total_data_location <- data.frame(Category = names(total_counts_location), Count = total_counts_location)
amputation_data_location <- data.frame(Category = names(amputation_counts_location), Count = amputation_counts_location, Percentage = amputation_percentages_location)

category_mapping <- c("Woundlocation#Anterior_Tibial_Artery" = "Anterior Tibial Artery", 
                      "Woundlocation#Dorsalis_Pedis" = "Dorsalis Pedis", 
                      "Woundlocation#Posterior_Tibial_Artery" = "Posterior Tibial Artery",
                      "Woundlocation#Medial_Calcaneal" = "Medial Calcaneal",
                      "Woundlocation#Medial_Plantar" = "Medial Plantar",
                      "Woundlocation#Lateral_Calcaneal" = "Lateral Calcaneal",
                      "Woundlocation#Lateral_Plantar" = "Lateral Plantar")

total_data_location$Category <- category_mapping[total_data_location$Category]
amputation_data_location$Category <- category_mapping[amputation_data_location$Category]

ggplot() +
  geom_bar(data = total_data_location, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_location, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_location, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif",color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound location") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per wound location")
#___________________________________________________________________
no_amputation_counts_location <- colSums(df_no_amputations[, c("Woundlocation#Anterior_Tibial_Artery", 
                                                                "Woundlocation#Dorsalis_Pedis", 
                                                                "Woundlocation#Posterior_Tibial_Artery", 
                                                                "Woundlocation#Medial_Calcaneal",
                                                                "Woundlocation#Medial_Plantar",
                                                                "Woundlocation#Lateral_Calcaneal",
                                                                "Woundlocation#Lateral_Plantar")])

no_amputation_percentages_location <- no_amputation_counts_location / total_counts_location * 100
total_data_location <- data.frame(Category = names(total_counts_location), Count = total_counts_location)
no_amputation_data_location <- data.frame(Category = names(no_amputation_counts_location), Count = no_amputation_counts_location, Percentage = no_amputation_percentages_location)

category_mapping <- c("Woundlocation#Anterior_Tibial_Artery" = "Anterior Tibial Artery", 
                      "Woundlocation#Dorsalis_Pedis" = "Dorsalis Pedis", 
                      "Woundlocation#Posterior_Tibial_Artery" = "Posterior Tibial Artery",
                      "Woundlocation#Medial_Calcaneal" = "Medial Calcaneal",
                      "Woundlocation#Medial_Plantar" = "Medial Plantar",
                      "Woundlocation#Lateral_Calcaneal" = "Lateral Calcaneal",
                      "Woundlocation#Lateral_Plantar" = "Lateral Plantar")

total_data_location$Category <- category_mapping[total_data_location$Category]
no_amputation_data_location$Category <- category_mapping[no_amputation_data_location$Category]

ggplot() +
  geom_bar(data = total_data_location, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_location, aes(x = Category, y = Count, fill = "No Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_location, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound location") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per wound location")
#___________________________________________________________________
uncertain_counts_location <- colSums(df_uncertain[, c("Woundlocation#Anterior_Tibial_Artery", 
                                                       "Woundlocation#Dorsalis_Pedis", 
                                                       "Woundlocation#Posterior_Tibial_Artery", 
                                                       "Woundlocation#Medial_Calcaneal",
                                                       "Woundlocation#Medial_Plantar",
                                                       "Woundlocation#Lateral_Calcaneal",
                                                       "Woundlocation#Lateral_Plantar")])

uncertain_percentages_location <- uncertain_counts_location / total_counts_location * 100
total_data_location <- data.frame(Category = names(total_counts_location), Count = total_counts_location)
uncertain_data_location <- data.frame(Category = names(uncertain_counts_location), Count = uncertain_counts_location, Percentage = uncertain_percentages_location)

category_mapping <- c("Woundlocation#Anterior_Tibial_Artery" = "Anterior Tibial Artery", 
                      "Woundlocation#Dorsalis_Pedis" = "Dorsalis Pedis", 
                      "Woundlocation#Posterior_Tibial_Artery" = "Posterior Tibial Artery",
                      "Woundlocation#Medial_Calcaneal" = "Medial Calcaneal",
                      "Woundlocation#Medial_Plantar" = "Medial Plantar",
                      "Woundlocation#Lateral_Calcaneal" = "Lateral Calcaneal",
                      "Woundlocation#Lateral_Plantar" = "Lateral Plantar")

total_data_location$Category <- category_mapping[total_data_location$Category]
uncertain_data_location$Category <- category_mapping[uncertain_data_location$Category]

ggplot() +
  geom_bar(data = total_data_location, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_location, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_location, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("lightgrey", "darkgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Wound location") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per wound location")


#####################################
#SMOKING
#####################################

total_counts_smoke <- df %>%
  summarise(
    "No smoking" = sum(Smoking == 0),
    "Quit smoking" = sum(Smoking == 1),
    "Current smoking" = sum(Smoking == 2)
  )

amputation_counts_smoke <- df_amputations %>%
  summarise(
    "No smoking" = sum(Smoking == 0),
    "Quit smoking" = sum(Smoking == 1),
    "Current smoking" = sum(Smoking == 2)
  )

amputation_percentages_smoke <- amputation_counts_smoke / total_counts_smoke * 100
total_counts_long_smoke <- tidyr::pivot_longer(total_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
amputation_counts_long_smoke <- tidyr::pivot_longer(amputation_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
amputation_percentage_long_smoke <- tidyr::pivot_longer(amputation_percentages_smoke, cols = everything(), names_to = "Category", values_to = "Percentage")

amputation_data_smoke <- merge(amputation_counts_long_smoke, amputation_percentage_long_smoke, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_smoke, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_smoke, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_smoke, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Smoking status") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per smoking status")
#_____________________________________________________________________
no_amputation_counts_smoke <- df_no_amputations %>%
  summarise(
    "No smoking" = sum(Smoking == 0),
    "Quit smoking" = sum(Smoking == 1),
    "Current smoking" = sum(Smoking == 2)
  )

no_amputation_percentages_smoke <- no_amputation_counts_smoke / total_counts_smoke * 100
total_counts_long_smoke <- tidyr::pivot_longer(total_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_counts_long_smoke <- tidyr::pivot_longer(no_amputation_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_percentage_long_smoke <- tidyr::pivot_longer(no_amputation_percentages_smoke, cols = everything(), names_to = "Category", values_to = "Percentage")

no_amputation_data_smoke <- merge(no_amputation_counts_long_smoke, no_amputation_percentage_long_smoke, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_smoke, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_smoke, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_smoke, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Smoking Status") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per smoking status")
#___________________________________________________________________

uncertain_counts_smoke <- df_uncertain %>%
  summarise(
    "No smoking" = sum(Smoking == 0),
    "Quit smoking" = sum(Smoking == 1),
    "Current smoking" = sum(Smoking == 2)
  )

uncertain_percentages_smoke <- uncertain_counts_smoke / total_counts_smoke * 100
total_counts_long_smoke <- tidyr::pivot_longer(total_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_counts_long_smoke <- tidyr::pivot_longer(uncertain_counts_smoke, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_percentage_long_smoke <- tidyr::pivot_longer(uncertain_percentages_smoke, cols = everything(), names_to = "Category", values_to = "Percentage")

uncertain_data_smoke <- merge(uncertain_counts_long_smoke, uncertain_percentage_long_smoke, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_smoke, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_smoke, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_smoke, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"), labels = c("Total", "No wound healing")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Smoking Status") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per smoking status")


#####################################
#ALCOHOL
#####################################

total_counts_alcohol <- df %>%
  summarise(
    "No alcohol use" = sum(Alcoholuse == 0),
    "Quit alcohol use" = sum(Alcoholuse == 1),
    "Current alcohol use" = sum(Alcoholuse == 2)
  )

amputation_counts_alcohol <- df_amputations %>%
  summarise(
    "No alcohol use" = sum(Alcoholuse == 0),
    "Quit alcohol use" = sum(Alcoholuse == 1),
    "Current alcohol use" = sum(Alcoholuse == 2)
  )

amputation_percentages_alcohol <- amputation_counts_alcohol / total_counts_alcohol * 100
total_counts_long_alcohol <- tidyr::pivot_longer(total_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
amputation_counts_long_alcohol <- tidyr::pivot_longer(amputation_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
amputation_percentage_long_alcohol <- tidyr::pivot_longer(amputation_percentages_alcohol, cols = everything(), names_to = "Category", values_to = "Percentage")

amputation_data_alcohol <- merge(amputation_counts_long_alcohol, amputation_percentage_long_alcohol, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_alcohol, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = amputation_data_alcohol, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = amputation_data_alcohol, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue1", "darkgrey"), labels = c("With Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Alcohol usage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of amputations per alcohol usage")
#_____________________________________________________________________

no_amputation_counts_alcohol <- df_no_amputations %>%
  summarise(
    "No alcohol use" = sum(Alcoholuse == 0),
    "Quit alcohol use" = sum(Alcoholuse == 1),
    "Current alcohol use" = sum(Alcoholuse == 2)
  )

no_amputation_percentages_alcohol <- no_amputation_counts_alcohol / total_counts_alcohol * 100
total_counts_long_alcohol <- tidyr::pivot_longer(total_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_counts_long_alcohol <- tidyr::pivot_longer(no_amputation_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
no_amputation_percentage_long_alcohol <- tidyr::pivot_longer(no_amputation_percentages_alcohol, cols = everything(), names_to = "Category", values_to = "Percentage")

no_amputation_data_alcohol <- merge(no_amputation_counts_long_alcohol, no_amputation_percentage_long_alcohol, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_alcohol, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = no_amputation_data_alcohol, aes(x = Category, y = Count, fill = "Amputation"), stat = "identity", alpha = 1) +
  geom_text(data = no_amputation_data_alcohol, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("royalblue4", "darkgrey"), labels = c("Without Amputation", "Total count")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Alcohol usage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of no amputations per alcohol usage")
#___________________________________________________________________

uncertain_counts_alcohol <- df_uncertain %>%
  summarise(
    "No alcohol use" = sum(Alcoholuse == 0),
    "Quit alcohol use" = sum(Alcoholuse == 1),
    "Current alcohol use" = sum(Alcoholuse == 2)
  )
uncertain_percentages_alcohol <- uncertain_counts_alcohol / total_counts_alcohol * 100
total_counts_long_alcohol <- tidyr::pivot_longer(total_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_counts_long_alcohol <- tidyr::pivot_longer(uncertain_counts_alcohol, cols = everything(), names_to = "Category", values_to = "Count")
uncertain_percentage_long_alcohol <- tidyr::pivot_longer(uncertain_percentages_alcohol, cols = everything(), names_to = "Category", values_to = "Percentage")

uncertain_data_alcohol <- merge(uncertain_counts_long_alcohol, uncertain_percentage_long_alcohol, by = "Category")

ggplot() +
  geom_bar(data = total_counts_long_alcohol, aes(x = Category, y = Count, fill = "Total Count"), stat = "identity") +
  geom_bar(data = uncertain_data_alcohol, aes(x = Category, y = Count, fill = "No wound healing"), stat = "identity", alpha = 1) +
  geom_text(data = uncertain_data_alcohol, aes(x = Category, y = Count, label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5, family="serif", color = "black") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"), labels = c("No wound healing", "Total")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family="serif"),
    legend.text = element_text(size = 16, family="serif"),
    axis.text.x = element_text(size = 14, family="serif"),  
    axis.text.y = element_text(size = 14, family="serif"),
    axis.title.y = element_text(size = 16, family="serif"),
    axis.title.x = element_text(size = 16, family="serif"),
    legend.position = "top"
  ) +
  xlab("Alcohol usage") +
  ylab("Count of patients") +
  labs(fill = "") +
  ggtitle("Ratio of number of uncertain outcomes per alcohol usage")

