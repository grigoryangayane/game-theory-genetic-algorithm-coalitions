#Gayane updated May 2023

library(tidyverse)
library(plyr)

G3<-read.csv("results/200_3_0_09-11-2022_12_28_19.csv",header = TRUE)
View(G3)
G4<-read.csv("results/410_4_0_09-11-2022_10_55_46.csv",header = TRUE)
G5<-read.csv("results/400_5_0_09-11-2022_10_54_18.csv",header = TRUE)
G6<-read.csv("results/200_6_0_21-10-2022_16_34_03.csv",header = TRUE)
G7<-read.csv("results/400_7_0_21-10-2022_16_30_46.csv",header = TRUE)
G8<-read.csv("results/410_8_0_21-10-2022_16_32_46.csv",header = TRUE)
G9<-read.csv("results/924_9_0_21-10-2022_16_33_15.csv",header = TRUE)

View(G3)
colnames(G3)
View(G4)
View(G5)
View(G6)
View(G7)
View(G8)
View(G9)

min(nrow(G3),nrow(G4),nrow(G5),nrow(G6),nrow(G7),nrow(G8),nrow(G9))

##################looking at best values for each data set#############################

best<-rbind(G3[7000,],G4[7000,],G5[7000,],G6[7000,],
            G7[7000,],G8[7000,],G9[7000,])
best

best$Game_Size<-factor(best$Game_Size)

#ggplot(best, aes(x = Game_Size_Factor, y = Suggestions_Mean_of_Mean)) +
  geom_point() 

#ggplot(best, aes(x = Game_Size_Factor, y = Population_Average_number_of_suggestions)) +
  geom_point() 

  
# Correlations
suggestion_mean_mean <- c(5.8, 16.1, 35.49, 205.13, 406.71, 1123.96, 890.18)
population_average_number_suggestion <- c(179.03, 192.86, 43.19, 445.88, 726.54, 1453.26, 1463.05)
Population_Average_time <- c(0.03, 0.05, 0.02,  0.28, 0.52, 1.26, 1.24)

cor(suggestion_mean_mean, Population_Average_time) # 0.98
cor(population_average_number_suggestion, Population_Average_time) # 0.99
cor(suggestion_mean_mean, population_average_number_suggestion) #0.98

# Figure suggestions mean of mean 
  ggplot(best, aes(x = Game_Size, group = 1)) +
    geom_line(aes(y = Suggestions_Mean_of_Mean), color = "blue", size = 1.2) +
    geom_point(aes(y = Suggestions_Mean_of_Mean), color = "red", size = 1.9) +
    theme(
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 9),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(t = 1, l = 1, unit = "cm"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    labs(x = "Game sizes", y = "Suggestions mean of mean") +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10)
    )
  

# Population_Average_number_of_suggestions
  ggplot(best, aes(x=Game_Size,group=1)) + 
    geom_line(aes(y = Population_Average_number_of_suggestions), color = "blue",size=1.2)+
    geom_point(aes(y = Population_Average_number_of_suggestions), color = "red",size=1.9)+
    theme(
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 9),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(t = 1, l = 1, unit = "cm"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    labs(x = "Game sizes", y = "Population average number of suggestions") +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10)
    )
   
#population average time to reach the coalition
ggplot(best, aes(x=Game_Size,group=1)) + 
    geom_line(aes(y = Population_Average_time), color = "blue",size=1.2)+
    geom_point(aes(y = Population_Average_time), color = "red",size=1.9)+
  theme(
    axis.title = element_text(size = 9, face = "bold"),
    axis.text = element_text(size = 9),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Game sizes", y = "Population average time") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
  
  
# population average number of suggestions per generation\
Rtruncated_data<-rbind(G3[1:7000,],G4[1:7000,],G5[1:7000,],G6[1:7000,],
                       G7[1:7000,],G8[1:7000,],G9[1:7000,])
dim(Rtruncated_data)
View(Rtruncated_data)

Rtruncated_data$Game_Size<-factor(Rtruncated_data$Game_Size)

# Population_Average_number_of_suggestions
ggplot(Rtruncated_data, aes(x = Generation, y = Population_Average_number_of_suggestions)) +
  geom_point(aes(color=Game_Size),size=0.7) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position="right",
        legend.title = element_text(size = 10,face="bold"),
        legend.text = element_text(size = 12),
        axis.title=element_text(size=9,face="bold"),
        axis.text=element_text(size=9))+
  guides(color = guide_legend(override.aes = list(size = 1)))+ 
  ylab("Population average number of suggestions") +
  labs(color = "Game sizes") 

# population average time per generation 
ggplot(Rtruncated_data, aes(x = Generation, y = Population_Average_time)) +
  geom_point(aes(color=Game_Size),size=0.7) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position="right",
    legend.title = element_text(size = 10,face="bold"),
    legend.text = element_text(size = 12),
    axis.title=element_text(size=9,face="bold"),
    axis.text=element_text(size=9))+
  guides(color = guide_legend(override.aes = list(size = 1)))+ 
  ylab("Population average time") +
  labs(color = "Game sizes") 


LRtruncated_data<-rbind(G3[1000:7000,],G4[1000:7000,],G5[1000:7000,],G6[1000:7000,],
                        G7[1000:7000,],G8[1000:7000,],G9[1000:7000,])
dim(LRtruncated_data)
View(LRtruncated_data)

LRtruncated_data$Game_Size<-factor(LRtruncated_data$Game_Size)

# truncated sugestion mean of mean per generation
ggplot(LRtruncated_data, aes(x = Generation, y = Suggestions_Mean_of_Mean)) +
  geom_point(aes(color=Game_Size),size=1) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position="right",
    legend.title = element_text(size = 10,face="bold"),
    legend.text = element_text(size = 12),
    axis.title=element_text(size=9,face="bold"),
    axis.text=element_text(size=9))+
  guides(color = guide_legend(override.aes = list(size = 1)))+ 
  ylab("Sugestion mean of mean over generations") +
  labs(color = "Game sizes") 

#population average number of suggestions per generation
ggplot(LRtruncated_data, aes(x = Generation, y = Population_Average_number_of_suggestions)) +
  geom_point(aes(color=Game_Size),size=0.01) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position="right",
    legend.title = element_text(size = 10,face="bold"),
    legend.text = element_text(size = 12),
    axis.title=element_text(size=9,face="bold"),
    axis.text=element_text(size=9))+
  guides(color = guide_legend(override.aes = list(size = 1)))+ 
  ylab("Population average number of suggestions") +
  labs(color = "Game sizes") 


# population average time
ggplot(LRtruncated_data, aes(x = Generation, y = Population_Average_time)) +
  geom_point(aes(color=Game_Size),size=0.01) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 1, l = 1, unit = "cm"),
    axis.line = element_line(color = "black", size = 0.8),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position="right",
    legend.title = element_text(size = 10,face="bold"),
    legend.text = element_text(size = 12),
    axis.title=element_text(size=9,face="bold"),
    axis.text=element_text(size=9))+
  guides(color = guide_legend(override.aes = list(size = 1)))+ 
  ylab("Population average time") +
  labs(color = "Game sizes") 

#########################################################################################
#########################################################################################  
######################################################################################### 

### Old not included in SBP_BRiMS paper
# This graph does not make sense, does it show that that Game 5 did not reach any coalition??
ggplot(best, aes(x=Game_Size,group=1)) + 
  geom_line(aes(y = Population_Average_percent_reached), color = "blue",size=0.4)+
  geom_point(aes(y = Population_Average_percent_reached), color = "blue",size=1)+
  theme(axis.title=element_text(size=6,face="bold"),axis.text=element_text(size=6))+ ylab("Pop_Avg_percent_reached")


##############################Truncating only right side of data to get same number of generations ######################################

Rtruncated_data<-rbind(G3[1:7000,],G4[1:7000,],G5[1:7000,],G6[1:7000,],
                      G7[1:7000,],G8[1:7000,],G9[1:7000,])
dim(Rtruncated_data)
View(Rtruncated_data)

Rtruncated_data$Game_Size<-factor(Rtruncated_data$Game_Size)


#suggestion mean of mean - figure used in IISE 
ggplot(Rtruncated_data, aes(x = Generation, y = Suggestions_Mean_of_Mean)) +
  geom_point(aes(color=Game_Size),size=0.01) +
  theme(legend.position="bottom",legend.title = element_text(size = 4,face="bold"),legend.text = element_text(size = 4),axis.title=element_text(size=6,face="bold"),axis.text=element_text(size=6))+
  guides(color = guide_legend(override.aes = list(size = 0.8)))

# Pop_avg_percent_reached not included in SBP - does not make sense for Game 9
ggplot(Rtruncated_data, aes(x = Generation, y = Population_Average_percent_reached)) +
  geom_point(aes(color=Game_Size),size=0.01) +
  theme(legend.position="bottom",legend.title = element_text(size = 4,face="bold"),legend.text = element_text(size = 4),axis.title=element_text(size=6,face="bold"),axis.text=element_text(size=6))+
  guides(color = guide_legend(override.aes = list(size = 0.8)))+ ylab("Pop_Avg_percent_reached")

####################Truncating both left and right side of data##################################
LRtruncated_data<-rbind(G3[1000:7000,],G4[1000:7000,],G5[1000:7000,],G6[1000:7000,],
            G7[1000:7000,],G8[1000:7000,],G9[1000:7000,])
dim(LRtruncated_data)
View(LRtruncated_data)

LRtruncated_data$Game_Size<-factor(LRtruncated_data$Game_Size)


# do not include population average number reached 
ggplot(LRtruncated_data, aes(x = Generation, y = Population_Average_percent_reached)) +
  geom_point(aes(color=Game_Size),size=0.01) +
  theme(legend.position="bottom",legend.title = element_text(size = 4,face="bold"),legend.text = element_text(size = 4),axis.title=element_text(size=6,face="bold"),axis.text=element_text(size=6))+
  guides(color = guide_legend(override.aes = list(size = 0.8)))+ ylab("Pop_Avg_percent_reached")

