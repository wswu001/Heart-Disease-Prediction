require(ggplot2)
require(class)
require(caret)
require(MASS)
library(GGally)
library(ggpubr)
library(tidyverse)

########## EDA and Data Visualization ########## 

#prepare the data
df <- read.csv('heart.csv')
df[sapply(df, is.character )] <- lapply(df[sapply(df, is.character)], as.factor)
df$FastingBS <- as.factor(df$FastingBS)
df$HeartDisease <- ifelse(df$HeartDisease == 1, "Y", "N")
df$HeartDisease <- as.factor(df$HeartDisease)

#data overview
summary(df)
prop.table(table(df[(df$Age >= 55),]$HeartDisease))

#scatter plot matrix (Fig 1)
num_df <- df[, c(1,4,5,8,10)]
ggpairs(num_df, legend = 1, aes(color = df$HeartDisease, alpha = 0.4)) +
  theme(legend.position = "bottom") +
  labs(fill = "HeartDisease")

#stacked scatter plots
bp_ct <- ggplot(df, aes(ChestPainType, fill = HeartDisease)) + geom_bar() +  labs(title = "Count by Chest Pain ", x = "Chest Pain Type", y = "Count")
bp_s <- ggplot(df, aes(Sex, fill = HeartDisease)) + geom_bar() +  labs(title = "Count by Sex ", x = "Sex", y = "Count")
bp_f <- ggplot(df, aes(FastingBS, fill = HeartDisease)) + geom_bar() +  labs(title = "Count by Fasting Blood Sugar", x = "FastingBS", y = "Count")
bp_r <- ggplot(df, aes(RestingECG, fill = HeartDisease)) + geom_bar() +  labs(title = "Count by Resting ECG", x = "RestingECG", y = "Count")
bp_e <- ggplot(df, aes(ExerciseAngina, fill = HeartDisease)) + 
  geom_bar() +  labs(title = "Count by Exercise Angina", x = "ExerciseAngina", y = "Count")
bp_s <- ggplot(df, aes(ST_Slope, fill = HeartDisease)) + geom_bar() +  labs(title = "Count by ST_Slope", x = "ST_Slope", y = "Count")
ggarrange(bp_ct, bp_s, bp_f, bp_r, bp_e, bp_s, 
          ncol = 2, nrow = 3, labels = c('A','B', 'C', 'D', 'E', 'F'))

#side-by-side boxplot for cholesterol
p3 <- ggplot(data = heart, mapping = aes(x = HeartDisease, y = heart[,quant_name_index[3]], fill = HeartDisease)) +
  geom_boxplot (alpha = 0.4, outlier.color = "slateblue", outlier.size = 4 ) +
  theme (legend.position = "none" ) +
  scale_x_discrete (labels = paste(c("Normal", "HeartDisease"), "\n(N=", table(heart[,quant_name_index[3]]), ")", sep=" ") ) +
  ggtitle(paste(quant_heart_name[3]), ": side-by-side box plot") +
  xlab("HeartDisease") + ylab(quant_heart_name[3])

p6 <- ggplot(data = heart_rm, mapping = aes(x = HeartDisease, y = heart_rm[,quant_name_index[3]], fill = HeartDisease)) +
  geom_boxplot (alpha = 0.4, 
                outlier.color = "slateblue", 
                outlier.size = 4 ) +
  scale_x_discrete (labels = paste(c("Normal", "HeartDisease"), "\n(N=", table(heart_rm[,quant_name_index[3]]), ")", sep=" ")) +
  ylim(0,600) +
  geom_hline(yintercept = mean(heart_rm$Cholesterol[which(heart_rm$HeartDisease == 1)]),
             linetype = "dashed", alpha = 0.5) +
  ggtitle(paste(quant_heart_name[3]), ": side-by-side box plot") +
  xlab("HeartDisease") + ylab(quant_heart_name[3]) +
  theme (legend.position = "none" ) 
grid.arrange(p3, p6, nrow = 1, ncol = 2)

##Frequency plot
heart$HeartDisease <- as.factor(heart$HeartDisease)
for(i in quant_name_index){
  label <- paste(colnames(heart)[i], ":", "frequency plot")
  p <- ggplot(data = heart, mapping = aes(x = heart[,i], color = HeartDisease))+
    geom_freqpoly()
  print(p + ggtitle(label = label) + xlab(colnames(heart)[i]) + ylab("Count"))
}
