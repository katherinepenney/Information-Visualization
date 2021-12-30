data <- read.csv('C:/Users/Katherine Penney/Documents/Data Visualization/Project/Heartdata.csv')
  
library(lattice)
# What are the most common cholesterol levels in each type of Resting ECG with each of the Chest Pain anaylzed?
dotplot(reorder(RestingECG, Cholesterol) ~ Cholesterol | ChestPainType, data=data, layout = c(1,4), 
        groups = Sex, pch=20, auto.key = TRUE, main= "Title", 
        xlab= "Cholesterol", par.settings = simpleTheme(col=c("pink", "blue")))

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# What is the distribution of resting blood pressure on a scale of cholesterol according to age?
datadf <- data.frame(data)
ggplot(datadf, aes(x= RestingBP, y= Age, size = Cholesterol, fill = ChestPainType )) +
  geom_point(alpha = 0.5, shape = 21, color = "black" )


# What are the levels of cholestoral, hear rate, and resting blood pressure grouped by chest pain type?
HB <- c(rep("ATA" , 3) , rep("NAP" , 3) , rep("ASY" , 3) , rep("TA" , 3) )
HB2 <- rep(c("RestingBP" , "Cholesterol" , "MaxHR") , 4)
value <- abs(rnorm(12 , 0 , 15))
data2 <- data.frame(HB,HB2,value)
# Graph
ggplot(data2, aes(fill=HB, y=value, x=HB2)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Heart Beat Analysis by Chest Pain Type") +
  facet_wrap(~HB) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("Heart Beats")


# What is the age distribution for each chest pain category? 
ggplot(datadf, aes(x=ChestPainType, y=Age)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  ggtitle("Title") + 
  xlab("Chest Pain Type")+
  ylab("Age")


library(tidyverse)
summary(data$Age)

subgroup = function(df, varname) {
  df %>%
    group_by({{varname}}) %>%
    summarise(count = n()) %>%
    mutate(proportion = round(count / sum(count), 3)) %>% 
    arrange(desc(proportion))
}


data %>% count(RestingBP < 60)

data %>% count(Cholesterol==0)

data = data %>% 
  mutate(across(c(RestingBP, Cholesterol), ~ifelse(.x == 0, NA, .x)))

data$heart_disease_status = ifelse(data$HeartDisease == 1, "Heart Disease", "No Heart Disease")
data$ExerciseAngina = ifelse(data$ExerciseAngina == "Y",1L,0L)  

data %>%
  drop_na() %>%
  select(heart_disease_status, Age, RestingBP, Cholesterol, FastingBS, MaxHR, ExerciseAngina, Oldpeak) %>% 
  group_by(heart_disease_status) %>% 
  summarise_all(.funs = "mean")

## Age Distribution of heart disease 
graph <- data %>% ggplot(aes(x=Age, fill=heart_disease_status)) + geom_histogram() + labs(title="Age Distribution")
graph + scale_color_manual(breaks = c("Heart Disease", "No Heart Disease"), values=c("#D47583", "#474FA2"))  


## Resting BP vs Age


graph1 <- data %>% ggplot(aes(x=Age, y=RestingBP, colour = heart_disease_status)) + 
  geom_point() + labs(title="Resting Blood Pressure vs Age") + 
  geom_smooth(aes(x=Age, y=RestingBP,group = heart_disease_status))
graph1 + scale_fill_manual(values=c("#D47583", "#474FA2", "#56B4E9"))
  
graph1 + scale_color_manual(breaks = c("Heart Disease", "No Heart Disease"), values=c("#D47583", "#474FA2"))  
  
  