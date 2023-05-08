library(tidyverse)
library(ggplot2)
file <- read.csv("/Users/sathwik/Downloads/Reduced_Access_to_Care_During_COVID-19.csv")
summary(file)
str(file)
view(file)
summary(file)
file <- select(file,-c(Round,Suppression,Significant.1,Significant.2))
file



ggplot(file, aes(x = 'indicator', y = 'Percent '))+
  geom_col()

#Imp(Box plot)
ggplot(file, aes(x = Indicator, y = Percent,fill = Subgroup))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Imp (Bar Plot)
ggplot(file, aes(x = Indicator, y = Percent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Reduced Access to Care During COVID-19",
       x = "Service",
       y = "Percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Imp(Stacked Bar plot)
ggplot(file, aes(x = Subgroup, y = Percent, fill = Response)) +
  geom_bar(stat = "identity") +
  labs(title = "Reduced Access to Care During COVID-19",
       x = "Subgroup",
       y = "Percent",
       fill = "Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(file, aes(x = Percent, fill = Response)) +
  geom_density(alpha = 0.5) +
  labs(title = "Reduced Access to Care During COVID-19",
       x = "Percent",
       y = "Density",
       fill = "Response") 
#Imp (Multiple Linear regression)
model <- lm(Percent ~ Indicator + Group + Subgroup + Response, data = file)
summary(model)



library(randomForest)

model <- randomForest(Percent ~ ., data = file)
print(model)

model <- lm(Response, data = file)
summary(model)

model <- glm(Percent ~ Indicator + Group + Subgroup + Response, data = file)
summary(model)








