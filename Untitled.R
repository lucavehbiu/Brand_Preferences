#Import dataset survey complete

library(readr)
install.packages("xlsx", lib = "Desktop")
library(xlsx)
data <- read.table("Desktop/Survey.xlsx")

data <- read.csv("Desktop/Survey.csv", sep= ";" )
data$elevel <- as.factor(data$elevel)
data$car <- as.factor(data$car)
data$zipcode <- as.factor(data$zipcode)
data$brand <- as.factor(data$brand)
str(data)

#Check for outliers
Find_outliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  IQR(data)
  #identify extreme outliers
  extreme.upper.threshold = (IQR(data)*3) + upperq
  extreme.lower.threshold = lowerq - (IQR(data)*3) 
  result = which(data > extreme.upper.threshold | data < extreme.lower.threshold)
}
temp <- Find_outliers(data$brand) #no appearent ones

#Data exploration
library(ggplot2)
library(dplyr)
l <- data %>% group_by(brand, salary, credit) %>% tally()


e <- data %>% group_by(brand, salary, elevel) %>% tally() 
e

e1 <- ggplot(e, aes(elevel, salary, fill = brand)) + geom_violin(scale = "area") + facet_grid(.~brand)
e1

e2 <- ggplot(e, aes(elevel)) + geom_histogram(stat = "count") + theme_minimal() + scale_*_discrete()
e2


l1 <- ggplot(l, aes(salary, fill = brand)) +
  geom_histogram(binwidth = 50, ymax(12)) 
l1


