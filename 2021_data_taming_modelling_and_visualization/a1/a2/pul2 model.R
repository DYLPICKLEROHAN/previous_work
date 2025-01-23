
library(tidyverse)
library(stringr)
library(dplyr)
pulitzer <- read_csv("C:\\Users\\rohad\\OneDrive\\Documents\\Data science\\Data Taming, modelling and Vizalization_RStudio\\a1\\a2\\pulitzer.csv")
head(pulitzer)


pulitzer <- pulitzer%>%
  mutate(perc_change = str_replace_all(pulitzer$change_0413, "%", ""))
pulitzer$perc_change <- as.integer(pulitzer$perc_change)
pulitzer <- pulitzer[, c(1,2,3,5,6)]
# needs work: pulitzer$percentage_change <- pulitzer$percentage_change/100
head(pulitzer)

colnames(pulitzer)
pulitzer$average_of_04_and_13 <- rowMeans(pulitzer[ , c(2,3)], na.rm=TRUE)
pulitzer <- pulitzer[, c(1,4,2,3,5,6)]
head(pulitzer)
            #__________________________________________________#
pul2 <- pulitzer %>%
   filter( perc_change != "-100")
#re-scaling the percentage to remove the negatives for simple transformation 
#probably won't work because there will be no way to undo the scaling
#at the other end. But maybe the model will be improved if those publishers 
#that appear to be out of print are removed.

ggplot(pul2, aes(perc_change, average_of_04_and_13))+
  geom_point()
ggplot(pul2, aes(perc_change))+
  geom_boxplot()
summary(pul2$perc_change)
sd(pulitzer$perc_change)

ggplot(pul2, aes(log(perc_change+100))) +
  geom_histogram()
pacman::p_load(caret)
BoxCoxTrans(pul2$prizes_9014, pul2$average_of_04_and_13)
with_outliers <-  lm(log(perc_change+100)~log(prizes_9014), data = pulitzer)
without_outliers <- lm(log(perc_change+60)~log(prizes_9014), data = pul2)
summary(with_outliers)
summary(without_outliers)
#removing those values leads to more significance


plot(without_outliers, which = 1)
plot(without_outliers, which = 3)
plot(without_outliers, which = 2)

pul_modelb <- lm((average_of_04_and_13) ~ (prizes_9014), data = pulitzer)
summary(pul_model)


plot(pul_modelb, which = 1)
plot(pul_modelb, which = 3)
plot(pul_modelb, which = 2)

