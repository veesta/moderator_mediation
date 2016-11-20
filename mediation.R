#mediation - unless you have a huge sample size, you should use SEM for mediation 

library(tidyverse)
library(apaTables)

#read a table 

my_data <- read.table("lectureData6060.csv",header=TRUE,sep=",",na.strings=c("NA"))

analytic.data <- my_data %>% select(Exam, Anxiety, Preparation)

analytic.data <- na.omit(analytic.data)



mediation.data <- analytic.data %>% select(Exam, Preparation, Anxiety)

#Exam = outcome, prep = predictor, anxiety = mediator
#the order you selected the categories is the order of your column names 
#you can switch 2 and 3 and have prep be the mediator for exp. 


psych::mediate(y=1,x=2,m=3, data=mediation.data)




