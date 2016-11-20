##GRAPHING 
library(tidyverse)
library(apaTables)

#Getting the line on the surface + or - one sd of Z, need sd. z first 

#GET ST. DEV 

my_data <- read.csv(file="lectureData6060.csv")

analytic.data <- my_data

analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety, center=T,scale=F)))
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(Preparation, center=T,scale=F)))

interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), 
                             data=analytic.data, na.action=na.exclude)

##GET Z AT PLUS ONE ST. DEV ... 


sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)

#create a new column for z that is x centered by st. dev
  #I don't want 0 to correspond to the mean, subtract all scores by 1 sd. so 0 occurs at plus 1 st. dev 

analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)

#-----------------------------------
#if you want minus 1 you add (+sd.2)

simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude)

summary(simple.slope.plus.1SD)

apa.reg.table(simple.slope.plus.1SD)


#Getting the line on the surface -1 SD
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)

simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD
                             + I(x.centered*z.centered.at.minus.1SD), data=analytic.data,na.action=na.exclude)
                            
summary(simple.slope.minus.1SD)
apa.reg.table(simple.slope.minus.1SD)

#ignore all z centered lines 
#-.397 is b value 
#are the lines significantly different from each other - yes, because there is an interaction 

#3D gaph takes the above b values from the apa.reg table and plug it into 1 line of the 3d graph equation 

###########################      Mediation      #########################################3

head(analytic.data)

#i think now, the variability in exam scores is due solely  anxiety and all preparation is good for is reducing
#your anxiety and your reduced anxiety reduces your exam score 

#psych package 
#wants the COLUMN NUMBERS (Not the name of the dv/iv)

#psych::mediate()

mediation.data <- analytic.data %>% select(Exam, Preparation, Anxiety)

head(mediation.data)
#now just have 3 columns 

psych::mediate(y=1, x=2, m=3, data=mediation.data)

#make the screen big to see the graphic correct
#ab is what matters 
#not significant mediation because 0 is in the ab confidence interval 

#indirect effect = 0 no indirect effect 

#### BARON KENNY
#therapy and attribution must be related 
#attirube predicts satisfaction 
#therapy predicts satisfaction = .76 (sat is predicted by therapy)
#is there a mediation happening; Satisfaction is predicted by therapy + attribution 
  #when you include attributions - then the weight of therapy goes down to .43; attirubtions takes some of this relationship away 
#ab path (CI doesn't overlap with 0=mediation, how wide is it, is it close to o? )