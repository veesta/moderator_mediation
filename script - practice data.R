library(tidyverse)
library(apaTables)

#read a excel

my_data <- read_csv("Practicedata.csv")

glimpse(my_data)

#process the data to just select columns we need and drop missing values 

analytic.data <- my_data %>% select(Exam, Anxiety, Preparation)

analytic.data <- na.omit(analytic.data)

psych::pairs.panels(analytic.data)
apa.cor.table(analytic.data)

apa.cor.table(analytic.data, filename="Table1_Correlation.doc", table.number=1)

#create mean centered versions of variables -> make the mean 0 
#facilitates easy graph interpretation 
#if scale = T and center = T = Z SCORES (mean = 0, st. dev = 1)
#if scale = F and center = T = Centered scores (mean = 0)

#add column with mean center anxiety 

analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety,center=T,scale=F)))

#add column with mean center preparation 

analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(Preparation,center=T,scale=F)))

#create 3 predictor variables: x centered, z centered and x*z centered 
#na. action = use list wise deletion - any row as a missing value, drop the row

interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), 
                             data=analytic.data, na.action = na.exclude)


summary(interaction.regression)

apa.reg.table(interaction.regression)

apa.reg.table(interaction.regression, filename="Table2_Regression.doc", table.number=2)

##BLOCK APPROACH 
#most people use a different approach 


block1 <- lm(Exam ~ x.centered + z.centered,data=analytic.data, na.action=na.exclude)
block2 <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)
apa.reg.table(block1, block2)

#regression with interaction term goes up to .34 - Change in R2 increases significantly 
  #sr2 and delta R2 are the same every time - only if you had one variable -> .03

#delta r2 is significant, but the confidence interval contains 0)
#significance is based on the SR, not the SR2, based on the R, not the R2

##SIG TESTING IS BASED ON THE NON-SQUARED VALUE 
#Althought significant, the CI is - to + - not that big of an effect
  #technically significant, inconsistent messaging here which may tell us something 

##R2 = 34% OF THE VARIABILITY IN EXAM GRADES IS DUE TO THIS SET OF PREDICTORS 
#3% OF THE 34% IS DUE TO THE INTERACTION/MODERATION 


#######################

#transform moderator so 0 corresponds to 1 SD above mean, so when you sub 0 in the reg. equation
# you'll get the relationship between predictor and outcome at 1 st. dev high for moderator 

sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)

analytic.data <- analytic.data %>% mutate(z.centered.plus1SD = z.centered - sd.z)

#lower scores to increase zero point to +1 SD 

simple.slope.plus1SD <- lm(Exam ~ x.centered + z.centered.plus1SD + I(x.centered*z.centered.plus1SD),
                           data=analytic.data, na.action=na.exclude)

summary(simple.slope.plus1SD)

apa.reg.table(simple.slope.plus1SD)


#1 SD below the mean 

analytic.data <- analytic.data %>% mutate(z.centered.minus1SD = z.centered + sd.z)

#lower scores to increase zero point to +1 SD 

simple.slope.minus1SD <- lm(Exam ~ x.centered + z.centered.minus1SD + I(x.centered*z.centered.minus1SD),
                           data=analytic.data, na.action=na.exclude)

summary(simple.slope.minus1SD)

apa.reg.table(simple.slope.minus1SD)

##### MAKING A 2D GRAPH 

#pick predictor for x axis 

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

#graph only the data within +1 and -1 sd 

x.axis.range <- seq(-1*sd.x, 1*sd.x, by=.25*sd.x)

#we want two lines - one for high and one for low preparation (moderator)

sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)

z.line.hi= 1*sd.z
z.line.lo= -1*sd.z

#create predicted values for each line - PART 1 

#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)

#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)

#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

#make the graph - PAR 2 

library(ggplot2)

#set default (x,y) variables
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)

#set APA part of graph below
my.plot <- my.plot + theme_classic()

#add lines 

my.plot <- my.plot + theme(axis.line.x = element_line(colour='black',size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour='black',size=0.5, linetype='solid'))

#adjust the axis 
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))

print(my.plot)

#label the lines 

my.plot <- my.plot+annotate("text", x= - 1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x= .9, y = 25, label = "-1 SD Preparation")
#?annotate

#label the axis
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")


print(my.plot)

#SAVE PLOT WITH THE EXPORT FUNCTION AND INSERT INTO WORD

##3D Graph 

summary(interaction.regression)
apa.reg.table(interaction.regression)

#review intercept = 55.58 and all b-weights and sub in --- looking at 2 SD range like you did with 2d graph 

library(MBESS)

#make the plot square (expand=1)
#set the angle (hor.angle=#)
#make plot gray scale
#add axis labels (xlab, zlab, ylab)
#refer to y-axis(exam scores) as the z-axis so you can change the range from 0 to 100 (its just a bug)
#make the regression line width a certain size 

intr.plot(b.0=47.05, b.x=15.01, b.z=9.45, b.xz=22.61,
          x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, 
          
          xlab="Anxiety (mean centered)", zlab="Preparation (mean centered)", ylab="Exam Grade",
          
          expand=1, hor.angle = 60, gray.scale=TRUE,
          
          line.wd = 4, zlim=c(0,100))

##SAVE THE GRAPH ---- export as pdf (side bar)


