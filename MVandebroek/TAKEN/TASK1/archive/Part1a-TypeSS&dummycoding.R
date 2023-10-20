options(width = 100, useFancyQuotes = FALSE)
rm(list=ls()) 

############################################################
# part 1a: regression
############################################################
#install.packages('ggplot2',dependencies=TRUE)
#install.packages('car',dependencies=TRUE)
#install.packages('corrplot',dependencies=TRUE)
#install.packages('readxl',dependencies=TRUE)
#install.packages('sjPlot',dependencies=TRUE)
#install.packages('gmodels',dependencies=TRUE)
library(ggplot2)
library(car)
library(corrplot)
library(readxl)
library(sjPlot)
library(gmodels)

#set the default directory with setwd, for instance
#setwd("C:/Documents/ATSTAT")  
#make sure the data are in the data subdirectory (here C:\Documents\ATSTAT\data)
rm(list=ls())      
############################################################

#----------------------------------------------------------------------------
# construction example
#----------------------------------------------------------------------------

construction = read.table( "data/construction.txt", header = TRUE)
head(construction)       
str(construction)
summary(construction)

#have a look at the correlation matrix
cormatrix = cor(construction)
cormatrix
#corrplot(cormatrix) #to visualize the correlations (not shown)
#----------------------------------------------------------------------------
# simple regression
#----------------------------------------------------------------------------
par(mar=c(5,5,5,5))
plot(price~size,data=construction, main = "scatterplot", xlab = "size", ylab = "price")
regmod= lm(price~size , data=construction) 
summary(regmod)
#names(regmod)  #to see the names of all components in regmod

#----------------------------------------------------------------------------
# additive model
#----------------------------------------------------------------------------
regmodadd= lm(price~size+distance , data= construction) 
summary(regmodadd)
anova(regmodadd)          #gives type 1 SS = rarely useful!
Anova(regmodadd)          #gives type 2 SS
Anova(regmodadd,type=3)   #gives type 3 SS = type 2 SS as there is no interaction

#----------------------------------------------------------------------------
# model including interaction
#----------------------------------------------------------------------------
modli= lm(price~size * distance , data= construction) 
summary(modli)
anova(modli)          #gives type 1 SS 
Anova(modli)          #gives type 2 SS
Anova(modli,type=3)   #gives type 3 SS 


#----------------------------------------------------------------------------
# using glh to test hypotheses (glh=general linear hypothesis): 
# specify L matrix
#----------------------------------------------------------------------------
#to test the interaction term once more
glh.test(modli,c(0,0,0,1))  

#testing the last 2 parameters jointly
L = rbind(c(0,0,1,0),c(0,0,0,1))
glh.test(modli,L)

#test whether the interaction is significantly different from -0.05
glh.test(modli, c(0,0,0,1),-0.05)

rm(list=ls()) 




#----------------------------------------------------------------------------
# multicollinearity
#----------------------------------------------------------------------------
x1 = c(4,4,4,4,6,6,6,6)
x2 = c(2,2,2,2,3,3,3.01,3)
y  = c(42,39,48,51,49,53,61,60)
datamul = data.frame(cbind(y,x1,x2))

modl= lm(y~., data= datamul) # the . includes the main effects
summary(modl)
anova(modl)
Anova(modl)
vif(modl) 

rm(list=ls()) 

#-----------------------------------------------------------------------
# outlier detection
#-----------------------------------------------------------------------
#read_xlsx reads excel files into a tibble 
#(a tibble is like a data.frame but prints also the type of variables)
outlierdata = read_xlsx("data/outlier_data.xlsx")
head(outlierdata)

plot(outlierdata$x,outlierdata$y)
text(30,49.2,'1',cex=1.0)
text(74,47,'2',cex=1.0)
text(74,11,'3',cex=1.0)

outliermod= lm(y~x,data=outlierdata)

#to get the outlier measures (rounded at 3 digits)
hatv   = round(hatvalues(outliermod), digits = 3)
rstud  = round(rstudent(outliermod), digits = 3)
dffits = round(dffits(outliermod), digits = 3)
cookd  = round(cooks.distance(outliermod), digits = 3)


#check for outliers:
k= 1                        #nr of explanatory variables
n= 36                       #nr of observations
cv_hat = 2*(k+1)/n
cv_rstud = qt(0.95,n-k-2)
if (n<100) {cv_dffits = 1} else {cv_dffits = 2*sqrt((k+1)/n)}
cv_cookd = qf(0.50,k+1,n-k-1)


combined=as.data.frame(cbind(hatv,cv_hat,rstud,cv_rstud,
                             dffits, cv_dffits,cookd, cv_cookd))
head(combined)

outliers = subset(combined, combined$hatv> cv_hat |
                            abs(combined$rstud)> cv_rstud|
                            abs(combined$dffits)> cv_dffits|
                            combined$cookd> cv_cookd)
outliers

rm(list=ls()) 


#----------------------------------------------------------------------------
# using categorical information with R&D data
#----------------------------------------------------------------------------
#we show first that dummy or treatment coding is the default
#in case the coding has been changed, use the next statement to get dummy coding again
#options(contrasts=c("contr.treatment", "contr.poly"))
RandD = read_xlsx("data/R&D.xlsx")
head(RandD)

modli = lm(expenses~revenues*type, data= RandD)
Anova(modli)
#you can check all SS types and the parameter estimates with
anova(modli)
Anova(modli)
Anova(modli,type=3)
summary(modli)
#you can check the coding with
model.matrix(modli)

plot_model(modli,type="int")


#as there is no significant interaction, fit an additive model
modl = lm(expenses~revenues+type, data= RandD)
Anova(modl)

#check the parameter estimates: 
summary(modl)
rm(list=ls()) 

##########################################
#we now switch to sum or effects coding:
#check the different SS and estimates as before!
options(contrasts = c("contr.sum","contr.poly"))

RandD = read_xlsx("data/R&D.xlsx")
head(RandD)

modli = lm(expenses~revenues*type, data= RandD)
Anova(modli)
model.matrix(modli)


#as there is no significant interaction, fit an additive model
modl = lm(expenses~revenues+type, data= RandD)
Anova(modl)

summary(modl)

rm(list=ls()) 


#----------------------------------------------------------------------------
# salary example of the introduction slides
#----------------------------------------------------------------------------
#I show here the results with sum or effects coding, check the differences in 
#type III SS and parameter estimates with dummy or treatment coding
options(contrast = c("contr.treatment","contr.poly"))
#options(contrast = c("contr.sum","contr.poly"))
Salaries = read_xlsx( "data/salaries.xlsx")
head(Salaries)

table(Salaries$Educ)
table(Salaries$Gender)


#simple regression (to show how to visualize the model)
modl= lm(Salary~Age, data= Salaries)
plot(Salaries$Age,Salaries$Salary,pch = 16, cex = 0.3, col = "blue", 
     main = "Salary against age", xlab = "age", ylab = "salary")
abline(modl,lty=5,col="red")

#quadratic regression to show how to do it
modl2= lm(Salary~Age+I(Age^2), data= Salaries)
summary(modl2)

rm(list=ls()) 
#----------------------------------------------------------------------------
# salary with education as categorical information
#----------------------------------------------------------------------------
Salaries = read_xlsx( "data/salaries.xlsx")
Salaries$Educ = as.factor(Salaries$Educ) 
#str(Salaries$Educ) #to check that bachelor is the reference level
Salaries$Educ = relevel(Salaries$Educ, ref = "secondary")
str(Salaries$Educ)
head(Salaries)

modelsali = lm(Salary~ Age * Educ,data = Salaries)
Anova(modelsali)
summary(modelsali)

plot_model(modelsali,type="int")


#check that predictions do not depend on the coding!
#use the same data for modeling and prediction
head(predict(modelsali, Salaries, interval="prediction"))  

#to make predictions for 45 age old bachelors
preddata = data.frame(Educ = "bachelor",Age= 45)

#compute the prediction interval for an individual's salary
predict(modelsali, newdata=preddata,interval="prediction") 

#compute the confidence interval for the mean salary
predict(modelsali, newdata=preddata,interval="confidence") 

rm(list=ls()) 
