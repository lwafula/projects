options(width = 100, useFancyQuotes = FALSE)
rm(list=ls()) 

###########################################################
# part 3b: duration or survival analysis
###########################################################
#install.packages('readxl',dependencies=TRUE)
#install.packages('survival',dependencies=TRUE)
#install.packages('ggfortify',dependencies=TRUE)
#install.packages('survminer',dependencies=TRUE)

library(readxl)
library(survival)
library(ggfortify)
library(survminer)

#set the default directory with setwd, for instance
#setwd("C:/Documents/ATSTAT")  
#make sure the data are in the data subdirectory (here C:\Documents\ATSTAT\data)
rm(list=ls())    
###########################################################

#----------------------------------------------------------------------------
# kaplan meier
#----------------------------------------------------------------------------
toydata = read_xlsx( "data/toyex.xlsx")
head(toydata)

#the censor variable is such that 1 denotes an event and 0 means right censored
#(it gives information about the censoring but could as well be called the event variable)
kaplanmeier = survfit(Surv(time,censor)~1,data=toydata)
summary(kaplanmeier)
autoplot(kaplanmeier)
quantile(kaplanmeier)

rm(list=ls()) 


#with employmentdata
employment = read_xlsx( "data/employment.xlsx")
head(employment)

kaplanmeier = survfit(Surv(time,censor)~course,data=employment)
print(kaplanmeier)

autoplot(kaplanmeier)
quantile(kaplanmeier)
survdiff(Surv(time,censor)~course,data=employment)

#----------------------------------------------------------------------------
# fitting distributions
#----------------------------------------------------------------------------

ggsurvplot(kaplanmeier, data = employment, fun = "cloglog")
course0 = subset(employment, course==0)

##WEIBULL
survreg0w <- survreg(Surv(time,censor)~1, data=course0, dist = "weibull")
summary(survreg0w)
AIC(survreg0w)
BIC(survreg0w)
mrl_0_weibull = exp(survreg0w$coefficients[[1]])*gamma(1+survreg0w$scale)
mrl_0_weibull

##LOGNORMAL
survreg0l <- survreg(Surv(time,censor)~1, data=course0, dist = "lognormal")
summary(survreg0l)
AIC(survreg0l)
BIC(survreg0l)
mrl_0_lognormal = exp(survreg0l$coefficients[[1]] + 0.50*survreg0l$scale^2)
mrl_0_lognormal

rm(list=ls()) 


#----------------------------------------------------------------------------
# accelerated failure time models
#----------------------------------------------------------------------------
employment = read_xlsx( "data/employment.xlsx")
head(employment)  #notice that age is read as a character variable
employment$agenumerical = as.numeric(employment$age) #the NAs remain NAs

survregw <- survreg(Surv(time,censor)~agenumerical+course+prevj, 
                    data=employment, dist = "weibull")
summary(survregw)
AIC(survregw)
BIC(survregw)
predict(survregw, newdata=list(agenumerical=39,prevj=3,course=0), 
        type="quantile",p=c(0.1, 0.5,0.9))

survregln <- survreg(Surv(time,censor)~agenumerical+course+prevj, 
                     data=employment, dist = "lognormal")
summary(survregln)
AIC(survregln)
BIC(survregln)
predict(survregln, newdata=list(agenumerical=39,prevj=3,course=0), 
        type="quantile",p=c(0.1, 0.5,0.9))


rm(list=ls()) 
