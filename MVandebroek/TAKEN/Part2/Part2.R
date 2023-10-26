options(width = 100, useFancyQuotes = FALSE)
rm(list=ls()) 

##########################################################
# part 2: econometrics
##########################################################
#install.packages('ggplot2',dependencies=TRUE)
#install.packages('readxl',dependencies=TRUE)
#install.packages('plm',dependencies=TRUE)
#install.packages('lmtest',dependencies=TRUE)
#install.packages('orcutt',dependencies=TRUE)
#install.packages('Hmisc',dependencies=TRUE)
#install.packages('AER',dependencies=TRUE) 
#install.packages('aTSA',dependencies=TRUE)
#install.packages('sandwich',dependencies=TRUE)
#install.packages('clubSandwich',dependencies=TRUE)
library(ggplot2)
library(readxl)
library(plm)
library(lmtest)
library(orcutt)
library(Hmisc)
library(AER)
library(aTSA)
library(sandwich)
library(clubSandwich)

#set the default directory with setwd, for instance
#setwd("C:/Documents/ATSTAT")  
#make sure the data are in the data subdirectory (here C:\Documents\ATSTAT\data)
rm(list=ls())    
##########################################################
#----------------------------------------------------------------------------
# heteroscedasticity
#----------------------------------------------------------------------------
foodexp = read.table( "data/foodexp.txt", header = TRUE)
head(foodexp)
plot(foodexp$INCOME,foodexp$FOOD_EXP,xlab="INCOME",ylab="FOOD_EXP")

reg = lm(FOOD_EXP~INCOME, data = foodexp)
summary(reg)

# compute heteroskedasticity-robust standard errors
# cfr vcovHC = variance-covariancematrix robust to HeterosCedasticity
whitecov = vcovHC(reg, type = "HC0")
coeftest(reg, vcov. = whitecov)
#remark that several alternatives have been developed recently 
#and can be applied by choosing HC1, HC2, HC3 or HC4

rm(list=ls()) 

#----------------------------------------------------------------------------
# autocorrelation
#----------------------------------------------------------------------------
infldata = read_xlsx("data/inflation.xlsx")
head(infldata)
infldata$time = 1:nrow(infldata)

#one can also use the ts.plot to plot the time series but I need the time variable anyway

plot(infldata$time,infldata$INFLN,type="l")
dwtest(INFLN~time,data=infldata)

#standard OLS inference is not valid but we want to check the residuals:
ols = lm(INFLN~time,data=infldata)
summary(ols)

#ACF autocorrelation function: corr(u_t, u_{t-s}) as a function of s
resnw = ols$residuals

#notice the significant first autocorrelation in the plot
acf(resnw,main="check autocorrelation in residuals")
# iterated cochrane-orcutt procedure
co = cochrane.orcutt(ols)  #3 iterations (not interactions as mentioned in the output)!
co
summary(co)

# compute HeterosCedasticity and Autocorrelation robust standard errors 
# the default in vcovHAC is NeweyWest
NWcov = vcovHAC(ols)
coeftest(ols, vcov. = NWcov)

#Breusch-Godfrey test for AR(1) residuals
regmod = lm(INFLN~time,data=infldata)
bgtest(regmod, order = 1)

#add one lag
infldata$laginfln= Lag(infldata$INFLN, 1)
regmod2 = lm(INFLN~time+laginfln,data=infldata)
bgtest(regmod2, order = 1)

#add another lag
infldata$lag2infln= Lag(infldata$laginfln, 1) #or Lag(infldata$INFLN, 2)
regmod3 = lm(INFLN~time+laginfln+lag2infln,data=infldata)
bgtest(regmod3, order = 1)
summary(regmod3)

rm(list=ls()) 

#----------------------------------------------------------------------------
# instrumental variables
#----------------------------------------------------------------------------
datIV <- read.csv("data/mroz2.csv")
head(datIV)
atwork = subset(datIV,wage > 0)

#as wage is right skewed, people often transform it to get a more
#symmetric (normally) distributed variable; remark that the estimators
#will asymptotically be normal anyway but we need less observations to reach normality if 
#the variables are more normally distributed
atwork$lnwage = log(atwork$wage)  #log stands for ln !!!


summary(lm(lnwage~educ, data = atwork))

#put all the explanatory variables before the | in ivreg  whether they are endogenous or exogenous
#put all the instrumental variables and all the exogenous regressors after the | in ivreg
summary(ivreg(lnwage~educ|mothereduc, data= atwork), diagnostics = TRUE)

#illustration: ratio of standard errors approximately equal to 1/correlation(x,z)  
stdev_OLS =  0.0144
stdev_IV =   0.03823
stdev_IV/stdev_OLS 
1/cor(atwork$educ,atwork$mothereduc)

#use fathereducation as extra IV
summary(ivreg(lnwage~educ|mothereduc+fathereduc, data= atwork), 
        diagnostics = TRUE)

rm(list=ls()) 

#----------------------------------------------------------------------------
# Augmented Dickey-Fuller test
#----------------------------------------------------------------------------
timeseries = read_xlsx("data/stationarity.xlsx")
head(timeseries)

ts.plot(timeseries$Fseries, xlab="time" )

adf.test(timeseries$Fseries)

#remark that the lags in this table refer to the 'extra' lags that have to be added 
#to get rid of autocorrelated errors

#no matter whether we need an intercept and a deterministic trend or not, 
#no matter whether we need 0, 1, 2 or 3 lags to get rid of autocorrelation,
#the H0 of a unit root is accepted, so the series is UNIT ROOT NONSTATIONARY


#----------------------------------------------------------------------------
#sometimes the decision depends on the exact number of lags you need to include 
#and on whether an intercept and trend are needed or not:
#to get rid of autocorrelated errors > we use Breusch-Godfrey to test autocorrelation

#remark that you have to use the Lag function with Capital L!

#first we test whether we need any extra lag 

timeseries$L1F = Lag(timeseries$Fseries, 1)           
regmod1 = lm(Fseries~0+L1F,data=timeseries)     #no drift, no trend
regmod2 = lm(Fseries~  L1F,data=timeseries)     #with drift, no trend
regmod3 = lm(Fseries~time+L1F,data=timeseries)  #with drift and trend
bgtest(regmod1, order = 1)
bgtest(regmod2, order = 1)
bgtest(regmod3, order = 1)  
#so the residuals are autocorrelated and we need at least 1 lag extra

#check whether 1 extra lag is enough:
timeseries$L2F = Lag(timeseries$Fseries, 2)  
regmod1 = lm(Fseries~0+L1F+L2F,data=timeseries)
regmod2 = lm(Fseries~L1F+L2F,data=timeseries)
regmod3 = lm(Fseries~time+L1F+L2F,data=timeseries)
bgtest(regmod1, order = 1)
bgtest(regmod2, order = 1)
bgtest(regmod3, order = 1)  

#the residuals are no longer autocorrelated 

#as the diff function removes the first observation, we have to create a 
#difference variable ourselves:
timeseries$D1F = timeseries$Fseries-Lag(timeseries$Fseries,1)
timeseries$L1DF = Lag(timeseries$D1F, 1)

summary(lm(D1F~ time +L1F + L1DF, data = timeseries))
#as intercept and trend are significant, we need intercept + trend
#and 1 lag, and the appropriate tau in the adf.test output is -3.14  
#with p-value=  0.109, so the Fseries is unit root nonstationarity
#----------------------------------------------------------------------------

#test whether the series is I(1) (so whether D1F is stationary):

ts.plot(timeseries$D1F, xlab="time")

#remark that we can also define D1F by diff(Fseries,1) if the
#variable is to be used in the adf.test function:

adf.test(timeseries$D1F)

#as almost all p-values are small, we might conclude that the difference variable is
#stationary and therefore the Fseries is I(1)

#if you need to check which teststatistic to use, we check similarly as before:
           
regmod_D_1 = lm(D1F~0+L1DF,data=timeseries)       #no drift, no trend
regmod_D_2 = lm(D1F~  L1DF,data=timeseries)       #with drift, no trend
regmod_D_3 = lm(D1F~  time+L1DF,data=timeseries)  #with drift and trend
bgtest(regmod_D_1, order = 1)
bgtest(regmod_D_2, order = 1)
bgtest(regmod_D_3, order = 1)  
#so the residuals are not autocorrelated and we do not need extra lags

timeseries$D1DF = timeseries$D1F-Lag(timeseries$D1F,1)
timeseries$L1DDF = Lag(timeseries$D1DF, 1)

summary(lm(D1DF~ time +L1DF , data = timeseries))
#so we need the model without an intercept or trend 
summary(lm(D1DF~ -1 +L1DF , data = timeseries))
#the appropriate tau = -4.007 with corrected p-value of 0.01
#so D1F is stationary and the Fseries  is I(1)

#----------------------------------------------------------------------------
#test whether Fseries and Bseries are cointegrated
#----------------------------------------------------------------------------
#test first whether Bseries is I(0) (is not the case) or I(1) (is the case):

ts.plot(timeseries$Bseries, xlab="time")

adf.test(timeseries$Bseries)

timeseries$D1B = timeseries$Bseries - Lag(timeseries$Bseries,1)

ts.plot(timeseries$D1B, xlab="time")

adf.test(timeseries$D1B)

reg = lm(Fseries~Bseries,data = timeseries)
res = reg$residuals
adf.test(res)

#we reject the null hypothesis of unit root nonstationarity 
#(you can check that you need 1 lag but no intercept or trend in this case)
rm(list=ls()) 


#----------------------------------------------------------------------------
# panel data
#----------------------------------------------------------------------------
paneldata = read_xlsx("data/paneldata.xlsx")
head(paneldata)

# to get the appropriate tests, we use the plm function
# with the options:
#  - pooling: simple OLS with 1 intercept,
#  - random: the random effects model with FGLS or RE estimator
#  - within: the fixed effects model with the within group estimator

#to use plm, we first need to convert the data with pdata.frame 
#to indicate which variable identifies the subjects and which the time
panel <- pdata.frame(paneldata, index=c("I","T"),  row.names=TRUE)

#pooled model
plm_pooled = plm(investments~value+capital, data=panel, model="pooling")
summary(plm_pooled)
#perfom the tests using the cluster corrected covariance matrix:
coeftest(plm_pooled, vcov=vcovCR(plm_pooled,type="CR0"))

#FGLS estimators
plm_random <- plm(investments~value+capital, data=panel, model="random")
summary(plm_random)
#remark that the variance component are estimated slightly different
#than in the slides but the parameter estimates + tests are OK

#fixed effects estimators
plm_fixed <- plm(investments~value+capital, data=panel, model="within")
summary(plm_fixed )
#perfom the tests using the cluster corrected covariance matrix:
coeftest(plm_fixed, vcov=vcovCR(plm_fixed,type="CR0"))

#test whether the intercepts in the fixed effects model are equal
#so compare the pooled and the fixed effects model
pFtest(plm_fixed,plm_pooled)

#Hausman test
phtest(plm_random,plm_fixed)
#test and p-value differ from the slides but conclusion is similar

rm(list=ls()) 
