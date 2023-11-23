options(width = 100, useFancyQuotes = FALSE)
rm(list=ls()) 

###########################################################
# part 3a: logistic regression
###########################################################
#install.packages('readxl',dependencies=TRUE)
#install.packages('epiDisplay',dependencies=TRUE)
#install.packages('Epi',dependencies=TRUE)
#install.packages('InformationValue',dependencies=TRUE)
#install.packages('VGAM',dependencies=TRUE)
#install.packages('mlogit',dependencies=TRUE)
#install.packages('ordinal',dependencies=TRUE)
#install.packages('gdata',dependencies=TRUE)
#install.packages("dfidx",dependencies=TRUE)
#install.packages("stats",dependencies=TRUE)
#install.packages("brant",dependencies=TRUE)
library(readxl)
library(epiDisplay)
library(Epi)
# library(InformationValue)
library(VGAM)
library(mlogit)
library(ordinal)
library(gdata)
library(dfidx)
library(stats)
library(brant)


#set the default directory with setwd, for instance
#setwd("C:/Documents/ATSTAT")  
setwd("C:/Users/u0118298/OneDrive/Projects/MVandebroek/TAKEN") 
#make sure the data are in the data subdirectory (here C:\Documents\ATSTAT\data)
rm(list=ls())          

###########################################################
#----------------------------------------------------------------------------
# binary logit model with 1 predictor
#----------------------------------------------------------------------------
accidents = read_xlsx( "data/accident.xlsx")
head(accidents)
#the response has to be a 0/1 dummy
accidents$dummyacc = numeric(nrow(accidents)) #initialize the dummy
accidents$dummyacc[accidents$accident == "noaccident"] = 0 
accidents$dummyacc[accidents$accident == "accident"] = 1 

logistmod = glm(dummyacc~hp,data = accidents,family=binomial()) 
summary(logistmod)

#to compute the likelihoodratiotest, we need to fit the null model and ask for the Chisquare test
logistmod0 = glm(dummyacc~1,data = accidents,family=binomial()) 
anova(logistmod0,logistmod, test="Chisq")

logistmod0$aic
logistmod$aic
confint(logistmod)
#compute the odds ratios and confidence intervals
exp(cbind("Odds ratio" = coef(logistmod), confint.default(logistmod, level = 0.95))) 

#to get predicted probabilities one can use the following 2 statements which are equivalent:
head(fitted(logistmod))    #fitted can only be used to make predictions for the original data
head(predict(logistmod,accidents,type="response"))   #predict can also be used with new data

pp = fitted(logistmod)
plotdata = as.data.frame(pp)
plotdata$hp = accidents$hp
plotdatas = plotdata[order(plotdata$hp),]     
par(mar=c(4,5,1,1))   

plot(plotdatas$hp,plotdatas$pp,main="probability of having an accident",
     xlab="hp",xlim = c(100,200),ylim = c(0,1),type="l")

#----------------------------------------------------------------------------
# classification (depends on cutoff)
#----------------------------------------------------------------------------
cutoff = 0.52
table(accidents$accident,pp> cutoff)
table(observed=accidents$accident,predicted = pp> cutoff)  #to get labels on rows and colums

accidents$pp = pp
TP = nrow(subset(accidents,accident=="accident"   & pp > cutoff))
FP = nrow(subset(accidents,accident=="noaccident" & pp > cutoff))
FN = nrow(subset(accidents,accident=="accident"   & pp < cutoff))
TN = nrow(subset(accidents,accident=="noaccident" & pp < cutoff))

pctcorrect =  (TP+TN)/(TP+FP+FN+TN)
sensitivity = TP/(TP+FN)
specificity = TN/(FP+TN)
FPR = FP/(FP+TN)
FNR = FN/(TP+FN)  

#----------------------------------------------------------------------------
# classification table with several cutoff values (code not to be known)
#----------------------------------------------------------------------------
ctable=c(0,0,0,0,0)
for(cutoff in seq(0.01,0.99,0.01)){      #no cutoff 0 and 1 to avoid problems 
  ctableor = table(accidents$accident,pp> cutoff)  
  ct = unmatrix(ctableor,byrow=TRUE)     #put table on 1 row
  cct = c(cutoff,ct)
  ctable = rbind(ctable,cct)
}
colnames(ctable) = c("cutoff","FN", "TP", "TN","FP")
rownames(ctable) = NULL
classtable = ctable[2:nrow(ctable),]
classtable[1:5,]

#----------------------------------------------------------------------------
# ROC curve and concordant pairs
#----------------------------------------------------------------------------
source("Part3/concordance.R")

par(mar=c(0,2,1,5))

ROC(form=dummyacc~hp, plot="ROC",AUC=TRUE,data=accidents)
Concordance(actuals = accidents$dummyacc, predictedScores=pp)
rm(list=ls()) 


#----------------------------------------------------------------------------
# binary logit model with more than 1 predictor
#----------------------------------------------------------------------------
accidents = read_xlsx( "data/accident.xlsx")
head(accidents)

accidents$dummyacc = numeric(nrow(accidents))
accidents$dummyacc[accidents$accident == "noaccident"] =0 
accidents$dummyacc[accidents$accident == "accident"] =1 

logistmod2 = glm(dummyacc~hp+experience,data = accidents,family=binomial())
summary(logistmod2)

cutoff = 0.55
table(accidents$accident,fitted(logistmod2)> cutoff)
table(observed=accidents$accident,predicted = fitted(logistmod2)> cutoff)

ROC(form=dummyacc~hp+experience, plot="ROC",AUC=TRUE,data=accidents)

source("Part3/concordance.R")
Concordance(actuals = accidents$dummyacc, predictedScores=fitted(logistmod2))

#----------------------------------------------------------------------------
# predict the logit and the probability
#----------------------------------------------------------------------------

predict(logistmod2,newdata=list(hp = 180,experience = 20),type = "response")  #gives the probability
predict(logistmod2,newdata=list(hp = 180,experience = 20),type = "link")      #gives the logit

rm(list=ls()) 


#----------------------------------------------------------------------------
# another example: workdata
#----------------------------------------------------------------------------
workdata = read_xlsx( "data/work_data.xlsx")
head(workdata)


logistmod = glm(work~.,data = workdata,family=binomial()) # work=1 is the event
summary(logistmod)
exp(cbind("Odds ratio" = coef(logistmod), confint.default(logistmod, level = 0.95))) 
logistmod$aic

table(observed=workdata$work,predicted = fitted(logistmod)> 0.50)

par(mar=c(5,5,5,5))
ROC(form=work~child1+child2+husbinc+educ+age, plot="ROC",AUC=TRUE,data = workdata)

source("Part3/concordance.R")
Concordance(actuals = workdata$work, predictedScores=fitted(logistmod))

rm(list=ls()) 


#----------------------------------------------------------------------------
# cumulative logit model
#----------------------------------------------------------------------------
cumuldat =  read_xlsx( "data/cumulative_logit_data.xlsx")
head(cumuldat)
cumuldat$response = as.factor(cumuldat$accident)  #to be used as response
str(cumuldat)

#we use 2 different functions:
#clm because it gives p-values for the significance of the parameters
#polr because it has a score test and is easier for making predictions

cumulclm = clm(response~hp+experience,data = cumuldat,family = cumulative)
summary(cumulclm)
cumulclm0 =  clm(response~1,data = cumuldat,family = cumulative)
summary(cumulclm0)
anova(cumulclm0,cumulclm)


cumulpolr = polr(response~hp+experience,data = cumuldat) 
brant(cumulpolr)  

#predicted probabilities of original data with either of the following 2 statements
head(fitted(cumulpolr))
head(predict(cumulpolr,cumuldat,type="probs"))

#predictions for new observations
predict(cumulpolr, newdata = list(hp=180,experience=20), type="probs") 
#type = class gives most likely response
predict(cumulpolr, newdata = list(hp=180,experience=20), type="class") 
#plot cumulative model (code not to be known)

predictions = fitted(cumulpolr)
par(mfrow=c(1,2)) 
par(mar=c(2,2,2,2))
#the next line creates a new plot with the get correct axes (but nothing in it)
plot(cumuldat$hp,predictions[,1],type="n",ylim = c(0,1),xlim = c(120,220)) 
curve(exp(19.2876-0.1285*x+0.1007*20)/(1+exp(19.2876-0.1285*x+0.1007*20))
      ,xlab="hp",ylab="",add=TRUE)
curve(exp(22.5384-0.1285*x+0.1007*20)/(1+exp(22.5384-0.1285*x+0.1007*20))
      ,xlab="hp",ylab="",lty=2,add=TRUE,col=2)
text(200,1,"experience=20",cex=0.6)
title("model as a function of hp",cex.main=0.750)
legend(115,0.2,legend=c("P(no accident)","P(no or small acc)"),
       lty=c(1,2),col=c(1,2),cex=0.6)


plot(cumuldat$experience,predictions[,1],type="n",xlim = c(-90,110),ylim = c(0,1))
curve(exp(19.2876-0.1285*180+0.1007*x)/(1+exp(19.2876-0.1285*180+0.1007*x)),
      xlab="experience",ylab="",ylim=c(0,1),add=TRUE)
curve(exp(22.5384-0.1285*180+0.1007*x)/(1+exp(22.5384-0.1285*180+0.1007*x)),
      xlab="experience",ylab="",ylim=c(0,1),col=2,lty=2,add=T)
text(-20,1,"hp=180",cex=0.6)
title("model as a function of experience",cex.main=0.75)
legend(40,0.2,legend=c("P(no accident)","P(no or small acc)"),lty=c(1,2),col=c(1,2),cex=0.4)
rm(list=ls()) 
par(mfrow=c(1,1)) 

#----------------------------------------------------------------------------
# multinomial model with multinom function
#----------------------------------------------------------------------------
traveldata1 =  read.csv( "data/multinomdata.csv")
head(traveldata1)

traveldata1$response = as.factor(traveldata1$chosen) #to be used as response  
traveldata1$response = relevel(traveldata1$response, ref = "train")
table(traveldata1$chosen)

mnlmultinom = multinom(response ~age+genderF0,data = traveldata1)
summary(mnlmultinom)

#compare models with and without age or genderF0 and perform LRT 
#in the slides this is tested with a Wald test which is only asymptotically equivalent:
mnlmultinom1 = multinom(response ~age,data = traveldata1)
mnlmultinom2 = multinom(response ~genderF0,data = traveldata1)
anova(mnlmultinom,mnlmultinom1,test = "Chisq")
anova(mnlmultinom,mnlmultinom2,test = "Chisq")

relative.risks =exp(coef(mnlmultinom))
relative.risks
exp(confint(mnlmultinom))

predictions = fitted(mnlmultinom) #or predict(mnlmultinom,traveldata1,type="probs")
head(predictions)

predict(mnlmultinom,newdata=list(age=25, genderF0 = 0),type = "probs")  #gives the probabilities
predict(mnlmultinom,newdata=list(age=25, genderF0 = 0),type = "class")  #gives most likely response   

rm(list=ls()) 


#----------------------------------------------------------------------------
# multinomial model with the mlogit function 
# mlogit fits models with generic coefficients + alternative specific coefficients;
# depending on the structure of the data (long or wide, balanced or not, ...), 
# the statements can become quite complicated
#----------------------------------------------------------------------------
dcdatlong =  read.csv( "data/travel_long.csv")
#for now we do not need all variables:
traveldata2=  dcdatlong[c(1,2,3,4,7)]
head(traveldata2) #remark that the data are structured differently now

#mlogit expects the attributes with alternative specific parameters after the |
#and the attributes with generic coefficients before the | 
mnlmlogit = mlogit(chosen~0|age+genderF0, data = traveldata2, idx=c("subject", "mode"),reflevel="train")
summary(mnlmlogit)

#here we can use the predefined Wald test:
waldtest(mnlmlogit,chosen~0|genderF0)
waldtest(mnlmlogit,chosen~0|age)

head(mnlmlogit$probabilities)

#making predictions for new data with mlogit is very complicated, we do not deal with that

#----------------------------------------------------------------------------
# discrete choice model  with generic coefficients
# #----------------------------------------------------------------------------
dcdatlong =  read.csv( "data/travel_long.csv")
traveldata3 =  dcdatlong[c(3,4,5,6,7)]
head(traveldata3) 

#remark that the 0 after the | is needed to avoid the default ASC (alternative specific constants)
dcm = mlogit(chosen~time+cost|0, data = traveldata3,idx=c("subject", "mode"),reflevel = "train") 
summary(dcm)

head(dcm$probabilities)


rm(list=ls()) 


#----------------------------------------------------------------------------
# discrete choice model  with generic + alternative specific coefficients
#----------------------------------------------------------------------------
traveldata =  read.csv( "data/travel_long.csv")
head(traveldata)

generalmod = mlogit(chosen~time+cost|age, data = traveldata,idx=c("subject", "mode"),reflevel = "train") 
#also genderF0 can be added in mlogit(chosen~time+cost|age+genderF0, ...) 
#but as that was not in the slides, we only include age with 
#alternative specific coefficients here too

summary(generalmod)

head(generalmod$probabilities)


rm(list=ls()) 
