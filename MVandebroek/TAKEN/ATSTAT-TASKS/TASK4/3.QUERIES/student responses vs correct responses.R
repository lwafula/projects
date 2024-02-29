
library(nFactors) 
library(psych)
library(candisc)
library(readxl)
library(MASS)
library(corrplot)
library(fastDummies)
library(matrixcalc)
library(ICSNP)

rm(list = ls())

# queries
# Gabrielle Moens: Task 1 -quiz 4, Q23 in source file 

library(readxl)
library(ggplot2)
library(car)
library(corrplot)
library(readxl)
library(sjPlot)
library(gmodels)
library(stargazer)
library(emmeans)
library(tidyverse)

dataQNsdTL=read.table ("C:/Users/u0118298/Downloads/dataQNsdTL.txt", header = TRUE)


model2<- lm(Salary ~ Experience + Gender+Department+Gender * Department, data = dataQNsdTL)
model2
anova_result <- Anova(model2, type = "II")
anova_result

# sol

data = dataQNsdTL
data$Department <- as.factor(data$Department)
data$Gender <- as.factor(data$Gender)

typeIISS = lm(Salary ~ Experience + Gender + Department, data = data)
Anova(typeIISS, type = 2) |> as.data.frame() |> rownames_to_column() |> 
  filter(rowname == 'Department') |> pull(contains('Pr('))

# Task 4-quiz 4 i.e. Q14 in source file
# The question was: Question 4: Perform a factor analysis extracting 4 factors with method='ml' 
# and SMC as initial estimates for the communalities. Rotate the factor model with a varimax rotation 
# and give the loading of variable q9 on the third factor.

data3 = read.csv("C:/Users/u0118298/Downloads/zvSsha_data3.txt", sep = "")
result<-fa(data3,nfactors=4, fm="ml", SMC=TRUE,residuals=TRUE,rotate="varimax")
result$loadings[9,]

# sol
fapavarmax <- fa(data3, nfactors = 4, fm="ml", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
fapavarmax$loadings[9,'ML3']

# Irene Paglia, q4 taak 4; Q in source file 15
##Question 4: Perform a factor analysis extracting 4 factors with method='pa' and SMC 
##as initial estimates for the communalities. Rotate the factor model with a quartimax rotation 
##and give the loading of variable q15 on the fourth factor.

data3 = read.csv("C:/Users/u0118298/Downloads/dataset3.txt", sep = "")

famod = fa(data3, nfactors = 4, fm = "pa", SMC = TRUE, rotate = "quartimax", residuals = TRUE)
famod$loadings
print(famod$loadings, cut = 0.50)

faMLquart <- fa(data3, nfactors = 4, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "quartimax") 
faMLquart$loadings[15,'PA4']

# Lisa Van den Houte, q2 taak 4: Q7
dataset1 <- read.table("C:/Users/u0118298/OneDrive - KU Leuven/ATSTAT-TASKS/TASK4/2.INDIVIDUAL/1.DATA/q1377671_data1.txt", header = TRUE) 

fitl<- lda(Industry ~ experience + ranking ,dataset1,prior=c(0.2,0.8),CV=T)
table(observed=dataset1$Industry,predicted=fitl$class)

fitlcvp <- lda(Industry ~ experience + ranking ,dataset1, prior = c(0.8,0.2), CV=TRUE)
table(dataset1$Industry,fitlcvp$class)[2,1]

# Juliette Ingels, q2 taak 4: Q6

data1 <- read.table( "C:/Users/u0118298/Downloads/dTkqbV_data1.txt", header = TRUE) 

fitlcv = lda(Industry ~ experience + ranking ,data1,prior=c(0.1,0.9),CV = TRUE)

head(fitlcv$posterior)
table(observed=data1$Industry,predicted=fitlcv$class )

# sol

fitlcvp <- lda(Industry ~ experience + ranking, data1, prior = c(0.9,0.1), CV=TRUE)
table(data1$Industry, fitlcvp$class)[2,1]


# Irmak, q2 taak 4: Q7 in sourcefile
# student sol


data1 <- read.table( "C:/Users/u0118298/Downloads/task4data1irmak.txt", header = TRUE) 

model_1 <- lda(Industry ~ experience + ranking, data = data1, pprior=c(0.80,0.20))
model_1

pp2 = predict(model_1,data1) 
table(observed=data1$Industry,classified = pp2$class) 


# sol
fitlcvp <- lda(Industry ~ experience + ranking ,data1, prior = c(0.8,0.2), CV=TRUE)
table(data1$Industry,fitlcvp$class)[2,1]

model_1T <- lda(Industry ~ experience + ranking, data = data1, prior=c(0.80,0.20), CV=TRUE)

table(data1$Industry,model_1T$class)[2,1]

pp2T = predict(model_1T,data1) 
table(observed=data1$Industry,classified = pp2$class) 