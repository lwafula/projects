library(readxl)
library(epiDisplay)
library(Epi)
library(VGAM)
library(mlogit)
library(ordinal)
library(gdata)
library(dfidx)
library(stats)
library(brant)

rm(list = ls())

data1=read.table("C:/Users/u0118298/OneDrive - KU Leuven/ATSTAT-TASKS/TASK3/3.QUERIES/provider.txt", header=T)
head(data1)
str(data1)
logistmodel=glm(data1$pack~data1$age+data1$gender, family=binomial())
summary(logistmodel)
data2=list(age=40,gender=1)
predict(logistmodel,newdata=data2, type="response")

# working
logistmodel2=glm(pack~age+gender, family=binomial(), data = data1)
data2df=data.frame(age=40,gender=1)
predict(logistmodel2,newdata=data2df, type="response")


logistmodel=glm(pack~age+gender, family=binomial(), data = data1)
data2=list(age=40,gender=1)
predict(logistmodel,newdata=data2, type="response")

