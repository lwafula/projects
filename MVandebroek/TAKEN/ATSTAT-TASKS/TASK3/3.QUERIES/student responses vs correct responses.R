
library("readxl")
library("dplyr")
library("gdata")
library("survival")
library("ordinal")
library("nnet")
library('writexl')
library('stringi')

# Amber Corneillie --------------------------------------------------------

# Q2

data <- read.table("C:/Users/u0118298/Downloads/taak3dataset1.txt", header = TRUE)
data

data$EMPLOYEES=as.numeric(data$EMPLOYEES)
data$FAMILY = as.factor(data$FAMILY)
data$CHARACTER=as.factor(data$CHARACTER)
attach(data)
survreg = survreg(Surv(YEARS,FAILURE)~EMPLOYEES+FAMILY+CHARACTER, data=data, dist = "lognormal")
predict(survreg, newdata=list(EMPLOYEES = 70, CHARACTER = '1', FAMILY = '0'), type="quantile",p=c(0.2))
#8,641374

# correct answer

SMEFailures_data = data
survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER+FAMILY+EMPLOYEES,
                    data= SMEFailures_data, dist = "lognormal")

predict(survregw, newdata=list(EMPLOYEES=70, FAMILY=as.factor(0), CHARACTER=as.factor(2)),
        type="quantile", p=0.2)
predict(survreg, newdata=list(EMPLOYEES = 70, CHARACTER = '2', FAMILY = '0'), type="quantile",p=c(0.2))


detach(data)

#Question 3
data2 = read.table("C:/Users/u0118298/Downloads/taak3dataset2.txt", header = TRUE)
data2
data2$GENDER =as.factor(data2$GENDER)
data2$Y =as.factor(data2$Y)
attach(data2)
model3 = multinom(Y~AGE + DOSE + GENDER, data = data2) 
AIC(model3)
#86.92933

counseling_data = data2
cumulmod = clm(Y ~ AGE + DOSE + GENDER, data = counseling_data)
sol <- summary(cumulmod)
as.numeric(as.character(sol$info[5]$AIC))

model3_1 = clm(Y~AGE + DOSE + GENDER, data = data2) 
AIC(model3_1)


# Xinru Quan --------------------------------------------------------------

depression = read.table( "C:/Users/u0118298/Downloads/test3_data2.txt", header = TRUE)
head(depression)
depression$response = as.factor(depression$Y)
depression$response = relevel(depression$response, ref = "1")
attach(depression)
table(Y)

mnlmod = multinom(response ~WEIGHT+DOSE+GENDER,data = depression)
summary(mnlmod)
predictions = fitted(mnlmod)
predictions[10, 2]

# correct
counseling_data = depression
cumulmod = clm(response ~ GENDER + DOSE + WEIGHT, data = counseling_data)
predictions=predict(cumulmod, newdata = subset(counseling_data, select = -response))$fit
predictions[10, 2]

predictions = fitted(cumulmod)
predictions[10]



# Ha Nguyen ---------------------------------------------------------------

# Q1
kaplanmeier = survfit(Surv(YEARS, FAILURE)~1,data=subset(data1, family = 1))
summary(kaplanmeier)
quantile(kaplanmeier)

data_filter <- SMEFailures_data[SMEFailures_data$FAMILY == 1, ]
kaplanmeier = survfit(Surv(YEARS, FAILURE) ~ 1, data = data_filter)
sol <- quantile(kaplanmeier)
sol$quantile[3]

# Q2
survreg0w <- survreg(Surv(YEARS,FAILURE)~1, data=data1, dist = "weibull")
summary(survreg0w)

survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER + FAMILY + EMPLOYEES,
                    data= SMEFailures_data, dist = "weibull")

predict(survregw, newdata=list(EMPLOYEES=50, FAMILY=0, CHARACTER=2),
        type="quantile", p=0.2)
