########################################
### script containing all functions ####
########################################

#libraries
library("readxl")
library("dplyr")
library("gdata")
library("survival")
library("ordinal")
library("nnet")

#No scientific notation
options(scipen = 999)

### read in individual dataset

getDATA <- function(ID, type){
  
  WD <- paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK3\\2.INDIVIDUAL\\")
  
  filename <- paste(WD, "1.DATA\\",ID, "_data", type, ".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}

### function to get group label
getLABEL <- function(ID, user_info){
  
  label <- user_info$`group`[user_info$`Username` == as.character(ID)]
  
  return(label)
}

### solution function
getSOL_story1 <- function(data, questions){
  
  covid_data <- data[[1]]
  rating_data <- data[[2]]
  rating_data$class <- as.factor(rating_data$class)
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
      #solution Q1
      {
        data_filter <- covid_data[covid_data$OXYGEN == 1, ]
        kaplanmeier = survfit(Surv(SURVT, STATUS) ~ 1, data = data_filter)
        sol <- quantile(kaplanmeier)
        sol$quantile[3]
      },
      
      #solution Q2
      {
        data_filter <- covid_data[covid_data$OXYGEN == 0, ]
        kaplanmeier = survfit(Surv(SURVT, STATUS) ~ 1, data = data_filter)
        sol <- quantile(kaplanmeier)
        sol$quantile[3]
      },
      
      #solution Q3
      {
        data_filter <- covid_data[covid_data$CLINIC == 1, ]
        kaplanmeier = survfit(Surv(SURVT, STATUS) ~ 1, data = data_filter)
        sol <- quantile(kaplanmeier)
        sol$quantile[2]
      },
      
      #solution Q4
      {
        data_filter <- covid_data[covid_data$DOSE == 50, ]
        kaplanmeier = survfit(Surv(SURVT, STATUS) ~ 1, data = data_filter)
        sol <- quantile(kaplanmeier)
        sol$quantile[2]
      },
      
      #solution Q5
      {
        survregw <- survreg(Surv(SURVT, STATUS)~ CLINIC+OXYGEN+DOSE,
                            data= covid_data, dist = "lognormal")
        predict(survregw, newdata=list(DOSE=70, OXYGEN=1, CLINIC=1),
                type="quantile", p=0.9)
        
      },
  
      #solution Q6
      {
        survregw <- survreg(Surv(SURVT, STATUS)~ CLINIC+OXYGEN+DOSE,
                            data= covid_data, dist = "lognormal")
        
        predict(survregw, newdata=list(DOSE=70, OXYGEN=0, CLINIC=2),
                type="quantile", p=0.9)
      },
    
      #solution Q7
      {
        survregw <- survreg(Surv(SURVT, STATUS)~ CLINIC+OXYGEN+DOSE,
                            data= covid_data, dist = "weibull")
        
        predict(survregw, newdata=list(DOSE=50, OXYGEN=1, CLINIC=1),
                type="quantile", p=0.9)
      },
      
      #solution Q8
      {
        survregw <- survreg(Surv(SURVT, STATUS)~ CLINIC + OXYGEN + DOSE,
                            data= covid_data, dist = "weibull")
        
        predict(survregw, newdata=list(DOSE=50, OXYGEN=0, CLINIC=2),
                type="quantile", p=0.9)
        
      },
      
      #solution Q9
      {
        cumulmod = clm(class ~ OPMAR + LTDCAP + LEVER, data = rating_data)
        sol <- summary(cumulmod)
        as.numeric(as.character(sol$info[5]$AIC))
      },
      
      #solution Q10
      {
        cumulmod = clm(class ~ OPMAR + LTDCAP + RECTURN, data = rating_data)
        sol <- summary(cumulmod)
        as.numeric(as.character(sol$info[5]$AIC))
      }, 
      
      #solution Q11
      {
        cumulmod = clm(class ~ OPMAR + LTDCAP + LEVER, data = rating_data)
        sol <- summary(cumulmod)
        sol$logLik
        
      }, 
      
      #solution Q12
      {
        cumulmod = clm(class ~ OPMAR + LTDCAP + RECTURN, data = rating_data)
        sol <- summary(cumulmod)
        sol$logLik
      },
      
      #solution Q13
      {
        cumulmod = clm(class ~ LEVER + LTDCAP + RECTURN, data = rating_data)
        predictions=predict(cumulmod, newdata = subset(rating_data, select = -class))$fit
        predictions[20, 1]
      },
      
      #solution Q14
      {
        cumulmod = clm(class ~ LEVER + LTDCAP + RECTURN, data = rating_data)
        predictions=predict(cumulmod, newdata = subset(rating_data, select = -class))$fit
        predictions[10, 2]
      },
      
      #solution Q15
      {
        cumulmod = clm(class ~ LEVER + OPMAR + RECTURN, data = rating_data)
        predictions=predict(cumulmod, newdata = subset(rating_data, select = -class))$fit
        predictions[20, 1]
      },
      
      #solution Q16
      {
        cumulmod = clm(class ~ LEVER + OPMAR + RECTURN, data = rating_data)
        predictions=predict(cumulmod, newdata = subset(rating_data, select = -class))$fit
        predictions[10, 2]
      }
    )
  }
    
 return(round(solutions, digits = 3))

}

getSOL_story2 <- function(data, questions){
  
  dropout_data <- data[[1]]
  vaccin_data <- data[[2]]
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
  
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             data_filter <- dropout_data[dropout_data$TYPE == "primary_school", ]
                             kaplanmeier = survfit(Surv(DAYS, DROPOUT) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[2]
                           },
                           
                           #solution Q2
                           {
                             data_filter <- dropout_data[dropout_data$TYPE == "high_school", ]
                             kaplanmeier = survfit(Surv(DAYS, DROPOUT) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[2]
                           },
                           
                           #solution Q3
                           {
                             data_filter <- dropout_data[dropout_data$VACCINATED == "YES", ]
                             kaplanmeier = survfit(Surv(DAYS, DROPOUT) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[1]
                           },
                           
                           
                           #solution Q4
                           {
                             data_filter <- dropout_data[dropout_data$VACCINATED == "NO", ]
                             kaplanmeier = survfit(Surv(DAYS, DROPOUT) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[1]
                           },
                           
                           #solution Q5
                           {
                             survregw <- survreg(Surv(DAYS, DROPOUT)~ VACCINATED+TYPE+AGE,
                                                 data= dropout_data, dist = "lognormal")
                             predict(survregw, newdata=list(VACCINATED="YES", TYPE="primary_school", AGE=60),
                                     type="quantile", p=0.9)
                             
                           },
                           
                           #solution Q6
                           {
                             survregw <- survreg(Surv(DAYS, DROPOUT)~ VACCINATED+TYPE+AGE,
                                                 data= dropout_data, dist = "lognormal")
                             predict(survregw, newdata=list(VACCINATED="NO", TYPE="primary_school", AGE=60),
                                     type="quantile", p=0.9)
                           },
                           
                           #solution Q7
                           {
                             survregw <- survreg(Surv(DAYS, DROPOUT)~ VACCINATED+TYPE+AGE,
                                                 data= dropout_data, dist = "weibull")
                             predict(survregw, newdata=list(VACCINATED="YES", TYPE="high_school", AGE=50),
                                     type="quantile", p=0.9)
                           },
                           
                           #solution Q8
                           {
                             survregw <- survreg(Surv(DAYS, DROPOUT)~ VACCINATED+TYPE+AGE,
                                                 data= dropout_data, dist = "weibull")
                             predict(survregw, newdata=list(VACCINATED="NO", TYPE="high_school", AGE=50),
                                     type="quantile", p=0.9)
                             
                           },
                           
                           #solution Q9
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             sol <- MASS::dropterm(model1,test='Chisq')
                             as.numeric(as.character(sol$LRT[4]))
                           },
                           
                           #solution Q10
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             sol <- MASS::dropterm(model1,test='Chisq')
                             as.numeric(as.character(sol$LRT[3]))
                           }, 
                           
                           #solution Q11
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             sol <- MASS::dropterm(model1,test='Chisq')
                             as.numeric(as.character(sol$LRT[2]))
                             
                           }, 
                           
                           #solution Q12
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             sol <- MASS::dropterm(model1,test='Chisq')
                             as.numeric(as.character(sol$`Pr(Chi)`[3]))
                           },
                           
                           #solution Q13
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             fitted(model1)[6, 1]
                           },
                           
                           #solution Q14
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             fitted(model1)[2, 1]
                           },
                           
                           #solution Q15
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             fitted(model1)[6, 3]
                           },
                           
                           #solution Q16
                           {
                             model1 <- multinom(BRAND ~ REGION + SEX + AGE, data = vaccin_data, trace = FALSE)
                             fitted(model1)[2, 3]
                           }
    )
  }
  return(round(solutions, digits = 3))
  
}

getSOL_story3 <- function(data, questions){
  
  SMEFailures_data <- data[[1]]
  counseling_data <- data[[2]]
  counseling_data$Y <- as.factor(counseling_data$Y)
  counseling_data$GENDER <- as.factor(counseling_data$GENDER)
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             data_filter <- SMEFailures_data[SMEFailures_data$FAMILY == 1, ]
                             kaplanmeier = survfit(Surv(YEARS, FAILURE) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[3]
                           },
                           
                           #solution Q2
                           {
                             data_filter <- SMEFailures_data[SMEFailures_data$FAMILY == 0, ]
                             kaplanmeier = survfit(Surv(YEARS, FAILURE) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[3]
                           },
                           
                           #solution Q3
                           {
                             data_filter <- SMEFailures_data[SMEFailures_data$CHARACTER == 1, ]
                             kaplanmeier = survfit(Surv(YEARS, FAILURE) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[2]
                           },
                           
                           #solution Q4
                           {
                             data_filter <- SMEFailures_data[SMEFailures_data$EMPLOYEES == 50, ]
                             kaplanmeier = survfit(Surv(YEARS, FAILURE) ~ 1, data = data_filter)
                             sol <- quantile(kaplanmeier)
                             sol$quantile[2]
                           },
                           
                           #solution Q5
                           {
                             survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER+FAMILY+EMPLOYEES,
                                                 data= SMEFailures_data, dist = "lognormal")
                             predict(survregw, newdata=list(EMPLOYEES=70, FAMILY=1, CHARACTER=1),
                                     type="quantile", p=0.2)
                             
                           },
                           
                           #solution Q6
                           {
                             survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER+FAMILY+EMPLOYEES,
                                                 data= SMEFailures_data, dist = "lognormal")
                             
                             predict(survregw, newdata=list(EMPLOYEES=70, FAMILY=0, CHARACTER=2),
                                     type="quantile", p=0.2)
                           },
                           
                           #solution Q7
                           {
                             survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER+FAMILY+EMPLOYEES,
                                                 data= SMEFailures_data, dist = "weibull")
                             
                             predict(survregw, newdata=list(EMPLOYEES=50, FAMILY=1, CHARACTER=1),
                                     type="quantile", p=0.2)
                           },
                           
                           #solution Q8
                           {
                             survregw <- survreg(Surv(YEARS, FAILURE)~ CHARACTER + FAMILY + EMPLOYEES,
                                                 data= SMEFailures_data, dist = "weibull")
                             
                             predict(survregw, newdata=list(EMPLOYEES=50, FAMILY=0, CHARACTER=2),
                                     type="quantile", p=0.2)
                             
                           },
                           
                           #solution Q9
                           {
                             cumulmod = clm(Y ~ AGE + DOSE + GENDER, data = counseling_data)
                             sol <- summary(cumulmod)
                             as.numeric(as.character(sol$info[5]$AIC))
                           },
                           
                           #solution Q10
                           {
                             cumulmod = clm(Y ~ AGE + DOSE + WEIGHT, data = counseling_data)
                             sol <- summary(cumulmod)
                             as.numeric(as.character(sol$info[5]$AIC))
                           }, 
                           
                           #solution Q11
                           {
                             cumulmod = clm(Y ~ AGE + DOSE + GENDER, data = counseling_data)
                             sol <- summary(cumulmod)
                             sol$logLik
                             
                           }, 
                           
                           #solution Q12
                           {
                             cumulmod = clm(Y ~ AGE + DOSE + WEIGHT, data = counseling_data)
                             sol <- summary(cumulmod)
                             sol$logLik
                           },
                           
                           #solution Q13
                           {
                             cumulmod = clm(Y ~ GENDER + DOSE + WEIGHT, data = counseling_data)
                             predictions=predict(cumulmod, newdata = subset(counseling_data, select = -Y))$fit
                             predictions[20, 1]
                           },
                           
                           #solution Q14
                           {
                             cumulmod = clm(Y ~ GENDER + DOSE + WEIGHT, data = counseling_data)
                             predictions=predict(cumulmod, newdata = subset(counseling_data, select = -Y))$fit
                             predictions[10, 2]
                           },
                           
                           #solution Q15
                           {
                             cumulmod = clm(Y ~ GENDER + AGE + WEIGHT, data = counseling_data)
                             predictions=predict(cumulmod, newdata = subset(counseling_data, select = -Y))$fit
                             predictions[20, 1]
                           },
                           
                           #solution Q16
                           {
                             cumulmod = clm(Y ~ GENDER + AGE + WEIGHT, data = counseling_data)
                             predictions=predict(cumulmod, newdata = subset(counseling_data, select = -Y))$fit
                             predictions[10, 2]
                           }
    )
  }
  
  return(round(solutions, digits = 3))
  
}


### function to compare answers with rounding 
comp <- function(answer, solution){
  
  grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.0011, scale = 1)), 1, 0)
  
  return(grade)
}

#some questions precision only to 0.01
comp2 <- function(answer, solution){
  
  grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.011, scale = 1)), 1, 0)
  
  return(grade)
}

### feedback function 
# data frame to store all scores 
all_scores <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(all_scores) <- c("Student ID", "Vraag 1", "Vraag 2", "Vraag 3", "Vraag 4", "Total score")

feedBACK <- function(ID, i, ID_resp, ID_sol){
  
  # get ID responses  
  feedback = vector(mode="character", length=length(ID_sol))
  
  #compare responses with correct results 
  for (q in 1:length(ID_sol)){
    if(q != 3){
      feedback[q] <- comp(answer = ID_resp[q], solution = ID_sol[q])
    } else { # for question 3 only 2 digits accuracy 
      feedback[q] <- comp2(answer = ID_resp[q], solution = ID_sol[q])
    }
    
  }
  
  #store feedback in table 
  
  Head <- paste("Student ID: ", as.character(ID), "\n",
                "Last Name:", user_info |> filter(User.Name == as.character(ID)) |> dplyr::select("Last.Name"), "\n", 
                "First Name:", user_info |> filter(User.Name == as.character(ID)) |> dplyr::select("First.Name"), "\n\n")
  
  feedbacktext = rbind(c("Question", "Your answer", "Correct answer", "Grade"),
                       c("1", ID_resp[1], round(ID_sol[1], digits = 3), feedback[1]),
                       c("2", ID_resp[2], round(ID_sol[2], digits = 3), feedback[2]),
                       c("3", ID_resp[3], round(ID_sol[3], digits = 3), feedback[3]),
                       c("4", ID_resp[4], round(ID_sol[4], digits = 3), feedback[4]),
                       c(""),
                       c(paste("Total score = ", sum(as.numeric(feedback)), "/", length(ID_sol))," ", " ", "")) |>
    as.data.frame()
  
  
  #write individual feedback file
  filenameW <- paste(user_info |> filter(`User.Name` == ID) |> pull(newid), "_feedback.txt", sep="")
  indfolder= paste0("W:\\TASK3\\3.FEEDBACK")
  filepathW <- paste0(indfolder,"\\",filenameW)
  
  filename <- paste(ID, "_feedback.txt", sep="")
  filepathB <- paste0("2.INDIVIDUAL\\3.FEEDBACK\\", filename)
  
  writeLines(Head, filepathW)
  write.fwf(feedbacktext, file=filepathW, width = 20, colnames = F, justify = 'left', append = T)
  
  writeLines(Head, filepathB)
  write.fwf(feedbacktext, file=filepathB, width = 20, colnames = F, justify = 'left', append = T)
  
  #return grades  
  return(feedback)
  
}


#### OVERALL FUNCTION 
gradingTOOL <- function(responses, solutions){
  
  #respondents ID 
  group_IDS <- as.character(user_info$User.Name)
  group_IDS <- na.omit(group_IDS)
  
  NA_IDS <-  group_IDS[!group_IDS %in% responses$Username]
  
  #dataframe to store all info
  all_info <- as.data.frame(matrix(data = NA, ncol = 18))
  
  #for each respondent 
  for (i in 1:length(group_IDS)){
    
    ID <- group_IDS[i]
    
    #####
    ## Get respondent answers if submitted 
    #####
    if(ID %in% responses$Username){
      ID_resp <- as.numeric(as.character(responses$Answer[responses$Username == ID]))
    } else {
      ID_resp <- rep(NA, 4)
    }
    
    #####
    ## Get respondent solutions and questions
    #####

    ID_sol = solutions |> filter(User.Name == as.character(ID)) |> 
      dplyr::select(-(contains(c("Name", "Q")))) |> 
      mutate(across(where(is.double), \(x) round(x, 3))) |> 
      unlist() |> as.vector() 
    
    ID_quest <- solutions |> filter(User.Name == as.character(ID)) |> 
      dplyr::select(-(contains(c("Name", "S")))) |> unlist() |> as.vector()
    
    #####
    ## Write feedback reports and store overall grades
    #####

    feedback <- feedBACK(ID=ID, i=i, ID_resp = ID_resp, ID_sol = ID_sol)
    
    all_info[i, ] <- c(ID, 
                       ID_quest[1], ID_resp[1], ID_sol[1],  feedback[1],
                       ID_quest[2], ID_resp[2], ID_sol[2],  feedback[2],
                       ID_quest[3], ID_resp[3], ID_sol[3],  feedback[3],
                       ID_quest[4], ID_resp[4], ID_sol[4],  feedback[4],
                       sum(as.numeric(feedback)))
  } 
  colnames(all_info) <- c("ID", "Q1", "R1", "S1", "G1", "Q2", "R2", "S2", "G2", 
                          "Q3", "R3", "S3", "G3", "Q4", "R4", "S4", "G4","TOTAL")
  
  #not participated
  not_PP <- group_IDS %in% NA_IDS
  all_info$Participated <- !not_PP
  
  return(all_info)
  
}




