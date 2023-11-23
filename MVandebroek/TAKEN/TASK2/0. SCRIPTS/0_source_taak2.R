########################################
### script containing all functions ####
########################################

#libraries
library("readxl")
library("dplyr")
library("MASS")
library("tidyverse")
library("gdata")
library(plm)
library(lmtest)
library(car)
library(Hmisc)
library(data.table)
library(AER)
library(pcse)
library(lme4)
library(parameters)
library(clubSandwich)
library(aTSA)


#No scientific notation
options(scipen = 999)

#print 10 digits (default = 7)
options(digits=10)

personID = "u0118298"

### check the type of the data files > this test with txt files !!!
### read in individual dataset
getDATA <- function(ID, type){
  
  WD <- paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK2\\2.INDIVIDUAL\\")
  
  filename <- paste(WD, "1.DATA\\",ID, "_data", type, ".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}

### function to get group label
getLABEL <- function(ID, user_info){
  
  label <- user_info$`group`[user_info$`Username` == as.character(ID)]
  
  return(label)
}


### solution function DEPENDS ON THE STORY USED (Different variable names) !!!
getSOL_story1 <- function(data, questions){
  
  Municipal <- data[[1]]
  infl <- data[[2]]
  solutions <- numeric(length = length(questions))
  
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             Muni1982 = subset(Municipal, YEAR == 1982)
                             reg = lm(EXPEND~GRANTS, data = Muni1982)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q2
                           {
                             Muni1984 = subset(Municipal, YEAR == 1984)
                             reg = lm(EXPEND~GRANTS, data = Muni1984)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q3
                           {
                             Muni1982 = subset(Municipal, YEAR == 1982)
                             reg = lm(EXPEND~REVENUE, data = Muni1982)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           
                           #solution Q4
                           {
                             Muni1984 = subset(Municipal, YEAR == 1984)
                             reg = lm(EXPEND~REVENUE, data = Muni1984)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q5
                           {
                             panel <- pdata.frame(Municipal, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(EXPEND ~ GRANTS, data=panel, model="within")
                             coeftest(plm_fixed, vcov = vcovCR(plm_fixed, type ="CR0"))[3]
                           },
                           
                           #solution Q6
                           {
                             panel <- pdata.frame(Municipal, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(EXPEND ~ GRANTS, data=panel, model="within")
                             plm_random <- plm(EXPEND ~ GRANTS, data=panel, model="random")
                             as.numeric(phtest(plm_fixed, plm_random)[1])
                             
                           },
                           
                           #solution Q7
                           {
                             
                             plm_random <- plm(formula = EXPEND ~ REVENUE, data = Municipal, model = "random")
                             obj <- summary(plm_random)
                             obj$ercomp$sigma2[1]
                             
                           },
                           
                           #solution Q8
                           {
                             plm_random <- plm(formula = EXPEND ~ REVENUE, data = Municipal, model = "random")
                             obj <- summary(plm_random)
                             sqrt(obj$ercomp$sigma2[1])
                             
                           },
                           
                           
                           #solution Q9
                           {
                             ct <- adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type3[7]
                             
                           },
                           
                           #solution Q10
                           {
                             ct=adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type2[6]
                           }, 
                           
                           #solution Q11
                           {
                             ct=adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type1[8]
                           },
                           
                           #solution Q12
                           {
                             infl$L1F = Lag(infl$inflation, 1)  
                             infl$L2F = Lag(infl$inflation, 2) 
                             regmod = lm(inflation~year+L1F+L2F, data=infl)
                             as.numeric(bgtest(regmod, order = 1)[4])
                           },
                           
                           #solution Q13
                           {
                             infl$L1F = Lag(infl$inflation, 1)  
                             infl$L2F = Lag(infl$inflation, 2) 
                             regmod = lm(inflation~year+L1F+L2F, data=infl)
                             as.numeric(bgtest(regmod, order = 1)[1])
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}

getSOL_story2 <- function(data, questions){
  
  emission <- data[[1]]
  temp <- data[[2]]
  solutions <- numeric(length = length(questions))
  
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             emi1985 = subset(emission, YEAR == 1985)
                             reg = lm(CO2~CARS, data = emi1985)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                           },
                           
                           #solution Q2
                           {
                             emi1979 = subset(emission, YEAR == 1979)
                             reg = lm(CO2~CARS, data = emi1979)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                           },
                           
                           #solution Q3
                           {
                             emi1981 = subset(emission, YEAR == 1981)
                             reg = lm(CO2~CARS, data = emi1981)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           
                           #solution Q4
                           {
                             emi1983 = subset(emission, YEAR == 1983)
                             reg = lm(CO2~CARS, data = emi1983)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                           },
                           
                           #solution Q5
                           {
                             panel <- pdata.frame(emission, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(CO2 ~ CARS, data=panel, model="within")
                             coeftest(plm_fixed, vcov. = vcovCR(plm_fixed, type ="CR0"))[3]
                           },
                           
                           #solution Q6
                           {
                             panel <- pdata.frame(emission, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(CO2 ~ CARS, data=panel, model="within")
                             plm_random <- plm(CO2 ~ CARS, data=panel, model="random")
                             as.numeric(phtest(plm_fixed, plm_random)[1])
                           },
                           
                           #solution Q7
                           {
                             plm_random <- plm(formula = CO2 ~ COWS, data = emission, model = "random")
                             obj <- summary(plm_random)
                             obj$ercomp$sigma2[1]
                           },
                           
                           #solution Q8
                           {
                             plm_random <- plm(formula = CO2 ~ COWS, data = emission, model = "random")
                             obj <- summary(plm_random)
                             sqrt(obj$ercomp$sigma2[1])
                           },
                           
                           #solution Q9
                           {
                             ct <- adf.test(temp$TEMP, nlag=4, output = FALSE)
                             ct$type3[7]
                           },
                           
                           #solution Q10
                           {
                             ct=adf.test(temp$TEMP, nlag=4, output = FALSE)
                             ct$type2[6]
                           }, 
                           
                           #solution Q11
                           {
                             ct=adf.test(temp$TEMP, nlag=4, output = FALSE)
                             ct$type1[8]
                           }, 
                           
                           #solution Q12
                           {
                             temp$L1F = Lag(temp$TEMP, 1)  
                             temp$L2F = Lag(temp$TEMP, 2) 
                             regmod = lm(TEMP~year+L1F+L2F, data=temp)
                             as.numeric(bgtest(regmod, order = 1)[4])
                           }, 
                           
                           #solution Q13
                           {
                             temp$L1F = Lag(temp$TEMP, 1)  
                             temp$L2F = Lag(temp$TEMP, 2) 
                             regmod = lm(TEMP~year+L1F+L2F, data=temp)
                             as.numeric(bgtest(regmod, order = 1)[1])
                             
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}

getSOL_story3 <- function(data, questions){
  
  Company <- data[[1]]
  infl <- data[[2]]
  solutions <- numeric(length = length(questions))
  
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             Comp1997 = subset(Company, YEAR == 1997)
                             reg = lm(REVENUE ~ COMPANYAGE, data = Comp1997)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q2
                           {
                             Comp1999 = subset(Company, YEAR == 1999)
                             reg = lm(REVENUE ~ COMPANYAGE, data = Comp1999)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q3
                           {
                             Comp1997 = subset(Company, YEAR == 1997)
                             reg = lm(REVENUE ~ RnDEXPEND, data = Comp1997)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           
                           #solution Q4
                           {
                             Comp1999 = subset(Company, YEAR == 1999)
                             reg = lm(REVENUE ~ RnDEXPEND, data = Comp1999)
                             whitecov = vcovHC(reg, type = "HC0")
                             coeftest(reg, vcov. = whitecov)[6]
                             
                           },
                           
                           #solution Q5
                           {
                             panel <- pdata.frame(Company, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(REVENUE ~ COMPANYAGE, data=panel, model="within")
                             coeftest(plm_fixed, vcov = vcovCR(plm_fixed, type ="CR0"))[3]
                           },
                           
                           #solution Q6
                           {
                             panel <- pdata.frame(Company, index=c("ID","YEAR"),  row.names=TRUE)
                             plm_fixed <- plm(REVENUE ~ COMPANYAGE, data=panel, model="within")
                             plm_random <- plm(REVENUE ~ COMPANYAGE, data=panel, model="random")
                             as.numeric(phtest(plm_fixed, plm_random)[1])
                             
                           },
                           
                           #solution Q7
                           {
                             
                             plm_random <- plm(formula = REVENUE ~ RnDEXPEND, data = Company, model = "random")
                             obj <- summary(plm_random)
                             obj$ercomp$sigma2[1]
                             
                           },
                           
                           #solution Q8
                           {
                             plm_random <- plm(formula = REVENUE ~ RnDEXPEND, data = Company, model = "random")
                             obj <- summary(plm_random)
                             sqrt(obj$ercomp$sigma2[1])
                             
                           },
                           
                           
                           #solution Q9
                           {
                             ct <- adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type3[7]
                             
                           },
                           
                           #solution Q10
                           {
                             ct=adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type2[6]
                           }, 
                           
                           #solution Q11
                           {
                             ct=adf.test(infl$inflation, nlag=4, output = FALSE)
                             ct$type1[8]
                           },
                           
                           #solution Q12
                           {
                             infl$L1F = Lag(infl$inflation, 1)  
                             infl$L2F = Lag(infl$inflation, 2) 
                             regmod = lm(inflation~year+L1F+L2F, data=infl)
                             as.numeric(bgtest(regmod, order = 1)[4])
                           },
                           
                           #solution Q13
                           {
                             infl$L1F = Lag(infl$inflation, 1)  
                             infl$L2F = Lag(infl$inflation, 2) 
                             regmod = lm(inflation~year+L1F+L2F, data=infl)
                             as.numeric(bgtest(regmod, order = 1)[1])
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

comp2 <- function(answer, solution){
  
  grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.011, scale = 1)), 1, 0)
  
  return(grade)
}


# 
# 
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
  indfolder= paste0("W:\\TASK2\\3.FEEDBACK")
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


###-----------------------------------------------------
#### 

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
    if(ID %in% responses$User.Name){
      ID_resp <- as.numeric(as.character(responses$Answer[responses$User.Name == ID]))
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


