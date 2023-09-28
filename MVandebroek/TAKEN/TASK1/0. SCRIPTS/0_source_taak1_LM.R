########################################
### script containing all functions ####
########################################

#libraries
library("readxl")
library("dplyr")
library("gdata")
library("car")
library(tidyverse)

#No scientific notation
options(scipen = 999)
#print 10 digits (default = 7)
options(digits=10)

### check the type of the data files > this test with txt files !!!
### read in individual dataset
getDATA <- function(ID){
  
  WD <- "C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK1\\2. INDIVIDUAL\\"
  filename <- paste(WD, "1. DATA\\data",ID,".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}


#setwd('C:\\Users\\u0105757\\Dropbox\\Toledo 2021-2022\\datafiles\\HI_taak1\\ind_datafiles')
#data <- getDATA(ID = "q0980093")

### solution function
getSOL <- function(data, questions){
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    data$Department <- as.factor(data$Department)
    data$Gender <- as.factor(data$Gender)
    
    solutions[i] <- switch(Q,  
                           
                           #solution Q1
                           {
                             mod = lm(Salary~ Experience + Employed + Education, data=data)
                             vif(mod)[3] 
                           },
                           
                           #solution Q2
                           {
                             mod = lm(Salary~ Experience + Employed + Education, data=data)
                             vif(mod)[2] 
                           },
                           
                           #solution Q3
                           {
                             mod = lm(Salary~ Experience + Employed + Education, data=data)
                             vif(mod)[1] 
                           },
                           
                           #solution Q4
                           {
                             model = lm(Salary ~ Gender * Employed, data = data)
                             preddata = data.frame(Gender = "F", Employed = 30)
                             
                             #prediction interval for individual salary
                             result <- predict(model, newdata = preddata, interval="prediction") 
                             result[3]
                           },
                           
                           #solution Q5
                           {
                             model = lm(Salary ~ Gender * Employed, data = data)
                             preddata = data.frame(Gender = "M", Employed = 30)
                             
                             #prediction interval for individual salary
                             result <- predict(model, newdata = preddata, interval="prediction") 
                             result[3]
                           },
                           
                           #solution Q6
                           {
                             model = lm(Salary ~ Gender * Employed, data = data)
                             preddata = data.frame(Gender = "F", Employed = 30)
                             
                             #prediction interval for individual salary
                             result <- predict(model, newdata = preddata, interval="prediction") 
                             result[2]
                           },
                           
                           #solution Q7
                           {
                             outliermod <- lm(Salary~Employed, data=data)
                             infl <- influence.measures(outliermod)
                             infl$infmat[25, 5] * 1000
                             
                           },
                           
                           #solution Q8
                           {
                             outliermod <- lm(Salary~Experience, data=data)
                             infl <- influence.measures(outliermod)
                             infl$infmat[35, 5] * 1000
                           },
                           
                           #solution Q9
                           {
                             outliermod <- lm(Salary~Education, data=data)
                             infl <- influence.measures(outliermod)
                             infl$infmat[15, 5] * 1000
                           }, 
                           
                           #solution Q10
                           {
                             outliermod = lm(Salary~Employed, data=data)
                             inflmeasures= influence.measures(outliermod)
                             inflmeasures$infmat[20, 6]
                           }, 
                           
                           #solution Q11
                           {
                             outliermod = lm(Salary~Experience, data=data)
                             inflmeasures= influence.measures(outliermod)
                             inflmeasures$infmat[12, 6]
                           },
                           
                           #solution Q12
                           {
                             outliermod = lm(Salary~Education, data=data)
                             inflmeasures= influence.measures(outliermod)
                             inflmeasures$infmat[35, 6]
                           },
                           
                           #solution Q13
                           {
                             salary_anova = aov(Salary~ Department, data = data)
                             tukey = TukeyHSD(salary_anova)
                             sum(tukey$'Department'[, 4] < 0.05)
                           },
                           
                           #solution Q14
                           {
                             salary_anova = aov(Salary~ Department, data = data)
                             tukey = TukeyHSD(salary_anova)
                             tukey$'Department'[3, 4] 
                           },
                           
                           #solution Q15
                           {
                             salary_anova = aov(Salary~ Department, data = data)
                             tukey = TukeyHSD(salary_anova)
                             tukey$'Department'[4, 4] 
                           },
                           
                           #solution Q16
                           {
                             salary_anova = aov(Salary ~ Department, data = data)
                             tukey = TukeyHSD(salary_anova)
                             tukey$'Department'[5, 4] 
                           },
                           
                           #solution Q17
                           {
                             groupmeans <- c(55,60,57,62)
                             result <- power.anova.test(groups = length(groupmeans),  
                                                        between.var = var(groupmeans), within.var = 25, sig.level = 0.05, n = 10)
                             result$power 
                           },
                           
                           #solution Q18
                           {
                             groupmeans <- c(55,60,57,62)
                             result <- power.anova.test(groups = length(groupmeans),  
                                                        between.var = var(groupmeans), within.var = 36, sig.level = 0.05, n = 20)
                             result$power 
                           },
                           
                           #solution Q19
                           {
                             groupmeans <- c(55,60,57,62)
                             result <- power.anova.test(groups = length(groupmeans),  
                                                        between.var = var(groupmeans), within.var = 25, sig.level = 0.10, n = 10)
                             result$power
                           },
                           
                           #solution Q20
                           {
                             twoway = lm(Salary ~ Department * Gender, data = data)
                             aov <- anova(twoway)
                             aov$`Pr(>F)`[3]
                           },
                           
                           #solution Q21
                           {
                             twoway = lm(Salary ~ Department * Gender, data = data)
                             test <- summary(twoway)
                             test$fstatistic[1]
                           },
                           
                           #solution Q22
                           {
                             twoway = lm(Salary ~ Department * Gender, data = data)
                             aov <- anova(twoway)
                             aov$`Mean Sq`[4]
                           }
    )
  }
  
  return(round(solutions, digits = 5))
  
}


### function to compare answers with rounding 
comp <- function(answer, solution){
  
  if (solution > 1000){ #output question 1 can only have 2 decimals 
    grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.011, scale = 1)), 1, 0)
  }
  
  if (solution < 1000){ #remaining questions 
    grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.0011, scale = 1)), 1, 0)
  }
  
  
  return(grade)
}
# 
# 
# ### feedback function 
# # data frame to store all scores 
# all_scores <- data.frame(matrix(ncol = 6, nrow = 0))
# colnames(all_scores) <- c("User.Name", "Q1", "Q2", "Q3", "Q4", "Total score")
# 
# 

###-----------------------------------------------------
#### 
gradingTOOL <- function(responses, solutions){
  
  #respondents ID 
  group_IDS <- as.character(user_info$User.Name)
  group_IDS <- na.omit(group_IDS)
  
  NA_IDS <-  group_IDS[!group_IDS %in% responses$User.Name]
  
  #dataframe to store all info and feedback
  #all_info <- as.data.frame(matrix(data = NA, ncol = 18))
  all_info <- as.data.frame(matrix(data = NA, ncol = 18))
  colnames(all_info) <- c("ID", "Q1", "R1", "S1", "G1", "Q2", "R2", "S2", "G2", 
                          "Q3", "R3", "S3", "G3", "Q4", "R4", "S4", "G4","TOTAL")
  
  
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
      select(-(contains(c("Name", "Q")))) |> mutate(across(where(is.double), round, 3)) |> 
      unlist() |> as.vector() 
    
   ID_quest <- solutions |> filter(User.Name == as.character(ID)) |> 
     select(-(contains(c("Name", "S")))) |> unlist() |> as.vector()
    
    #####
    ## Write feedback in file and store overall grades
    #####
    
    
    #compare responses with correct results
    feedback = vector(mode="character", length=length(ID_sol))
    
    for (q in 1:length(ID_sol)){
      feedback[q] <- comp(answer = ID_resp[q], solution = ID_sol[q])
    }
    
    #store feedback in table
   
   Head <- paste("Student ID: ", as.character(ID), "\n",
                 "Last Name:", user_info |> filter(User.Name == as.character(ID)) |> select("Last.Name"), "\n", 
                 "First Name:", user_info |> filter(User.Name == as.character(ID)) |> select("First.Name"), "\n\n")
   
   feedbacktext = rbind(c("Question", "Your answer", "Correct answer", "Grade"),
                        c("1", ID_resp[1], round(ID_sol[1], digits = 3), feedback[1]),
                        c("2", ID_resp[2], round(ID_sol[2], digits = 3), feedback[2]),
                        c("3", ID_resp[3], round(ID_sol[3], digits = 3), feedback[3]),
                        c("4", ID_resp[4], round(ID_sol[4], digits = 3), feedback[4]),
                        c(""),
                        c(paste("Total score = ", sum(as.numeric(feedback)), "/", length(ID_sol))," ", " ", "")) |>
     as.data.frame()
    
    
    all_info[i, ] <- c(ID, ID_quest[1], ID_resp[1], ID_sol[1],  feedback[1],
                       ID_quest[2], ID_resp[2], ID_sol[2],  feedback[2],
                       ID_quest[3], ID_resp[3], ID_sol[3],  feedback[3],
                       ID_quest[4], ID_resp[4], ID_sol[4],  feedback[4],
                       sum(as.numeric(feedback)))
    
    # individual feedback files
    
    indfolder= paste0("W:\\TASK1\\dd",user_info[i,"newid"])
    
    filepathW <- paste0(indfolder,"\\3.feedback",user_info[i,"newid"],".txt")  
    filepathB <- paste0("2. INDIVIDUAL\\3. FEEDBACK\\feedback",user_info[i,"newid"],".txt") 
    filepathBx <- paste0("2. INDIVIDUAL\\3. FEEDBACK\\feedback",user_info[i,"User.Name"],".txt") 
    writeLines(Head, filepathW)
    write.fwf(feedbacktext, file=filepathW, width = 20, colnames = F, justify = 'left', append = T)
    
    writeLines(Head, filepathB)
    write.fwf(feedbacktext, file=filepathB, width = 20, colnames = F, justify = 'left', append = T)
    
    writeLines(Head, filepathBx)
    write.fwf(feedbacktext, file=filepathBx, width = 20, colnames = F, justify = 'left', append = T)
    
    
  } 

  return(all_info)
  
}


