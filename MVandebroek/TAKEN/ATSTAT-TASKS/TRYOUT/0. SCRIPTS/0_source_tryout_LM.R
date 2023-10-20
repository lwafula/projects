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

personID = "u0118298"

### check the type of the data files > this test with txt files !!!
### read in individual dataset
getDATA <- function(ID){
  
  WD <- paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TRYOUT\\2.INDIVIDUAL\\")
  
  filename <- paste(WD, "1.DATA\\data",ID,".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}


### solution function
getSOL <- function(data, questions){
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    # data$Department <- as.factor(data$Department)
    # data$Gender <- as.factor(data$Gender)
    
    solutions[i] <- switch(Q,  
                           
                           #solution Q1
                           {
                             mod = lm(Y1 ~ X1 + X2, data=data)
                             as.numeric(coefficients(mod)['X2']) 
                           },
                           
                           #solution Q2
                           {
                             mod = lm(Y2 ~ X1 + X3, data=data)
                             as.numeric(coefficients(mod)['X3']) 
                           },
                           
                           #solution Q3
                           {
                             mod = lm(Y3 ~ X1 + X3, data=data)
                             as.numeric(coefficients(mod)['X1']) 
                           },
                           
                           #solution Q4
                           {
                             mod = lm(Y1 ~ X2*X4, data=data)
                             
                             # p-value for the interaction term
                             summary(mod)[['coefficients']] |> as.data.frame() |> 
                               rownames_to_column(var = 'term') |> 
                               filter(term == 'X2:X4') |> pull('Pr(>|t|)')
                           },
                           
                           #solution Q5
                           {
                             mod = lm(Y2 ~ X2*X5, data=data)
                             
                             # p-value for the interaction term
                             summary(mod)[['coefficients']] |> as.data.frame() |> 
                               rownames_to_column(var = 'term') |> 
                               filter(term == 'X2:X5') |> pull('Pr(>|t|)')
                           },
                           
                           #solution Q6
                           {
                             mod = lm(Y3 ~ X3*X4, data=data)
                             
                             # p-value for the interaction term
                             summary(mod)[['coefficients']] |> as.data.frame() |> 
                               rownames_to_column(var = 'term') |> 
                               filter(term == 'X3:X4') |> pull('Pr(>|t|)')
                           },
                           
                           #solution Q7
                           {
                             mod = lm(Y3 ~ X1 + X2 + X3, data=data)
                             summary(mod)[['r.squared']]
                             
                           },
                           
                           #solution Q8
                           {
                             mod = lm(Y1 ~ X2 + X3 + X4, data=data)
                             summary(mod)[['r.squared']]
                           },
                           
                           #solution Q9
                           {
                             mod = lm(Y2 ~ X3 + X4 + X5, data=data)
                             summary(mod)[['r.squared']]
                           },
                           #solution Q10
                           {
                             mod = lm(Y1 ~ X2, data=data)
                             (influence.measures(mod))[['infmat']][20,6]
                           },
                           #solution Q11
                           {
                             mod = lm(Y3 ~ X4, data=data)
                             (influence.measures(mod))[['infmat']][12,6]
                           },
                           #solution Q12
                           {
                             mod = lm(Y2 ~ X3, data=data)
                             (influence.measures(mod))[['infmat']][35,6]
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
  
  #dataframe to store all info and feedback: ID, quiz,resp,sol,grade
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
    
    # indfolder= paste0("W:TRYOUT\\dd",user_info[i,"newid"])
    # 
    # filepathW <- paste0(indfolder,"\\3.feedback",user_info[i,"newid"],".txt")  
    filepathB <- paste0("2.INDIVIDUAL\\3.FEEDBACK\\feedback",user_info[i,"newid"],".txt") 
    filepathBx <- paste0("2.INDIVIDUAL\\3.FEEDBACK\\feedback",user_info[i,"User.Name"],".txt") 
    # writeLines(Head, filepathW)
    # write.fwf(feedbacktext, file=filepathW, width = 20, colnames = F, justify = 'left', append = T)
    
    writeLines(Head, filepathB)
    write.fwf(feedbacktext, file=filepathB, width = 20, colnames = F, justify = 'left', append = T)
    
    writeLines(Head, filepathBx)
    write.fwf(feedbacktext, file=filepathBx, width = 20, colnames = F, justify = 'left', append = T)
    
    
  } 

  return(all_info)
  
}


