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
  
  # WD <- "C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TAAK1\\3. DATA individual\\"
  WD <- "C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\"
  filename <- paste(WD, "DATA\\data",ID,".txt", sep = "") 
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
  all_info <- as.data.frame(matrix(data = NA, ncol = 14))
  colnames(all_info) <- c("ID", "Q1", "R1", "S1", "G1", "Q2", "R2", "S2", "G2", 
                          "Q3", "R3", "S3", "G3","TOTAL")
  
  feedbackfile = data.frame(matrix(nrow = nrow(user_info), ncol = 4))
  names(feedbackfile) = c("User.Name", "Last Name", "First Name", "Feedback")
  
  
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
    feedbacktext <- paste("Question 1: Your answer = ", ID_resp[1], " Correct answer = ",round(ID_sol[1], digits = 3), " Grade = ", feedback[1],"<br/>",
                          "Question 2: Your answer = ", ID_resp[2], " Correct answer = ",round(ID_sol[2], digits = 3), " Grade = ", feedback[2],"<br/>",
                          "Question 3: Your answer = ", ID_resp[3], " Correct answer = ",round(ID_sol[3], digits = 3), " Grade = ", feedback[3],"<br/>",
                          "Total score = ", sum(as.numeric(feedback)))
    
    #write individual feedback file
    feedbackfile[i,1] = ID
    feedbackfile[i,2:3] = user_info |> filter(User.Name == as.character(ID)) |> 
      select("Last.Name", "First.Name")
    feedbackfile[i,4] = feedbacktext
    
    
    
    all_info[i, ] <- c(ID, ID_quest[1], ID_resp[1], ID_sol[1],  feedback[1],
                       ID_quest[2], ID_resp[2], ID_sol[2],  feedback[2],
                       ID_quest[3], ID_resp[3], ID_sol[3],  feedback[3],
                       sum(as.numeric(feedback)))
    
    # individual feedback files
    
    indfolder= paste0("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\")
    # filepathW <- paste0(indfolder,"FEEDBACK\\feedback",user_info[i,"newid"],".xlsx")  
    # filepathBx <- paste0(indfolder, "FEEDBACK\\feedback",user_info[i,"User.Name"],".xlsx") 
    # write_xlsx(feedbackfile[i, ], path = filepathW)
    # write_xlsx(feedbackfile[i, ], path = filepathBx)
    
    filepathW <- paste0(indfolder,"FEEDBACK\\feedback",user_info[i,"newid"],".txt")  
    filepathBx <- paste0(indfolder, "FEEDBACK\\feedback",user_info[i,"User.Name"],".txt") 
    write.table(feedbackfile[i, ], file = filepathW, quote = FALSE, row.names = FALSE)
    write.table(feedbackfile[i, ], file = filepathBx, quote = FALSE, row.names = FALSE)
    
  } 

  
  #not participated
  #  not_PP <- group_IDS %in% NA_IDS
  #  all_info$PP <- !not_PP
  
  # write_xlsx(feedbackfile, path = "2. INDIVIDUAL\\FEEDBACK\\feedbackfile_all_taak0.xlsx")
  write.table(feedbackfile, file = "2. INDIVIDUAL\\FEEDBACK\\feedbackfile_all_taak0.txt", 
              quote = FALSE, row.names = FALSE)
  
  
  
  return(all_info)
  
}


