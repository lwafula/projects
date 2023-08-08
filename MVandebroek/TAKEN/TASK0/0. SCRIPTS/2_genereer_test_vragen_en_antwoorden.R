#########################################################
### Vragen toewijzing an antwoorden opslaan test taak ###
#########################################################
rm(list=ls())

#libraries
library(readxl)
library(writexl)
library(data.table)

# setwd('C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TASK0')
setwd("C:\\Users\\Public\\lmaaya\\Projects\\MVandebroek\\TAKEN\\TASK0")

# include functions from 0_source_taak1.R

source("0. SCRIPTS/0_source_taak1_LM.R")

### Laad vragenpool en student numbers 
vragenpool_NED_ENG <- read_excel(path ="1. FILES\\vragen_questions_TEST.xlsx")

#Read in Q-numbers 
user_info <- read_excel(path = "1. FILES\\user_info with coding.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 


I <- nrow(user_info)
N <- 3 #number of questions  


#store solutions S=Solution, Q=Question
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

# store questions, Q=Question, Qexp = Question explanation in appropriate language

vragen <- vector(mode="character", length=N)
questions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(questions_IQ) <- c("User.Name", paste0("Q", 1:N), paste0("Qexp", 1:N))

#replicability
set.seed(2223)


for(i in 1:I) {
  
  #draw for each i N questions  
  QI <- c(sample(c(1:N), 1), N + sample(c(1:N), 1), (2*N) + sample(c(1:N), 1))
  
  #read in data
  # ID <- as.character(user_info[i , 'Student ID'])
  ID <- as.character(user_info[i , 'Username']) # Student ID or username? from 1_genereer
  data <- getDATA(ID)
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:(N*2+1)] <- c(getSOL(data = data, questions = QI), QI)


  #save questions dependent on language
  #group <- getLABEL(ID, user_info)
  
  indfolder= paste0("C:\\Users\\Public\\lmaaya\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\")
  LABEL = user_info[i,"Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    
    questions_i <- vragenpool_NED_ENG |> slice(QI) |> select(contains('NED'))
    
    questions_IQ[i, 1:(length(QI)+1)] <- c(as.character(ID), QI)
    for(q in 1:NROW(questions_i)){
      questions_IQ[i, (length(QI)+1+q)] <- as.character(questions_i[q,1])
    }

    filepathW <- paste0(indfolder,"QUESTIONS\\vragen",user_info[i,"newid"],".txt")  
    filepathBx <- paste0(indfolder, "QUESTIONS\\vragen",user_info[i,"Username"],".txt") 
    write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE)
    write.table(questions_IQ[i, ], file = filepathBx, quote = FALSE, row.names = FALSE)
    
    # long formats
    
    colA = paste0("Q", 1:N)
    colB = paste0("Qexp", 1:N)
    questions_IQLong_i = melt(questions_IQ[i, ] |> as.data.table(), measure = list(colA, colB), 
                              value.name = c("Q", "Q exp"))[, !'variable'] |> as.data.frame()
    
    write_xlsx(questions_IQLong_i[, -1], 
               path = paste0(indfolder, "QUESTIONS\\LongFormat\\vragenLong",user_info[i,"newid"],".xlsx"))
    write_xlsx(questions_IQLong_i, 
               path = paste0(indfolder, "QUESTIONS\\LongFormat\\vragenLong",user_info[i,"Username"],".xlsx"))
    
    
    } else{
      questions_i <- vragenpool_NED_ENG |> slice(QI) |> select(contains('ENG'))
      
      questions_IQ[i, 1:(length(QI)+1)] <- c(as.character(ID), QI)
      for(q in 1:NROW(questions_i)){
        questions_IQ[i, (length(QI)+1+q)] <- as.character(questions_i[q,1])
      }
      
      filepathW <- paste0(indfolder,"QUESTIONS\\questions",user_info[i,"newid"],".txt")  
      filepathBx <- paste0(indfolder, "QUESTIONS\\questions",user_info[i,"Username"],".txt") 
      write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE)
      write.table(questions_IQ[i, ], file = filepathBx, quote = FALSE, row.names = FALSE)
      
      
      # long format
      colA = paste0("Q", 1:N)
      colB = paste0("Qexp", 1:N)
      questions_IQLong_i = melt(questions_IQ[i, ] |> as.data.table(), measure = list(colA, colB), 
                              value.name = c("Q", "Q exp"))[, !'variable'] |> as.data.frame()
      
      write_xlsx(questions_IQLong_i[, -1], 
                 path = paste0(indfolder, "QUESTIONS\\LongFormat\\questionsLong",user_info[i,"newid"],".xlsx"))
      write_xlsx(questions_IQLong_i, 
                 path = paste0(indfolder, "QUESTIONS\\LongFormat\\questionsLong",user_info[i,"Username"],".xlsx"))
      
      
    }

}


#save all questions and solutions
write_xlsx(solutions_IQ, path = "1. FILES\\solutions_taak0.xlsx")
write_xlsx(questions_IQ, path = "1. FILES\\indquestions_taak0.xlsx")

# long formats
colA = paste0("Q", 1:3)
colB = paste0("Qexp", 1:3)
questions_IQLong = melt(questions_IQ |> as.data.table(), measure = list(colA, colB), 
                        value.name = c("Q", "Q exp"))[order(User.Name), !'variable'] |> as.data.frame()

write_xlsx(questions_IQLong, path = "1. FILES\\indquestionsLong_taak0.xlsx")


