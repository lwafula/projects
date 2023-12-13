#########################################################
### Vragen toewijzing an antwoorden opslaan test taak ###
#########################################################
rm(list=ls())

#libraries
library(readxl)
library(writexl)
library(data.table)
library(tidyverse)

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\Individualized TASKS"))

# include functions from 0_source_IndivTASKS.R

source("0. SCRIPTS/0_source_IndivTASKS.R")

### Laad vragenpool en student numbers 
vragenpool_NED_ENG <- read_excel(path ="1.FILES\\vragen_questions_IndivTASKS.xlsx") 

#Read in Q-numbers 
user_info <- read_excel(path = "1.FILES\\user_info with coding_Indiv_TASKS.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 


I <- nrow(user_info)
NB <- 3 #number of questions per block 
N <- 4 # number of questions to attempt

#store solutions S=Solution, Q=Question
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

# store questions, Q=Question, Qexp = Question explanation in appropriate language

vragen <- vector(mode="character", length=N)
questions_IQ <- data.frame(matrix(nrow = I, ncol = 2))
colnames(questions_IQ) <- c("User.Name", "Questions")

#replicability
set.seed(2223)


for(i in 1:I) {
  
  #draw for each i N questions  
  QI <- c(sample(c(1:NB), 1), NB + sample(c(1:NB), 1), (2*NB) + sample(c(1:NB), 1),
          (3*NB) + sample(c(1:NB), 1))
  
  #read in data
  # ID <- as.character(user_info[i , 'Student ID'])
  ID <- as.character(user_info[i , 'Username']) # Student ID or username? from 1_genereer
  idnewid = user_info[i,"newid"]
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:(N*2+1)] <- c(getSOL(questions = QI), QI)


  #save questions dependent on language
  #group <- getLABEL(ID, user_info)

  indfolder= paste0("W:\\IndividualizedTASKS\\2.QUESTIONS")
  LABEL = user_info[i,"Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "GRP1") {
    vragen <- vector(mode="character", length=N)
    vragen  <- vragenpool_NED_ENG |> dplyr::slice(QI) |> select(contains('NED'))
    
    questions_IQ[i, 1] <- as.character(ID)
    questions_IQ[i,2] = paste("Cursist ID: ", as.character(ID), "\n\n", 
                              "De vragen voor deze taak staan hieronder vermeld.", "\n\n\n",
                              "V1:", vragen[1, ], "\n\n", "V2:", vragen[2, ],"\n\n", "V3:", vragen[3, ],"\n\n", "V4:", vragen[4, ],"\n\n\n")
    
    filepathW <- paste0(indfolder,"\\vragen",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"newid"],".txt") 
    write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(questions_IQ[i, -1], file = filepathB, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    } else{
      
      vragen <- vector(mode="character", length=N)
      vragen  <- vragenpool_NED_ENG |> dplyr::slice(QI) |> select(contains('ENG'))
      
      questions_IQ[i, 1] <- as.character(ID)
      questions_IQ[i,2] = paste("Student ID: ", as.character(ID), "\n\n", 
                                "The questions for this task are listed below.", "\n\n\n",
                                "Q1:", vragen[1, ], "\n\n", "Q2:", vragen[2, ],"\n\n", "Q3:", vragen[3, ],"\n\n", "Q4:", vragen[4, ],"\n\n\n")
      
      filepathW <- paste0(indfolder,"\\questions",user_info[i,"newid"],".txt")  # write to the public folder
      filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"newid"],".txt") 
      write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE, col.names = FALSE)
      write.table(questions_IQ[i, -1], file = filepathB, quote = FALSE, row.names = FALSE, col.names = FALSE)

      
    }

}


#save all questions and solutions
write_xlsx(solutions_IQ, path = "1.FILES\\solutions_IndivTASKS.xlsx")


