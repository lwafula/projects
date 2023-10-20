#########################################################
### Vragen toewijzing an antwoorden opslaan test taak ###
#########################################################
rm(list=ls())

#libraries
library(readxl)
library(writexl)
library(data.table)

personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK3"))

# include functions from 0_source_taak3.R

source("0. SCRIPTS/0_source_taak3.R")

### Laad vragenpool en student numbers 
vragenpool_NED_ENG <- read_excel(path ="1.FILES\\vragen_questions_taak3.xlsx")

#Read in Q-numbers 
# user_info <- read_excel(path = "1.FILES\\user_info with coding.xlsx")
user_info <- read_excel(path = "1.FILES/user_info with coding_TASK3.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 


I <- nrow(user_info)
N <- 4 #number of questions  


#store solutions S=Solution, Q=Question
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

# store questions, Q=Question, Qexp = Question explanation in appropriate language

vragen <- vector(mode="character", length=N)
questions_IQ <- data.frame(matrix(nrow = I, ncol = 2))
colnames(questions_IQ) <- c("User.Name", "Questions")

#replicability
set.seed(22231)


for(i in 1:I) {
  
  #draw for each i N questions  
  # QI <- c(sample(c(1:N), 1), N + sample(c(1:N), 1), (2*N) + sample(c(1:N), 1))
  
  # randomly select a question in each block: N in total: prediction, cook's D/hat value, one-way ANOVA, Type I/II/III SS
  
  QI <- vragenpool_NED_ENG |> filter(BLOK %in% c(2, sample(c(3, 4), 1), 5, 8)) |> group_by(BLOK) |> 
    slice_sample(n=1) |> ungroup() |> select(`Vraag ID`) |> as.vector() |> 
    unname() |> unlist() |> sort()
  QI
  
  #read in data
  # ID <- as.character(user_info[i , 'Student ID'])
  ID <- as.character(user_info[i , 'Username']) # Student ID or username? from 1_genereer
  idnewid = user_info[i,"newid"]
  data <- getDATA(ID)
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:(N*2+1)] <- c(getSOL(data = data, questions = QI), QI)
  
  
  #save questions dependent on language
  #group <- getLABEL(ID, user_info)
  
  indfolder= paste0("W:\\TASK3\\2.QUESTIONS")
  LABEL = user_info[i,"Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    vragen <- vector(mode="character", length=N)
    vragen  <- vragenpool_NED_ENG |> slice(QI) |> select('Vraag')
    
    questions_IQ[i, 1] <- as.character(ID)
    questions_IQ[i,2] = paste("Cursist ID: ", as.character(ID), "\n\n", 
                              paste0("Gebruik de data in https://feb.kuleuven.be/public/",personID,"/TASK3/1.DATA/data",idnewid,".txt"), "\n",
                              "De vragen voor deze taak staan hieronder vermeld.", "\n\n\n",
                              "V1:", vragen[1, ], "\n\n", "V2:", vragen[2, ],"\n\n", "V3:", vragen[3, ],"\n\n", "V4:", vragen[4, ], "\n\n\n",
                              "Vergeet kommagetallen niet af te ronden op 3 decimalen.")
    
    filepathW <- paste0(indfolder,"\\vragen",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"newid"],".txt") 
    # filepathBx <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"Username"],".txt") 
    write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(questions_IQ[i, -1], file = filepathB, quote = FALSE, row.names = FALSE, col.names = FALSE)
    # write.table(questions_IQ[i, -1], file = filepathBx, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    
  } else{
    
    vragen <- vector(mode="character", length=N)
    vragen  <- vragenpool_NED_ENG |> slice(QI) |> select('Question')
    
    questions_IQ[i, 1] <- as.character(ID)
    questions_IQ[i,2] = paste("Student ID: ", as.character(ID), "\n\n", 
                              paste0("Use the data in https://feb.kuleuven.be/public/",personID,"/TASK3/1.DATA/data",idnewid,".txt"), "\n",
                              "The questions for this task are listed below.", "\n\n\n",
                              "Q1:", vragen[1, ], "\n\n", "Q2:", vragen[2, ],"\n\n", "Q3:", vragen[3, ],"\n\n", "Q4:", vragen[4, ],"\n\n\n",
                              "Don't forget to round decimals to three digits.")
    
    filepathW <- paste0(indfolder,"\\questions",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"newid"],".txt") 
    # filepathBx <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"Username"],".txt") 
    write.table(questions_IQ[i, -1], file = filepathW, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(questions_IQ[i, -1], file = filepathB, quote = FALSE, row.names = FALSE, col.names = FALSE)
    # write.table(questions_IQ[i, -1], file = filepathBx, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    
  }
  
}


#save all questions and solutions
write_xlsx(solutions_IQ, path = "1.FILES\\solutions_taak3.xlsx")
write_xlsx(questions_IQ, path = "1.FILES\\indquestions_taak3.xlsx")


