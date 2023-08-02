#########################################################
### Vragen toewijzing an antwoorden opslaan test taak ###
#########################################################
rm(list=ls())

#libraries
library(readxl)
library(writexl)

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
solutions_IQ <- data.frame(matrix(nrow = I, ncol = 7))
colnames(solutions_IQ) <- c("ID", "S1", "S2", "S3", "Q1", "Q2", "Q3")

#replicability
set.seed(2223)


for(i in 1:I) {
  
  #draw for each i 3 questions  
  QI <- c(sample(c(1:3), 1), 3 + sample(c(1:3), 1), 6 + sample(c(1:3), 1))
  
  #read in data
  # ID <- as.character(user_info[i , 'Student ID'])
  ID <- as.character(user_info[i , 'Username']) # Student ID or username? from 1_genereer
  data <- getDATA(ID)
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:7] <- c(getSOL(data = data, questions = QI), QI)
  
  #save questions dependent on language
  #group <- getLABEL(ID, user_info)
  
  if (group == "English") {
    
    questions_i <- vragenpool_NED_ENG[QI, 3]
    quests <- list()
    for(n in 1:N){
      quests[[n]] <- questions_i$`Question ENG`[n]
    }
    

#store solutions
vragen <- vector(mode="character", length=N)
questions_IQ <- data.frame(matrix(nrow = I, ncol = 2))
colnames(questions_IQ) <- c("User.Name", "Questions")
#settings to draw questions
bloklength <- table(vragenpool_NED_ENG$BLOK)
B_end <- cumsum(bloklength)

#replicability
set.seed(2223)

for(i in 1:I){
  
  #draw for each i 4 questions  
  QI <- c(sample(c(1:B_end[1]), 1),
          sample(c((B_end[1]+1) : B_end[2]), 1),
          sample(c((B_end[2]+1) : B_end[3]), 1),
          sample(c((B_end[3]+1) : B_end[4]), 1))
  
  QI <- vragenpool_NED_ENG$`Vraag ID`[QI]
  
  #read in data
  ID <- user_info[i , "User.Name"]
  idnewid = user_info[i,"newid"]
  data <- getDATA(ID)
  LABEL = user_info[i,"Group.Code"]
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:9] <- c(getSOL(data = data, questions = QI), QI)
  
  questions_IQ[i,1] =  as.character(ID) 
  
 
  if (!(LABEL == "TSTAT")){
    
    vragen  <- vragenpool_NED_ENG$Question[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    questions_IQ[i,2] = paste("Gebruik de data in https://feb.kuleuven.be/public/U0004359/data",idnewid,".txt",
                               "<br/>",vragen[1],"<br/>",vragen[2],"<br/>",vragen[3],"<br/>",vragen[4],
                              "<br/>","Don't forget to round decimals to three digits.")
  
  }
  
  if (LABEL == "TSTAT"){
    
    vragen  <- vragenpool_NED_ENG$Question[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    questions_IQ[i,2] = paste("Use the data in https://feb.kuleuven.be/public/U0004359/data",idnewid,".txt",
                              "<br/>",vragen[1],"<br/>",vragen[2],"<br/>",vragen[3],"<br/>",vragen[4],
                              "<br/>","Vergeet kommagetallen niet af te ronden op 3 decimalen.")
  
}
}


#save all questions and solutions
write_xlsx(solutions_IQ, path = "2. GRADING\\solutions_taak1.xlsx")
write_xlsx(questions_IQ, path = "1. INPUT\\indquestions_taak1.xlsx")




