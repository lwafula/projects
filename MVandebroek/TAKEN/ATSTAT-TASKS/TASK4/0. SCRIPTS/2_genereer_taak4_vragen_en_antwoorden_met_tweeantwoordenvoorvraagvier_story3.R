##########################################################
### Solutions with 2 possible answers for EFA questions  ###
##########################################################
rm(list=ls())

#library
library('readxl')
library('writexl')
library('stringi')
library(tidyverse)

#setwd
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK4"))

### Laad vragenpool en student numbers 

story = 3 

vragenpool_NED_ENG <- read_excel(path = paste0("1.FILES\\vragen_questions_HI_taak4_story",story, ".xlsx"))

#Read in Q-numbers 
user_info <- read_excel(path = "1.FILES/user_info with coding_TASK4.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 
any(is.na(user_info$group))

#source oplossingsfunctie 
source("0. SCRIPTS\\0_source_taak4.R")

#settings
I <- nrow(user_info)
N <- 4 #number of questions  

#store solutions
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

solutions_IQEFA <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQEFA) <- c("ID", paste0("S", 1:(N-1)), 'S4_2', paste0("Q", 1:N))

#set.seed
# set.seed(22234)

## Read in correct questions
AllQIs <- read_xlsx("1.FILEs\\solutions_taak4.xlsx") |> rename("User.Name" = `ID`) |> 
  dplyr::select("User.Name", paste0("Q", 1:N))

for(i in 1:I){
  
  #read in data
  
  ID <- user_info[i , "Username"]
  
  #story3
  
  data1 <- getDATA(ID, type = "1")
  data2 <- getDATA(ID, type = "2")
  data3 <- getDATA(ID, type = "3")
  
  # get questions assigned
  
  QI <- AllQIs |> filter(`User.Name` == as.character(ID)) |> dplyr::select(contains("Q")) |> as.vector() |>
    unname() |> unlist() |> sort()
  
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  
  #story3
  solutions_IQ[i, 2:9] <- c(get(paste0("getSOL_story", story))(data = list(data1, data2, data3), questions = QI), QI)
  
 # using ordered explained variances for EFA question
  solutions_IQEFA[i, 1] <- as.character(ID)
  solutions_IQEFA[i, 2:9] <- c(get(paste0("getSOL_story", story, "_orderedEFAvariances"))(data = list(data1, data2, data3), questions = QI), QI)
  
}


#save overall solutions 
write_xlsx(merge(solutions_IQ, solutions_IQEFA), path = c("1.FILES\\solutions_q4s_taak4.xlsx"))




