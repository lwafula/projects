
#######################################
##### ATSTAT - test task
#######################################
rm(list = ls())

############
### DATA ### 
############

#libraries
library(readxl)
library(openxlsx)
library(dplyr)

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\Individualized TASKS"))


#set seed 
set.seed(2222)

#Read in Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_Dataandquestions_IndivTASKS.csv")
group_info = read.csv("1.FILES\\group info_IndivTASKS.csv", check.names = F)


# students with unassigned groups
write.xlsx(olduser_info |> filter(!(Username %in% group_info$`User Name`)),"3.QUERIES\\students with no group assigned.xlsx")

user_info = merge(olduser_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings > GENEREER ALIASSEN VOOR DE STUDENTENNUMMERS en voeg group toe
I <- nrow(user_info)
N <- 50 #dataset size 

df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)

write.xlsx(user_info,"1.FILES\\user_info with coding_Indiv_TASKS.xlsx")



#------------------------------------------------------------------
###genereer algemene dataset voor regressie vragen. 
#set seed for replicability
set.seed(2223)

beta1 <- c(-0.5, 1.4, 2.4, -1.1, -0.6)
beta2 <- c( 0.5, -0.4, 1.1, -2.1, -1.6)
beta3 <- c(1.2, 1.4, 1.7, -1.1, -0.8)

error1 <- rnorm(50, 0, 1)
error2 <- rnorm(50, 0, 0.5)
error3 <- rnorm(50, 0, 2)

DATA <- matrix(data = rnorm(1000), nrow = 200, ncol = 5)
DATA <- as.data.frame(DATA)
##------------------------------------------------------------------

###########################
### individual datasets ###
###########################



for(i in 1:I){
  
  #draw for each I a subset from DATA 
  sample_data <- as.matrix(sample_n(DATA, size = N, replace = TRUE))
  
  #Regression outcomes
  Y1 <- sample_data %*% beta1 + error1
  Y2 <- sample_data %*% beta2 + error2
  Y3 <- sample_data %*% beta3 + error3
  
  sample_data <- as.data.frame(cbind(sample_data, Y1, Y2, Y3))
  sample_data <- round(sample_data, digits = 2)
  colnames(sample_data) <- c("X1", "X2", "X3", "X4", "X5", "Y1", "Y2", "Y3")
  
  
  # write individual datasets 
  indfolder= paste0("W:\\IndividualizedTASKS\\1.DATA")
  filepathW <- paste0(indfolder,"\\data",user_info[i,"newid"],".txt")  # write to the public folder
  # filepathB <- paste0("2.INDIVIDUAL\\1.DATA\\", "data",user_info[i,"newid"],".txt")
  filepathBx <- paste0("2.INDIVIDUAL\\1.DATA\\", "data",user_info[i,"Username"],".txt") 
  write.table(sample_data, file = filepathW, quote = FALSE, row.names = FALSE)
  # write.table(sample_data, file = filepathB, quote = FALSE, row.names = FALSE)
  write.table(sample_data, file = filepathBx, quote = FALSE, row.names = FALSE)
}


