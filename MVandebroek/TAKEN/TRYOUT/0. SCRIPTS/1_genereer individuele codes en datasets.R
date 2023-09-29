
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

# setwd('C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TRYOUT')
setwd("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TRYOUT")


#set seed 
set.seed(2222)

#Read in Q-numbers 
# olduser_info <- read_xlsx("1. INPUT\\gc_ULTRA-C-17518426-K_columns_2023-06-13-15-45-33.xlsx")
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C-17518426-K_columns_2023-09-29-10-20-54.csv")
# group_info = read_xlsx("1. FILES\\group info.xlsx")
group_info = read.csv("1. FILES\\group info.csv", check.names = F, sep = ';')

# students with unassigned groups
write.xlsx(olduser_info |> filter(!(Username %in% group_info$`User Name`)),"3. QUERIES\\students with no group assigned.xlsx")

user_info = merge(olduser_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings > GENEREER ALIASSEN VOOR DE STUDENTENNUMMERS en voeg group toe
I <- nrow(user_info)
N <- 50 #dataset size 

df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)
user_info

write.xlsx(user_info,"1. FILES\\user_info with coding.xlsx")



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
  
  
  #write individual datasets 
  indfolder= paste0("W:\\TRYOUT\\dd",user_info[i,"newid"])
  dir.create(indfolder,showWarnings=TRUE, recursive = FALSE, mode = "0777")
  filepathW <- paste0(indfolder,"\\1.data",user_info[i,"newid"],".txt")  # write to the public folder
  filepathB <- paste0("2. INDIVIDUAL\\1. DATA\\", "data",user_info[i,"newid"],".txt")
  filepathBx <- paste0("2. INDIVIDUAL\\1. DATA\\", "data",user_info[i,"Username"],".txt") 
  write.table(sample_data, file = filepathW, quote = FALSE, row.names = FALSE)
  write.table(sample_data, file = filepathB, quote = FALSE, row.names = FALSE)
  write.table(sample_data, file = filepathBx, quote = FALSE, row.names = FALSE)
}


