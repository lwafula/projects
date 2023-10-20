
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
library(tidyverse)

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK1"))


#set seed 
set.seed(22221)

#Read in Q-numbers 
# olduser_info <- read_xlsx("1. INPUT\\gc_ULTRA-C-17518426-K_columns_2023-06-13-15-45-33.xlsx")
olduser_info <- read.csv("1.FILES\\olduser_info_TASK1.csv") |> filter(!str_detect(tolower(Username), "preview"))
# group_info = read_xlsx("1.FILES\\group info.xlsx")
group_info = read.csv("1.FILES\\group info_TASK1.csv", check.names = F) |> filter(!str_detect(tolower(`User Name`), "preview"))

# students with unassigned groups
write.xlsx(olduser_info |> filter(!(Username %in% group_info$`User Name`)),"3.QUERIES\\students with no group assigned.xlsx")

user_info = merge(olduser_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings > GENEREER ALIASSEN VOOR DE STUDENTENNUMMERS en voeg group toe
I <- nrow(user_info)
N <- 50 #dataset size
df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)

write.xlsx(user_info,"1.FILES\\user_info with coding_TASK1.xlsx")


#------------------------------------------------------------------
###genereer algemene dataset voor regressie vragen. 
#set seed for replicability
set.seed(22231)

data <- read_xlsx("1.FILES\\salary_data.xlsx", 1)

#clean and add observations to global data file 
newdata1 <- data
newdata2 <- data
newdata3<- data
newdata4 <- data

newdata1[newdata1$Department == 4, ]$Salary <-  data[data$Department == 4, ]$Salary + rnorm(1, 500, 2000)
newdata1[newdata1$Department == 3, ]$Salary <-  data[data$Department == 3, ]$Salary + rnorm(1, 1000, 2000)
newdata1[newdata1$Department == 2, ]$Salary <-  data[data$Department == 2, ]$Salary + rnorm(1, -50, 2000)
newdata1[newdata1$Department == 1, ]$Salary <-  data[data$Department == 1, ]$Salary - rnorm(1, 5000, 2000)

newdata2[newdata2$Department == 4, ]$Salary <-  data[data$Department == 4, ]$Salary + rnorm(1, 100, 250)
newdata2[newdata2$Department == 3, ]$Salary <-  data[data$Department == 3, ]$Salary + rnorm(1, 1000, 2000)
newdata2[newdata2$Department == 2, ]$Salary <-  data[data$Department == 2, ]$Salary + rnorm(1, 50, 200)
newdata2[newdata2$Department == 1, ]$Salary <-  data[data$Department == 1, ]$Salary - rnorm(1, 250, 20)

newdata3[newdata3$Department == 4, ]$Salary <-  data[data$Department == 4, ]$Salary + rnorm(1, 3500, 1000)
newdata3[newdata3$Department == 3, ]$Salary <-  data[data$Department == 3, ]$Salary + rnorm(1, 100, 100)
newdata3[newdata3$Department == 2, ]$Salary <-  data[data$Department == 2, ]$Salary + rnorm(1, 0, 200)
newdata3[newdata3$Department == 1, ]$Salary <-  data[data$Department == 1, ]$Salary - rnorm(1, 500, 200)


data <- rbind(data, newdata1, newdata2, newdata3)
data$Employee <- 1:nrow(data)
DATA <- data[, -8]

colnames(DATA) <- c("ID", "Salary", "Experience", "Employed", "Education", "Gender", "Department")
DATA$Salary <- round(DATA$Salary, digits = 0)
DATA$Gender <- as.factor(DATA$Gender)
levels(DATA$Gender) <- c("M", "F") 


##------------------------------------------------------------------

###########################
### individual datasets ###
###########################



for(i in 1:I){
  
  #draw for each I, draw a subset from DATA, repeat until some observations in each group
  groups <- "notOK"
  while (groups != "OK"){
    sample_data <- sample_n(DATA, size = N, replace = FALSE)
    if(all(!(table(sample_data$Department) < 5))){groups <- "OK"}
  }
  
  
  #write individual datasets 
  indfolder= paste0("W:\\TASK1\\1.DATA")
  # dir.create(indfolder,showWarnings=TRUE, recursive = FALSE, mode = "0777")
  filepathW <- paste0(indfolder,"\\data",user_info[i,"newid"],".txt")  # write to the public folder
  filepathB <- paste0("2.INDIVIDUAL\\1.DATA\\", "data",user_info[i,"newid"],".txt")
  filepathBx <- paste0("2.INDIVIDUAL\\1.DATA\\", "data",user_info[i,"Username"],".txt")
  write.table(sample_data, file = filepathW, quote = FALSE, row.names = FALSE)
  write.table(sample_data, file = filepathB, quote = FALSE, row.names = FALSE)
  write.table(sample_data, file = filepathBx, quote = FALSE, row.names = FALSE)
}


