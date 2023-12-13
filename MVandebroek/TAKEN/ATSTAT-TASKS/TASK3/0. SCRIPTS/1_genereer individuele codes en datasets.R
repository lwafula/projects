######################################
##### HI TAAK 3 DATA GENERATION ######
######################################
rm(list = ls())

##libraries
library(readxl)
library(writexl)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(data.table)

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK3"))

##############
### DATA 1 ###
##############
#story 3
SMEFailures_data <- read_excel("1.FILES/SMEFailures.xlsx") |>
  rename('FAILURE'=`STATUS`, "YEARS" = `SURVT`, 'EMPLOYEES' = `DOSE`, 
         'FAMILY' = `OXYGEN`, 'CHARACTER' = `CLINIC`) |>
  mutate(YEARS = round(YEARS/32, 0))
SMEFailures_sort <-  SMEFailures_data[order(SMEFailures_data$YEARS), ]
SMEFailures_data <- SMEFailures_sort[-c(1:30, 205:235), ]
data1 <- SMEFailures_data


##############
### DATA 2 ###
##############
#story 3
counseling_data <- read_excel("1.FILES\\datacounseling.xlsx", 1) |>
  rename('AGE'=`OPMAR`, "DOSE" = `LTDCAP`, 'GENDER' = `LEVER`,'WEIGHT' = `RECTURN`, 'Y' = `class`) |>
  mutate(Y = ifelse(Y=='A', '1', ifelse(Y == 'B', '2', '3')), AGE = round(AGE*250, 0), 
         DOSE = round((DOSE-1)*10,0), GENDER = ifelse(GENDER >= mean(GENDER), 1, 0),
         WEIGHT = scales::rescale(WEIGHT, c(45, 85)))

counseling_data[, -5] <- round(counseling_data[, -5], 3)
data2 <- counseling_data


###########################
### individual datasets ###
###########################

#Read in Q-numbers 
#set seed for replicability

#set seed 
set.seed(22233)

#Read in Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_TASK3.csv")
group_info = read.csv("1.FILES\\group info_TASK3.csv", check.names = F)

# students with unassigned groups
write.xlsx(olduser_info |> filter(!(Username %in% group_info$`User Name`)),"3.QUERIES\\students with no group assigned.xlsx")

user_info = merge(olduser_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings > GENEREER ALIASSEN VOOR DE STUDENTENNUMMERS en voeg group toe
I <- nrow(user_info)
N_1 <- 150 #dataset size 
N_2 <- 50

df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)

write.xlsx(user_info,"1.FILES\\user_info with coding_TASK3.xlsx")


# --- ---------------------------------------------------------------------


for(i in 1:I){
  
  #draw for each Q a subset from DATA, repeat untill some observations in each group
  sample1 <- sample_n(data1, size = N_1, replace = FALSE)
  sample2 <- sample_n(data2, size = N_2, replace = FALSE)
  
  #write individual datasets
  
  indfolder= paste0("W:\\TASK3\\1.DATA")
  
  filepathW1 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data1", ".txt")  # write to the public folder
  filepathW2 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data2", ".txt")
  
  filepathBx1 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data1",".txt")
  filepathBx2 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data2",".txt")
  
  write.table(sample1, file = filepathW1, quote = FALSE, row.names = FALSE)
  write.table(sample1, file = filepathBx1, quote = FALSE, row.names = FALSE)
  
  write.table(sample2, file = filepathW2, quote = FALSE, row.names = FALSE)
  write.table(sample2, file = filepathBx2, quote = FALSE, row.names = FALSE)
  
}


 
 