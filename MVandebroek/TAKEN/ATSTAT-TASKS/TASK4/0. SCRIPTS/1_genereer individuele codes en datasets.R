#######################################
##### HIR TAAK 4 DATA GENERATION ######
#######################################
rm(list = ls())

##libraries
library(writexl)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(data.table)

#setwd
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK4"))

#set seed 
set.seed(22234)


###
## DATA 
###
#story 3
data <- read.table("1.FILES\\hospitals.txt") |> 
  rename('Industry'=`Academic`, "experience" = `stay`, 'publications' = `beds`,
         'masters' = `nurses`, 'ranking' = `quality`, 'phds' = `doctors`) |>
  mutate(masters = floor(scales::rescale(masters, c(200, 700))), 
         phds = floor(scales::rescale(phds, c(15, 50))),
         experience = scales::rescale(experience, c(7, 15)),
         age = scales::rescale(age, c(35, 50)))

# data2

data2 <- read.table("1.FILES/calories.txt", header=T) |> dplyr::select(-1) |>
  stats::setNames(paste0("V", 1:5)) 

data2x = matrix(data = NA, nrow = nrow(data2), ncol = ncol(data2))

for(c in 1:ncol(data2x)){
  data2x[, c] <- sample(data2[, c], nrow(data2x))
}

data2 <- data2 |> add_row(data2x |> as.data.frame())

# data3: EFA

# https://rpubs.com/Symrna/EFA_CFA
# saq <- foreign::read.spss("https://stats.idre.ucla.edu/wp-content/uploads/2018/05/SAQ.sav", to.data.frame=TRUE, use.value.labels = FALSE)
# write_xlsx(saq, path = "1.FILES\\EFA_SAQ_taak4.xlsx")

numberMultiples = rep(1, 2571) %o% sample(seq(1, 23, 0.01), 23)
data3 <- read_xlsx("1.FILES/EFA_SAQ_taak4.xlsx")
data3 = data3*numberMultiples

###########################
### individual datasets ###
###########################


#Read in Q-numbers 
user_info <- read.csv("1.FILES\\olduser_info_TASK4.csv")
group_info = read.csv("1.FILES\\group info_TASK4.csv", check.names = F)

# students with unassigned groups
write.xlsx(user_info |> filter(!(Username %in% group_info$`User Name`)),"3.QUERIES\\students with no group assigned.xlsx")

user_info = merge(user_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings
I <- nrow(user_info)
N_1 <- 100 #dataset size 
N_2 <- 27
N_3 <- 1000

df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)

write.xlsx(user_info,"1.FILES\\user_info with coding_TASK4.xlsx")


for(i in 1:I){
  
  #draw for each Q a subset from DATA,
  sample_data1 <- data |> slice_sample(n = N_1, replace = FALSE)
  sample_data2 <- data2 |> slice_sample(n = N_2, replace = FALSE)
  sample_data2 = scale(sample_data2)
  sample_data3 <- data3 |> slice_sample(n = N_3, replace = FALSE)
  
  #write individual datasets
  
  indfolder= paste0("W:\\TASK4\\1.DATA")
  
  filepathW1 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data1", ".txt")  # write to the public folder
  filepathW2 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data2", ".txt")
  filepathW3 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data3", ".txt")
  
  filepathBx1 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data1",".txt")
  filepathBx2 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data2",".txt")
  filepathBx3 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data3",".txt")
  
  write.table(sample_data1, file = filepathW1, quote = FALSE, row.names = FALSE)
  write.table(sample_data2, file = filepathW2, quote = FALSE, row.names = FALSE)
  write.table(sample_data3, file = filepathW3, quote = FALSE, row.names = FALSE)
  
  write.table(sample_data1, file = filepathBx1, quote = FALSE, row.names = FALSE)
  write.table(sample_data2, file = filepathBx2, quote = FALSE, row.names = FALSE)
  write.table(sample_data3, file = filepathBx3, quote = FALSE, row.names = FALSE)
  
}


