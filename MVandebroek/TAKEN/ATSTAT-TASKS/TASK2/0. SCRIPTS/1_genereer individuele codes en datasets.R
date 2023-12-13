
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
library(data.table)
library(gdata)

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK2"))

#set seed for replicability

set.seed(22231)

#Read in Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_TASK2.csv")
# group_info = read_xlsx("1.FILES\\group info.xlsx")
group_info = read.csv("1.FILES\\group info_TASK2.csv", check.names = F)

# students with unassigned groups
write.xlsx(olduser_info |> filter(!(Username %in% group_info$`User Name`)),"3.QUERIES\\students with no group assigned.xlsx")

user_info = merge(olduser_info,group_info,by.x="Username",by.y = "User Name", all.x=TRUE) # keep those with complete group status

#settings > GENEREER ALIASSEN VOOR DE STUDENTENNUMMERS en voeg group toe
I <- nrow(user_info)
N <- 150 #dataset size

df <- data.frame(
  passwd = replicate(I, paste(sample(c(LETTERS, letters), 6), collapse="")))

user_info$newid<-as.character(df$passwd)

write.xlsx(user_info,"1.FILES\\user_info with coding_TASK2.xlsx")


#------------------------------------------------------------------

# company expenditures on social responsibility activities as a function of their revenue and tax grants they get from the government
# years: 1985 - 1993: firm performance (income) on their RnD + tax breaks/marketing/age
# some nice variables here: https://www.emerald.com/insight/content/doi/10.1108/CG-03-2022-0128/full/pdf (Prowess IQ database)

beta <- c(3.86, -0.31) # +ve RnD, -ve measure of age 
Nc = 265 # number of companies
cols2format = c('RnDEXPEND', 'COMPANYAGE')
std_fn <- function(x) (x - min(x))/(max(x) - min(x)) + 0.01

data1 = cbind(ID = sort(rep(1:Nc, 9))) |> as.data.table() %>% 
  .[, `:=`(YEAR = 1994:2002), by = ID] %>% group_by(ID) |> mutate(n = 1:n()) |>
  mutate("COMPANYAGE" = ifelse(n == 1, sample(seq(1, 15, 0.25), 1), 0)) |>
  mutate("COMPANYAGE" = ifelse(n > 1, `COMPANYAGE`[n==1] + (n-1), `COMPANYAGE`)) |>
  dplyr::select(-n) |> as.data.table() %>%
  .[, `:=`('RnDEXPEND' = exp(rlnorm(1, 0, 0.1))), by = .(ID, YEAR)] %>%
  mutate(across(all_of(cols2format), ~ std_fn(.))) |> rowwise() |>
  mutate(REVENUE = beta[1]*RnDEXPEND + beta[2]*COMPANYAGE + rnorm(1, 3, 0.01))


data2 <- read_excel("1.FILES/inflationtaak_meerlanden.xlsx") |>
  pivot_longer(!`Country Name`, names_to = "year", values_to = "inflation") |>
  mutate(year = as.numeric(year)) |> group_by(`Country Name`) |> 
  mutate(cID = dplyr::cur_group_id()) |> ungroup() |> arrange(cID)


for(i in 1:I){
  
  #data1
  ID_in <- sample(unique(data1$ID), size = N, replace = FALSE)
  sample_data1 <- data1[data1$ID %in% ID_in, ]
  
  #data2: for each student, select an inflation timeseries for a single country
  data2_1 = data2 |> filter(cID == sample(1:max(data2[['cID']]), 1)) |>
    dplyr::select(c(year, inflation))
  
  start <- sample(1:5, 1)
  end <- sample(1:5, 1)
  sample_data2<- data2_1[start:(nrow(data2_1)-end), ]
  
  #write individual datasets
  
  indfolder= paste0("W:\\TASK2\\1.DATA")
  
  filepathW1 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data1", ".txt")  # write to the public folder
  filepathW2 <- paste0(indfolder, "\\", user_info[i,"newid"], "_data2", ".txt")
  
  filepathBx1 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data1",".txt")
  filepathBx2 <- paste0("2.INDIVIDUAL\\1.DATA\\", user_info[i,"Username"], "_data2",".txt")
  
  write.table(sample_data1, file = filepathW1, quote = FALSE, row.names = FALSE)
  write.table(sample_data1, file = filepathBx1, quote = FALSE, row.names = FALSE)
  
  write.table(sample_data2, file = filepathW2, quote = FALSE, row.names = FALSE)
  write.table(sample_data2, file = filepathBx2, quote = FALSE, row.names = FALSE)
  
}


