

# https://www.html.am/html-codes/links/
# https://community.rstudio.com/t/how-to-preserve-hyperlinks-in-r-htmltable-while-writing-to-a-csv-excel-file/37728/2

library("readxl")
library("dplyr")
library("gdata")
library("car")
library(tidyverse)

# UPLOAD file 

rm(list = ls())

setwd("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0")

#Read in the Q-numbers 
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-09-11-14-22-26.csv",
                         check.names = F)

group_info = read_xlsx("1. FILES\\user_info with coding.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    # create html links for toledo
    # the public link points to the W: drive
    dataquizpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'],'/1.data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          # "<p></p>", 
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/2.vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>",
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/3.feedback',dfUPLOAD[i,'newid'],'.txt',">here for your feedback</a></p>")
    
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    dataquizpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/1.data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          # "<p></p>", 
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/2.questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>",
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/3.feedback',dfUPLOAD[i,'newid'],'.txt',">here for your feedback</a></p>")
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
  }
  
}

# to mutate a column with contains: e.g. TASK column changes with a new 
# https://www.statology.org/dplyr-mutate-if-contains/

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Marking Notes` = as.character("Marking Notes"), 
         `Notes Format` = 'SMART_TEXT', `Feedback Format` = 'HTML') |> 
  mutate_at(vars(contains('TASK')), ~ (1)) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1. FILES\\CHECKUPLOADFILE.csv", 
            sep = ',', row.names = F)