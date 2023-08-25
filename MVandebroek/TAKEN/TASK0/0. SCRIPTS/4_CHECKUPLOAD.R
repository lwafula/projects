
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
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-08-23-13-59-08.csv",
                         check.names = F)

group_info = read_xlsx("1. FILES\\user_info with coding.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  dataquizpath = paste0("<p>Click <a href=", 'https://kuleuven-my.sharepoint.com/:f:/r/personal/martina_vandebroek_kuleuven_be/Documents/Desktop/TAKEN/LMaaya/TASK0/2.%20INDIVIDUAL/1.%20DATA?csf=1&web=1&e=pvidg5\\data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                        # "<p></p>", 
                        "<p>Click <a href=", 'https://kuleuven-my.sharepoint.com/:f:/r/personal/martina_vandebroek_kuleuven_be/Documents/Desktop/TAKEN/LMaaya/TASK0/2.%20INDIVIDUAL/2.%20QUESTIONS?csf=1&web=1&e=MDHT9X\\questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
  
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
    
  } else{
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK 0: data & quiz links [Total Pts: 1 Score] |114592` = 1) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1. FILES\\CHECKUPLOADFILE_DATAQUIZ.csv", 
            sep = ',', row.names = F)




# Feedback files ----------------------------------------------------------




#Read in the Q-numbers 
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-08-23-14-59-37.csv",
                         check.names = F)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  feedpath = paste0("<p>Click <a href=", 'https://kuleuven-my.sharepoint.com/:f:/r/personal/martina_vandebroek_kuleuven_be/Documents/Desktop/TAKEN/LMaaya/TASK0/2.%20INDIVIDUAL/3.%20FEEDBACK?csf=1&web=1&e=QY8l6T\\feedback',dfUPLOAD[i,'newid'],'.txt',">here for your feedback</a></p>")
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
    
  } else{
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK0-116082023: feedback [Total Pts: 1 Score] |114607` = 1) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )


write.table(dfUPLOAD, file = "1. FILES\\FEEDBACKUPLOADFILE.csv", 
            sep = ',', row.names = F)

