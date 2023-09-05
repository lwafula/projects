
# https://www.html.am/html-codes/links/
# https://community.rstudio.com/t/how-to-preserve-hyperlinks-in-r-htmltable-while-writing-to-a-csv-excel-file/37728/2

library("readxl")
library("dplyr")
library("gdata")
library("car")
library(tidyverse)

# UPLOAD file 

rm(list = ls())

setwd("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK1")

#Read in the Q-numbers 
olduser_info <- read.csv("C:/Users/u0118298/OneDrive/Projects/MVandebroek/TAKEN/TASK0/1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-09-04-09-39-57.csv",
                         check.names = F)

group_info = read_xlsx("C:/Users/u0118298/OneDrive/Projects/MVandebroek/TAKEN/TASK0/1. FILES\\user_info with coding.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    # create html links for toledo
    # the public link points to the W: drive
    dataquizpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'],'/1. data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          # "<p></p>", 
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/2. vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    dataquizpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'],'/1. data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          # "<p></p>", 
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'], '/2. questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK0-116082023: data & quiz links [Total Pts: 1 Score] |118854` = row_number()) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1. FILES\\CHECKUPLOADFILE_DATAQUIZ.csv", 
            sep = ',', row.names = F)




# Feedback files ----------------------------------------------------------




#Read in the Q-numbers 
olduser_info <- read.csv("C:/Users/u0118298/OneDrive/Projects/MVandebroek/TAKEN/TASK0/1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-09-04-10-11-30.csv",
                         check.names = F)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){

  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    feedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'],'/3. feedback',dfUPLOAD[i,'newid'],'.txt',">here for your feedback</a></p>")
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
    
  } else{
    
    feedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/u0118298/dd', dfUPLOAD[i,'newid'],'/3. feedback',dfUPLOAD[i,'newid'],'.txt',">here for your feedback</a></p>")
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK0-116082023: feedback [Total Pts: 1 Score] |114607` = row_number()) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )


write.table(dfUPLOAD, file = "1. FILES\\FEEDBACKUPLOADFILE.csv", 
            sep = ',', row.names = F)

