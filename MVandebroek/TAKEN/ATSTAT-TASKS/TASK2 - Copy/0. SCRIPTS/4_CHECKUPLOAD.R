


# https://www.html.am/html-codes/links/
# https://community.rstudio.com/t/how-to-preserve-hyperlinks-in-r-htmltable-while-writing-to-a-csv-excel-file/37728/2

library("readxl")
library("dplyr")
library("gdata")
library("car")
library(tidyverse)

# UPLOAD file 

rm(list = ls())

personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK2"))

# link to shared folder
sharedFolder = "https://kuleuven-my.sharepoint.com/:t:/r/personal/leonard_maaya_kuleuven_be1/Documents/ATSTAT-TASKS"

#Read in the Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_TASK2.csv",
                         check.names = F)

group_info = read_xlsx("1.FILES\\user_info with coding_TASK2.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    # create html links for toledo
    # the public link points to the W: drive
    
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK2/1.DATA/data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK2/2.QUESTIONS/vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    # for the sharedFolder
    # dataquizfeedpath = paste0("<p>Click <a href=", sharedFolder,'/TASK2/2.INDIVIDUAL/1.DATA/data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
    #                       # "<p></p>", 
    #                       "<p>Click <a href=", sharedFolder,'/TASK2/2.INDIVIDUAL/2.QUESTIONS/vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    # 
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizfeedpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK2/1.DATA/data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK2/2.QUESTIONS/questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizfeedpath))
  }
  
}

# to mutate a column with contains: e.g. TASK column changes with a new 
# https://www.statology.org/dplyr-mutate-if-contains/

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Marking Notes` = as.character("Marking Notes"), 
         `Notes Format` = 'SMART_TEXT', `Feedback Format` = 'HTML') |> 
  mutate_at(vars(contains('TASK')), ~ (1)) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1.FILES\\CHECKUPLOADFILE_TASK2.csv", 
            sep = ',', row.names = F)


