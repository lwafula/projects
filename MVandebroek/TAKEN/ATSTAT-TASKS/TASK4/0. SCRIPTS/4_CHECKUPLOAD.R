
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

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK4"))

#Read in the Q-numbers 

olduser_info <- read.csv("1.FILES\\olduser_info_TASK4.csv", check.names = F) |> 
  filter(!((str_detect(tolower(`First Name`), "bert")|str_detect(tolower(`First Name`), "elia")) & str_detect(tolower(`Last Name`), "preview")))

group_info = read_xlsx("1.FILES\\user_info with coding_TASK4.xlsx") |>
  dplyr::select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    # create html links for toledo
    # the public link points to the W: drive paste0(indfolder, "\\", user_info[i,"newid"], "_data1", ".txt")
    
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data1.txt',">here for the dataset1</a>",
                              ", <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data2.txt',">here for dataset2</a></p>",
                              " and <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data3.txt',">here for dataset3</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/2.QUESTIONS/vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/3.FEEDBACK/',dfUPLOAD[i,'newid'],'_feedback.txt',">here for your feedback</a></p>")
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizfeedpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data1.txt',">here for the dataset1</a>",
                              ", <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data2.txt',">here for dataset2</a></p>",
                              " and <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/1.DATA/', dfUPLOAD[i,'newid'],'_data3.txt',">here for dataset3</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/2.QUESTIONS/questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>",
                              "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/TASK4/3.FEEDBACK/',dfUPLOAD[i,'newid'],'_feedback.txt',">here for your feedback</a></p>")
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizfeedpath))
  }
  
}

# to mutate a column with contains: e.g. TASK column changes with a new 
# https://www.statology.org/dplyr-mutate-if-contains/

dfUPLOAD = dfUPLOAD |> dplyr::select(-c(`Group Code`, newid)) |>
  mutate(`Marking Notes` = as.character("Marking Notes"), 
         `Notes Format` = 'SMART_TEXT', `Feedback Format` = 'HTML') |> 
  mutate_at(vars(contains('TASK')), ~ (1)) |>
  dplyr::select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1.FILES\\CHECKUPLOADFILE_TASK4.csv", 
            sep = ',', row.names = F)


