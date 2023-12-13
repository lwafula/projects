

# https://www.html.am/html-codes/links/
# https://community.rstudio.com/t/how-to-preserve-hyperlinks-in-r-htmltable-while-writing-to-a-csv-excel-file/37728/2

library("readxl")
library("dplyr")
library("gdata")
library("car")
library(tidyverse)

# UPLOAD file 

rm(list = ls())

# personID
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\Individualized TASKS"))

#Read in the Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_Dataandquestions_IndivTASKS.csv",
                         check.names = F)

group_info = read_xlsx("1.FILES\\user_info with coding_Indiv_TASKS.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "GRP1") {
    # create html links for toledo
    # the public link points to the W: drive
    
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/1.DATA/data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/2.QUESTIONS/vragen',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")

    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(dataquizfeedpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    dataquizfeedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/1.DATA/data',dfUPLOAD[i,'newid'],'.txt',">here for your data</a></p>",
                          "<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/2.QUESTIONS/questions',dfUPLOAD[i,'newid'],'.txt',">here for your questions</a></p>")
    
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

write.table(dfUPLOAD, file = "1.FILES\\CHECKUPLOADFILE_Dataandquestions_IndivTASKS.csv", 
            sep = ',', row.names = F)



# Feedbacks ---------------------------------------------------------------


#Read in the Q-numbers 
olduser_info <- read.csv("1.FILES\\olduser_info_Feedbacks_IndivTASKS.csv",
                         check.names = F)

dfUPLOAD2 = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD2$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  LABEL = dfUPLOAD2[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "GRP1") {
    # create html links for toledo
    # the public link points to the W: drive
    
    feedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/3.FEEDBACK/feedback',dfUPLOAD2[i,'newid'],'.txt',">here for your feedback</a></p>")
    
    dfUPLOAD2[i, ] = dfUPLOAD2 |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
    
  } else{
    
    # create html links for toledo
    # the public link points to the W: drive
    feedpath = paste0("<p>Click <a href=", 'https://feb.kuleuven.be/public/', personID,'/IndividualizedTASKS/3.FEEDBACK/feedback',dfUPLOAD2[i,'newid'],'.txt',">here for your feedback</a></p>")
    
    dfUPLOAD2[i, ] = dfUPLOAD2 |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0(feedpath))
  }
  
}

# to mutate a column with contains: e.g. TASK column changes with a new 
# https://www.statology.org/dplyr-mutate-if-contains/

dfUPLOAD2 = dfUPLOAD2 |> select(-c(`Group Code`, newid)) |>
  mutate(`Marking Notes` = as.character("Marking Notes"), 
         `Notes Format` = 'SMART_TEXT', `Feedback Format` = 'HTML') |> 
  mutate_at(vars(contains('TASK')), ~ (1)) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD2, file = "1.FILES\\CHECKUPLOADFILE_Feedbacks_IndivTASKS.csv", 
            sep = ',', row.names = F)
