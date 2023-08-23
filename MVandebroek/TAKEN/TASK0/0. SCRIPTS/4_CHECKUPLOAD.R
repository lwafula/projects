


# UPLOAD file 

rm(list = ls())

#Read in the Q-numbers 
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-08-23-13-59-08.csv",
                         check.names = F)

group_info = read_xlsx("1. FILES\\user_info with coding.xlsx") |>
  select(Username, `Group Code`, newid)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  indfolder= paste0("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\")
  datapath = paste0(indfolder,"1. DATA\\data",dfUPLOAD[i,"newid"],".txt")
  quizpath = paste0(indfolder,"2. QUESTIONS\\questions",dfUPLOAD[i,"newid"],".txt")
  
  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0("DATA: ", datapath, " VRAGEN: ", quizpath))
    
  } else{
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0("DATA: ", datapath, " QUESTIONS: ", quizpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK 0: data & quiz links [Total Pts: 1 Score] |114592` = 1) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

# write_xlsx(dfUPLOAD, path = "1. FILES\\CHECKUPLOADFILE.xlsx")
write.table(dfUPLOAD, file = "1. FILES\\CHECKUPLOADFILE.csv", 
            sep = ',', row.names = F)



# Feedback files ----------------------------------------------------------

#Read in the Q-numbers 
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-08-23-14-59-37.csv",
                         check.names = F)

dfUPLOAD = merge(olduser_info, group_info, by = 'Username')

group_IDS <- as.character(dfUPLOAD$Username)
group_IDS <- na.omit(group_IDS)

for (i in 1:length(group_IDS)){
  
  indfolder= paste0("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\")
  feedpath = paste0(indfolder,"3. FEEDBACK\\feedback",dfUPLOAD[i,"newid"],".txt")

  LABEL = dfUPLOAD[i, "Group Code"] |> toupper() |> as.character()
  
  if (LABEL == "TSTAT") {
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0("FEEDBACK: ", feedpath))
    
  } else{
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0("FEEDBACK: ", feedpath))
  }
  
}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid)) |>
  mutate(`Notes Format` = 'HTML', `Feedback Format` = 'HTML', 
         `TASK0-116082023: feedback [Total Pts: 1 Score] |114607` = 1) |>
  select("Last Name", "First Name", "Username", `Student ID`:`Feedback Format` )

write.table(dfUPLOAD, file = "1. FILES\\FEEDBACKUPLOADFILE.csv", 
            sep = ',', row.names = F)
