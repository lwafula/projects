
# UPLOAD file 

rm(list = ls())

#Read in the Q-numbers 
olduser_info <- read.csv("1. FILES\\gc_ULTRA-C17747534-B-2324_columns_2023-08-14-15-34-59.csv",
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
      mutate(`Feedback to Learner` = paste0("KIJK NAAR: \n\n", "DATA: ", datapath, "\n", " VRAGEN: ", quizpath))
    
  } else{
    
    dfUPLOAD[i, ] = dfUPLOAD |> filter(Username == group_IDS[i]) |> 
      mutate(`Feedback to Learner` = paste0("LOOK AT: \n\n", "DATA: ", datapath, "\n", " QUESTIONS: ", quizpath))
  }

}

dfUPLOAD = dfUPLOAD |> select(-c(`Group Code`, newid))

write_xlsx(dfUPLOAD, path = "1. FILES\\CHECKUPLOADFILE.xlsx")
