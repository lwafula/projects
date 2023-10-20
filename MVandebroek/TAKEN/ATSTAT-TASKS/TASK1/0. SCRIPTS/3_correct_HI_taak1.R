#################################
#####  Correcting HI TAAK 1 #####
#################################

#delete environment 
rm(list=ls())

#library
library(readxl)
library(writexl)
library(tidyverse)

personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK1"))


## Source functions
source("0. SCRIPTS\\0_source_taak1_LM.R")


## Read in individual responses
## Change unanswered to NA
responses <- read_excel("1.FILES\\responses_test_TAAK1.xlsx") |>
  # mutate(across(contains("Ans"), ~if_else(. %in% c("<Unanswered>", "/"), NA_character_, .))) |>
  mutate(Answer = gsub(",", ".", Answer)) |> mutate(Answer = as.numeric(Answer)) |>
  filter(!(is.na(`Question ID`) | `Question` == 'Additional Content')) |>
  rename("User.Name" = `Username`)

## Read in correct solutions
solutions <- read_xlsx("1.FILES\\solutions_taak1.xlsx") |>
  rename("User.Name" = `ID`)

## read in user info
user_info <- read_excel("1.FILES\\user_info with coding_TASK1.xlsx") |>
  rename("User.Name" = `Username`)


### run evaluation 
all_scores <- gradingTOOL(responses = responses, solutions = solutions)

### lay out final file
user_info$ID = user_info$User.Name
# user_info <- user_info[!duplicated(user_info[, c(2)]), ]
all_scores <- merge(all_scores, user_info, by = "ID")

cols2numeric = c(paste0("R", 1:4), paste0("G", 1:4), paste0("S", 1:4), paste0("Q", 1:4), "TOTAL")
colsresp = paste0("R", 1:4)

all_scores <- all_scores[, c(19,20,21, 30, 22,23,25,2:18)] |> 
  mutate(across(all_of(cols2numeric), ~as.numeric(.))) |> rowwise() |> 
  mutate("Participated" = ifelse(all(is.na(across(all_of(colsresp)))), "No",
                                 ifelse(!all(is.na(across(all_of(colsresp)))) & TOTAL==0, "Yes, Grade 0", "Yes")))


#write overall grades to excel file
write_xlsx(all_scores, path = "1.FILES\\overallgrades_all_taak1.xlsx")
write.table(all_scores, file = "1.FILES\\overallgrades_all_taak1.txt", 
            quote = FALSE, row.names = FALSE)




