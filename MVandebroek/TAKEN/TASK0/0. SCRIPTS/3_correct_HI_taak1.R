#################################
#####  Correcting HI TAAK 1 #####
#################################

#delete environment 
rm(list=ls())

#library
library(readxl)
library(writexl)

#setwd
# setwd('C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TAAK1')
setwd("C:\\Users\\u0118298\\OneDrive\\Projects\\MVandebroek\\TAKEN\\TASK0")


## Source functions
source("0. SCRIPTS\\0_source_taak1_LM.R")


## Read in individual responses
## Change unanswered to NA
responses <- read_excel("1. FILES\\responses_test_TAAK0.xlsx") |>
  mutate(across(contains("Ans"), ~if_else(. %in% c("<Unanswered>", "/"), NA_character_, .))) |>
  mutate(Answer = gsub(",", ".", Answer)) |> mutate(Answer = as.numeric(Answer))

## Read in correct solutions
solutions <- read_xlsx("1. FILES\\solutions_taak0.xlsx") |>
  rename("User.Name" = `ID`)

## read in user info
user_info <- read_excel("1. FILES\\user_info with coding.xlsx") |>
  rename("User.Name" = `Username`)

### run evaluation 
all_scores <- gradingTOOL(responses = responses, solutions = solutions)

### lay out final file
user_info$ID = user_info$User.Name
user_info <- user_info[!duplicated(user_info[, c(2)]), ]
all_scores <- merge(all_scores, user_info, by = "ID")
all_scores <- all_scores[, c(19,20,22,23,25,2:18)]


#write overall grades to excel file
# write_xlsx(all_scores, path = "2. INDIVIDUAL\\FEEDBACK\\overallgrades_all_taak0.xlsx")
write.table(all_scores, file = "2. INDIVIDUAL\\FEEDBACK\\overallgrades_all_taak0.txt", 
            quote = FALSE, row.names = FALSE)



