#################################
#####  Correcting HI TAAK 1 #####
#################################

#delete environment 
rm(list=ls())

#library
library(readxl)
library(writexl)

#setwd
setwd('C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TAAK1')

## Source functions
source("0. SCRIPTS\\0_source_taak1.R")


## Read in individual responses
responses <- read_excel("2. GRADING\\responses_test_TAAK1.xlsx")
                        

#Change unanswered to NA 
responses$Answer[responses$Answer == "<Unanswered>"] <- NA
responses$Answer[responses$Answer == "/"] <- NA
responses$Answer <- gsub(",", ".", responses$Answer) #if used , in stead of .
#which(is.na(responses$Answer))
responses$Answer <- as.numeric(responses$Answer) #SHOULD NOT INDUCE ADDITIONAL NA's
#which(is.na(responses$Answer)) # if still NA's, see which responses caused it  

## Read in correct solutions
solutions <- read_xlsx("2. GRADING\\solutions_taak1.xlsx")

## read in user info
user_info <- read_excel("1. INPUT\\user_info with coding.xlsx")

### run evaluation 
all_scores <- gradingTOOL(responses = responses, solutions = solutions)

### lay out final file
user_info$ID = user_info$User.Name
user_info <- user_info[!duplicated(user_info[, c(2)]), ]
all_scores <- merge(all_scores, user_info, by = "ID")
all_scores <- all_scores[, c(19,20,22,23,25,2:18)]


#write overall grades to excel file
write_xlsx(all_scores, path = "2. GRADING\\overallgrades_taak1.xlsx")




