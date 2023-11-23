##########################################################
### Vragen toewijzing an antwoorden opslaan HIR TAAK 2 ###
##########################################################
rm(list=ls())

#library
library('readxl')
library('writexl')
library('stringi')
library(data.table)
library(tidyverse)

#setwd
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK2"))


# Laad vragenpool (DEPENDS ON STORY) --------------------------------------


# Choose the story on which the test is based. Questions & solutions depend on the story

story = 3 

vragenpool_NED_ENG <- read_excel(path = paste0("1.FILES\\vragen_questions_HI_taak2_story",story, ".xlsx"))


### laad studentennummers : Read in Q-numbers 
user_info <- read_excel(path = "1.FILES/user_info with coding_TASK2.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 
any(is.na(user_info$group))

#source oplossingsfunctie 
source("0. SCRIPTS//0_source_taak2.R")

#settings
I <- nrow(user_info)
N <- 4 #number of questions  

#store solutions

solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

#set.seed
set.seed(22232)

for(i in 1:I){
  
  # randomly select a question from each of blocks 1, 2, 4, 5
  
  QI <- vragenpool_NED_ENG |> filter(BLOK %in% c(1, 2, 4, 5)) |> group_by(BLOK) |>
    slice_sample(n=1) |> ungroup() |> dplyr::select(`Vraag ID`) |> as.vector() |>
    unname() |> unlist() |> sort()
  QI
  
  #read in data
  
  ID <- user_info[i , "Username"]
  data1 <- getDATA(ID, type = "1")
  data2 <- getDATA(ID, type = "2")
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  
  # SOLUTIONS (!!!DEPENDS ON STORY!!!)
  solutions_IQ[i, 2:9] <- c(get(paste0("getSOL_story", story))(data = list(data1, data2), questions = QI), QI)
  
  # save questions dependent on language
  group <- getLABEL(ID, user_info)
  indfolder= paste0("W:\\TASK2\\2.QUESTIONS")
  
  if (group == "TSTAT"){
    
    questions_i <- vragenpool_NED_ENG$Vraag[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    
    filepathW <- paste0(indfolder,"\\vragen",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"Username"],".txt") 
    
    quests <- list()
    for(n in 1:4){
      quests[[n]] <- paste("Vraag ",n,": ",questions_i[n], sep = "")
    }
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Reageer alleen met een numerieke waarde. Vermijd wetenschappelijke notatie. Als het resultaat groter is dan 1000, rondt u de waarde af op nul decimalen. Als het resultaat kleiner is dan 1000, geef het getal dan afgerond op drie decimalen en gebruik een punt (.) als decimaal scheidingsteken.")
    
    # story 1
    if (story == 1){
      data1txt <- c("Dataset1 bevat van een aantal gemeenten de jaarlijkse uitgaven (EXPEND), de inkomsten (REVENUE) en de subsidies (GRANTS) van de overheid.")
      data2txt <- c("Dataset2 bevat een tijdreeks met de inflatiegegevens (Inflation) van een bepaald land.")
      
      if (story == 2){
        # story 2
        data1txt <- c("Dataset1 bevat fictieve data waarbij voor een aantal landen de jaarlijkse C02 uitstoot werd berekend, het aantal koeien per inwoner (COWS), en het aantal autos per inwoner (CARS).")
        data2txt <- ("Dataset2 bevat een fictieve tijdreeks met de gemiddelde jaarlijkse temperatuur van een bepaald land.")
      }
    } else{
      # story 3
      data1txt <- c("Dataset1 bevat informatie over de jaaromzet (REVENUE) van verschillende bedrijven gedurende een aantal jaren, evenals hun uitgaven aan Research en Development (RnDEXPEND) en de leeftijd van het bedrijf (COMPANYAGE).")
      data2txt <- c("Dataset2 bevat een tijdreeks met de inflatiegegevens (Inflation) van een bepaald land.")
      
    }
    
    #write 
    stri_write_lines(c( titel, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), 
                        whiteline,
                        unlist(quests[[2]]), whiteline, whiteline,
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline,
                        unlist(quests[[4]]), whiteline), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, whiteline,
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline,
                        unlist(quests[[4]]), whiteline), fname = filepathB, encoding = "UTF-8")
    
  } else{
    
    questions_i <- vragenpool_NED_ENG$Question[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    
    filepathW <- paste0(indfolder,"\\questions",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"Username"],".txt") 
    
    quests <- list()
    for(n in 1:4){
      quests[[n]] <- paste("Question ",n,": ",questions_i[n], sep = "")
    }
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Only respond with a numeric value. Avoid scientific notation. If the result is larger than 1000, round the value to zero decimals. If the result is smaller than 1000, give the number rounded to three decimal places and use a point (.) as decimal separator.")
    
    # story 1
    if (story == 1){
      data1txt <- c("Dataset1 has information on the yearly expenditures (EXPEND) of several cities for a number of years as well as the related revenues (REVENUE) and grants (GRANTS) obtained.")
      data2txt <- c("Dataset2 has the inflation in a specific country over a number of years.")
      
      if (story == 2){
        # story 2
        data1txt <- c("Dataset1 has fictional information on the emission of CO2 of several countries for a number of years, as well as the number of COWS per capita, and the number of CARS per capita.")
        data2txt <- c("Dataset2 contains fictional data on the average temperature in a specific country over a number of years.")
        
      }
    } else{
      # story 3
      data1txt <- c("Dataset1 has information on the annual revenues (REVENUE) of several companies for a number of years, as well as their expenditures on Research and Development (RnDEXPEND), and the company age (COMPANYAGE).")
      data2txt <- c("Dataset2 has the inflation in a specific country over a number of years.")
      
    }
    
    #write 
    stri_write_lines(c( titel, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, whiteline,
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline,
                        unlist(quests[[4]]), whiteline), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, whiteline,
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline,
                        unlist(quests[[4]]), whiteline), fname = filepathB, encoding = "UTF-8")
    
  }
  
}

#save overall solutions 
write_xlsx(solutions_IQ, path = "1.FILES\\solutions_taak2.xlsx")
