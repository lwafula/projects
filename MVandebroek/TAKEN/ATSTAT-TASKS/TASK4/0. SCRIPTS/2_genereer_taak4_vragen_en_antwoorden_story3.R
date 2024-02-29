##########################################################
### Vragen toewijzing an antwoorden opslaan HIR TAAK 4 ###
##########################################################
rm(list=ls())

#library
library('readxl')
library('writexl')
library('stringi')
library(tidyverse)

#setwd
personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK4"))

### Laad vragenpool en student numbers 

story = 3 

vragenpool_NED_ENG <- read_excel(path = paste0("1.FILES\\vragen_questions_HI_taak4_story",story, ".xlsx"))

#Read in Q-numbers 
user_info <- read_excel(path = "1.FILES/user_info with coding_TASK4.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 
any(is.na(user_info$group))

#source oplossingsfunctie 
source("0. SCRIPTS\\0_source_taak4.R")

#settings
I <- nrow(user_info)
N <- 4 #number of questions  

#store solutions
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

#set.seed
set.seed(22234)

for(i in 1:I){
  
  #draw for each i 4 questions  
  QI <- vragenpool_NED_ENG |> filter(BLOK %in% c(1, 2, 3, 4)) |> group_by(BLOK) |>
    slice_sample(n=1) |> ungroup() |> dplyr::select(`Vraag ID`) |> as.vector() |>
    unname() |> unlist() |> sort()
  
  #read in data
  
  ID <- user_info[i , "Username"]
  
  #story3
  
  data1 <- getDATA(ID, type = "1")
  data2 <- getDATA(ID, type = "2")
  data3 <- getDATA(ID, type = "3")
  
  #get solutions
  solutions_IQ[i, 1] <- as.character(ID) 
  
  #story3
  solutions_IQ[i, 2:9] <- c(get(paste0("getSOL_story", story))(data = list(data1, data2, data3), questions = QI), QI)
  
  #save questions dependent on language
  group <- getLABEL(ID, user_info)
  indfolder= paste0("W:\\TASK4\\2.QUESTIONS")
  
  
  if (group == "TSTAT"){
    
    questions_i <- vragenpool_NED_ENG$Vraag[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    
    filepathW <- paste0(indfolder,"\\vragen",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"Username"],".txt")
    
    #STORY 3
    #Write First note  
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Reageer alleen met een numerieke waarde. Vermijd wetenschappelijke notatie. Als het resultaat groter is dan 1000, rondt u de waarde af op nul decimalen. Als het resultaat kleiner is dan 1000, geef het getal dan afgerond op drie decimalen en gebruik een punt (.) als decimaal scheidingsteken.")
    
    data1txt <- "De dataset (data1) bevat gegevens over de Faculteit Economie en Bedrijfskunde van enkele Europese universiteiten. Er is informatie over de gemiddelde ervaring van het onderwijzend personeel (experience), de gemiddelde leeftijd van het personeel (age), het aantal publicaties van de faculteit (publications), het aantal promovendi (phds), het aantal masterstudenten (masters) en een rangschikkingsmaatstaf (ranking, hoe hoger hoe beter). De variabele Industrie geeft aan of de faculteit een band heeft met een bedrijf uit de sector (Industry = yes), of niet (Industry = no)."
    data2txt <- "De dataset (data2) bevat gegevens over vijf verschillende kenmerken van deelnemers aan een onderzoek. Alle variabelen zijn gestandaardiseerd."
    data3txt <- "De dataset (data3) bevat 23 items over de percepties van studenten ten aanzien van het leren van een statistische eenheid aan de KU Leuven. Gebruik deze dataset om een verkennende factoranalyse uit te voeren."
    
    quests <- list()
    for(n in 1:4){
      quests[[n]] <- paste("Vraag ",n,": ",questions_i[n], sep = "")
    }
    
    #write 
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data3txt, line,
                        unlist(quests[[4]]), whiteline, line, line), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data3txt, line,
                        unlist(quests[[4]]), whiteline, line, line), fname = filepathB, encoding = "UTF-8")
    
    
  } else{
    
    questions_i <- vragenpool_NED_ENG$Question[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    
    filepathW <- paste0(indfolder,"\\questions",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"Username"],".txt")
    
    ### STORY 3 ###
    
    #Write First note
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Only respond with a numeric value. Avoid scientific notation. If the result is larger than 1000, round the value to zero decimals. If the result is smaller than 1000, give the number rounded to three decimal places and use a point (.) as decimal separator.")
    
    data1txt <- "The dataset (data1) contains data on the Faculty of Economics and Business in some European universities. There is information on the average experience of the teaching staff (experience), the average age of the staff (age), the number of publications from the faculty (publications), the number of PhD researchers (phds), the number of Masters students (masters), and a ranking measure (ranking, the higher the better). The variable Industry indicates whether the faculty has an affiliation to a company in the industry (Industry = yes), or not (Industry = no)."
    data2txt <- "The dataset (data2) contains data on five different attributes of participants in a study. All the variables have been standardized."
    data3txt <- "The dataset (data3) contains 23 items on students perceptions towards learning a statistical unit at KU Leuven. Use this to perform an exploratory factor analysis."
    
    
    quests <- list()
    for(n in 1:4){
      quests[[n]] <- paste("Question ",n,": ",questions_i[n], sep = "")
    }
    
    #write 
    
    
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data3txt, line,
                        unlist(quests[[4]]), whiteline, line, line), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests[[1]]), whiteline,
                        unlist(quests[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests[[3]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data3txt, line,
                        unlist(quests[[4]]), whiteline, line, line), fname = filepathB, encoding = "UTF-8")
    
  }
  
}


#save overall solutions 
write_xlsx(solutions_IQ, path = c("1.FILES\\solutions_taak4.xlsx"))




