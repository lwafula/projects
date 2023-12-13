##########################################################
### Vragen toewijzing an antwoorden opslaan HIR TAAK 3 ###
##########################################################
rm(list=ls())

#library
library('readxl')
library('writexl')
library('stringi')

#setwd

personID = "u0118298"

setwd(paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK3"))


# Laad vragenpool (DEPENDS ON STORY) --------------------------------------


# Choose the story on which the test is based. Questions & solutions depend on the story

story = 3 

vragenpool_NED_ENG <- read_excel(path = paste0("1.FILES\\vragen_questions_taak3_story",story, ".xlsx"))


### laad studentennummers : Read in Q-numbers 
user_info <- read_excel(path = "1.FILES/user_info with coding_TASK3.xlsx")
user_info$group = user_info$"Group Code"
user_info <- user_info[!is.na(user_info$group),] 
any(is.na(user_info$group))


#source oplossingsfunctie 
source("0. SCRIPTS//0_source_taak3.R")

#settings
I <- nrow(user_info)
N <- 4 #number of questions  

#store solutions
solutions_IQ <- data.frame(matrix(nrow = I, ncol = (N*2+1)))
colnames(solutions_IQ) <- c("ID", paste0("S", 1:N), paste0("Q", 1:N))

#set.seed
set.seed(22233)

#for each student
for(i in 1:I){
  
  QI <- vragenpool_NED_ENG |> filter(BLOK %in% c(1, 2, 3, 4)) |> group_by(BLOK) |>
    slice_sample(n=1) |> ungroup() |> dplyr::select(`Vraag ID`) |> as.vector() |>
    unname() |> unlist() |> sort()
  
  #read in data
  
  ID <- user_info[i , "Username"]
  data1 <- getDATA(ID, type = "1")
  data2 <- getDATA(ID, type = "2")
  
  #get solutions 
  solutions_IQ[i, 1] <- as.character(ID) 
  solutions_IQ[i, 2:9] <- c(get(paste0("getSOL_story", story))(data = list(data1, data2), questions = QI), QI)
  
  #save questions dependent on language
  group <- getLABEL(ID, user_info)
  indfolder= paste0("W:\\TASK3\\2.QUESTIONS")
  
  if (group == "TSTAT"){
    
    questions_i <- vragenpool_NED_ENG$Vraag[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    
    filepathW <- paste0(indfolder,"\\vragen",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\vragen",user_info[i,"Username"],".txt")
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Reageer alleen met een numerieke waarde. Vermijd wetenschappelijke notatie. Als het resultaat groter is dan 1000, rondt u de waarde af op nul decimalen. Als het resultaat kleiner is dan 1000, geef het getal dan afgerond op drie decimalen en gebruik een punt (.) als decimaal scheidingsteken.")
    
    #Write First note  
    data1txt <- "De dataset (data1) bevat gegevens over een aantal kleine en middelgrote ondernemingen. De variabele YEARS geeft het aantal jaren sinds de start van het bedrijf tot een eventueel faillissement. Verder is er een variabele CHARACTER die 1 is als het bedrijf opereert in een zeer competitieve markt en die 2 is als het een bedrijf in een nichemarkt betreft. De FAILURE variabele is 1 (= het event) als de onderneming failliet ging en 0 als deze nog steeds bestaat. De variabele FAMILY is 1 als het een familiebedrijf betreft en 0 als dat niet het geval is en het aantal werknemers in het bedrijf wordt weergegeven in de variabele EMPLOYEES."
    
    quests_sme <- list()
    for(n in 1:2){
      quests_sme[[n]] <- paste("Vraag ",n,": ",questions_i[n], sep = "")
    }
    
    data2txt <- "De dataset (data2) bevat gegevens van personen die behandeld werden voor een depressie. Er werd hen gevraagd of de behandeling zeer nuttig was (Y=1), enigszins nuttig was (Y=2) of niet nuttig was (Y=3). De dataset bevat ook de volgende variabelen: AGE van de persoon, DOSE van een medicijn, GENDER (0 voor vrouwen, 1 voor mannen), en WEIGHT van de persoon."
    
    quests_counseling <- list()
    for(n in 3:4){
      quests_counseling[[n-2]] <- paste("Vraag ",n,": ",questions_i[n], sep = "")
    }
    
    
    #write 
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests_sme[[1]]), whiteline,
                        unlist(quests_sme[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests_counseling[[1]]), whiteline,
                        unlist(quests_counseling[[2]]), whiteline, line, line), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line,line, whiteline,
                        data1txt, line,
                        unlist(quests_sme[[1]]), whiteline,
                        unlist(quests_sme[[2]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data2txt, line, 
                        unlist(quests_counseling[[1]]), whiteline,
                        unlist(quests_counseling[[2]]), whiteline, line, line), fname = filepathB, encoding = "UTF-8")
    
  } else{
    
    questions_i <- vragenpool_NED_ENG$Question[vragenpool_NED_ENG$`Vraag ID` %in% QI]
    
    #Write file 
    filepathW <- paste0(indfolder,"\\questions",user_info[i,"newid"],".txt")  # write to the public folder
    filepathB <- paste0("2.INDIVIDUAL\\2.QUESTIONS\\questions",user_info[i,"Username"],".txt") 
    
    #Write First note
    
    whiteline <- c("                                                            ")
    line <- c("----------------------------------------------------------------")
    titel <- c("Only respond with a numeric value. Avoid scientific notation. If the result is larger than 1000, round the value to zero decimals. If the result is smaller than 1000, give the number rounded to three decimal places and use a point (.) as decimal separator.")
    
    data1txt <- "The dataset (data1) has data on the number of years (YEARS) until failure for various small and medium enterprises. Data of 2 market characteristics (CHARACTER 1 for a highly competitive and CHARACTER 2 for a niche market) are included. The FAILURE variable is 1 (the event) if the enterprise fails and 0 if it is still in business. Data is also available on whether the enterprise is family-owned (FAMILY = 1) or not (FAMILY = 0) and the number of employees in the firm (EMPLOYEES)."
    
    quests_sme <- list()
    for(n in 1:2){
      quests_sme[[n]] <- paste("Question ",n,": ",questions_i[n], sep = "")
    }
    
    data2txt <- "The dataset (data2) contains data on persons that were treated for depression. They were asked whether the treatment was highly useful (Y = 1), somewhat helpful (Y = 2) or not helpful (Y = 3). The dataset also contains the following variables: AGE of the person, DOSE of a drug, GENDER (0 for females and 1 for males), and WEIGHT of the person."
    
    quests_counseling <- list()
    for(n in 3:4){
      quests_counseling[[n-2]] <- paste("Question ",n,": ",questions_i[n], sep = "")
    }
    
    
    #write 
    
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests_sme[[1]]), whiteline,
                        unlist(quests_sme[[2]]), whiteline, line, line, whiteline, whiteline, 
                        whiteline, 
                        data2txt, line, 
                        unlist(quests_counseling[[1]]), whiteline,
                        unlist(quests_counseling[[2]]), whiteline, line, line), fname = filepathW, encoding = "UTF-8")
    
    stri_write_lines(c( titel, line, line, whiteline,
                        data1txt, line,
                        unlist(quests_sme[[1]]), whiteline,
                        unlist(quests_sme[[2]]), whiteline, line, line, whiteline, whiteline,
                        whiteline, 
                        data2txt, line, 
                        unlist(quests_counseling[[1]]), whiteline,
                        unlist(quests_counseling[[2]]), whiteline, line, line), fname = filepathB, encoding = "UTF-8")
    
    
  }
  
}

#save overall solutions 
write_xlsx(solutions_IQ, path = "1.FILES\\solutions_taak3.xlsx")
