

########################################
### script containing all functions ####
########################################

#libraries
library("readxl")
library("dplyr")
library("MASS")
library("gdata")

library(nFactors) 
library(psych)
library(candisc)
library(corrplot)
library(fastDummies)
library(matrixcalc)
library(ICSNP)
library(GPArotation)


#No scientific notation
options(scipen = 999)


### read in individual dataset
#ID = 'q1189206'; type = 'hospital'
getDATA <- function(ID, type){
  
  WD <- paste0("C:\\Users\\",personID,"\\OneDrive - KU Leuven\\ATSTAT-TASKS\\TASK4\\2.INDIVIDUAL\\")
  
  filename <- paste(WD, "1.DATA\\",ID, "_data", type, ".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}


### function to get group label
getLABEL <- function(ID, user_info){
  
  label <- user_info$`group`[user_info$`Username` == as.character(ID)]
  
  return(label)
}

### solution function
getSOL_story1 <- function(data, questions){
  
  data$Academic <- as.factor(data$Academic)
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             exdata = data[,-c(1,6)] 
                             s=cor(exdata)
                             ev=eigen(s)                
                             ev$values[2] 
                             
                           },
                           
                           #solution Q2
                           {
                             exdata = data[,-c(1,5)] 
                             s=cov(exdata)
                             ev=eigen(s)                
                             ev$values[1]
                           },
                           
                           #solution Q3
                           {
                             exdata = data[,-c(1,3)] 
                             s=cor(exdata)
                             ev=eigen(s)                
                             ev$values[3]
                           },
                           
                           
                           #solution Q4
                           {
                             exdata = data[,2:7]
                             pcatoy = prcomp(exdata, scale = TRUE)
                             pcatoy$x[ 10, 1]
                             
                           },
                           
                           #solution Q5
                           {
                             exdata = data[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 20, 2]
                             
                           },
                           
                           #solution Q6
                           {
                             exdata = data[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 30, 3]
                           },
                           
                           
                           #solution Q7
                           {
                             fitl <- lda(Academic ~ stay + quality , data, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q8
                           {
                             fitl <- lda(Academic ~ doctors + nurses , data, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q9
                           {
                             fitlcvp <- lda(Academic ~ stay + quality, data, prior = c(0.9,0.1), CV=TRUE)
                             table(data$Academic, fitlcvp$class)[2,1]
                           },
                           
                           #solution Q10
                           {
                             fitlcvp <- lda(Academic ~ stay + quality ,data, prior = c(0.8,0.2), CV=TRUE)
                             table(data$Academic,fitlcvp$class)[2,1]
                           }, 
                           
                           #solution Q11
                           {
                             fitlcvp <- lda(Academic ~ stay + quality ,data, prior = c(0.9,0.1), CV=TRUE)
                             table(data$Academic,fitlcvp$class)[1,2]
                           }, 
                           
                           #solution Q12
                           {
                             fitlcvp<- lda(Academic ~ stay + quality ,data, prior = c(0.8,0.2), CV=TRUE)
                             table(data$Academic,fitlcvp$class)[1,2]
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}


getSOL_story2 <- function(data, questions){
  
  data$RETURNED <- as.factor(data$RETURNED)
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             exdata = data[ ,2:5] 
                             s=cor(exdata)
                             ev=eigen(s)                
                             ev$values[1] 
                             
                           },
                           
                           #solution Q2
                           {
                             exdata = data[ ,2:5] 
                             s=cor(exdata)
                             ev=eigen(s)                
                             ev$values[2]
                           },
                           
                           #solution Q3
                           {
                             exdata = data[ ,2:5] 
                             s=cor(exdata)
                             ev=eigen(s)                
                             ev$values[3]
                           },
                           
                           
                           #solution Q4
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[10, 1]
                             
                           },
                           
                           #solution Q5
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[20, 2]
                             
                           },
                           
                           #solution Q6
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[30, 3]
                           },
                           
                           
                           #solution Q7
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, data, CV=TRUE)
                             fitl$posterior[2,1]
                             
                           },
                           
                           #solution Q8
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, data, CV=TRUE)
                             fitl$posterior[3,2]
                             
                           },
                           
                           #solution Q9
                           {
                             
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[2, 2] / table(data$RETURNED)[2])
                             
                           },
                           
                           #solution Q10
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[1, 1] / table(data$RETURNED)[1])
                           }, 
                           
                           #solution Q11
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[1, 2] /  table(data$RETURNED)[1])
                           }, 
                           
                           #solution Q12
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[2, 1] / table(data$RETURNED)[2])
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}

getSOL_story3 <- function(data, questions){
  
  data1 <- data[[1]]
  data1$Industry <- as.factor(data1$Industry)
  solutions <- numeric(length = length(questions))
  
  data2 <- data[[2]]
  #data2 = scale(data2)  #standardize the numerical variables
  distdata2 <- dist(data2)
  
  data3 <- data[[3]]
  
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           
                           #solution Q1
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale = TRUE)
                             pcatoy$x[ 10, 1]
                             
                           },
                           
                           #solution Q2
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 20, 2]
                             
                           },
                           
                           #solution Q3
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 30, 3]
                           },
                           
                           
                           #solution Q4
                           {
                             fitl <- lda(Industry ~ experience + ranking , data1, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q5
                           {
                             fitl <- lda(Industry ~ phds + masters , data1, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q6
                           {
                             fitlcvp <- lda(Industry ~ experience + ranking, data1, prior = c(0.9,0.1), CV=TRUE)
                             table(data1$Industry, fitlcvp$class)[2,1]
                           },
                           
                           #solution Q7
                           {
                             fitlcvp <- lda(Industry ~ experience + ranking ,data1, prior = c(0.8,0.2), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[2,1]
                           }, 
                           
                           #solution Q8
                           {
                             fitlcvp <- lda(Industry ~ phds + masters ,data1, prior = c(0.9,0.1), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[1,2]
                           }, 
                           
                           #solution Q9
                           {
                             fitlcvp<- lda(Industry ~ phds + masters ,data1, prior = c(0.8,0.2), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[1,2]
                           },
                           
                           #solution Q10
                           {
                             
                             clus.single<-hclust(distdata2, method = "single") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q11
                           {
                             
                             clus.single<-hclust(distdata2, method = "centroid") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q12
                           {
                             
                             clus.single<-hclust(distdata2, method = "ward.D2") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q13
                           {
                             fapavarmax <- fa(data3, nfactors = 4, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
                             fapavarmax$loadings[21,'PA2']
                            
                             
                           },
                           
                           #solution Q14
                           {
                             fapavarmax <- fa(data3, nfactors = 4, fm="ml", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
                             fapavarmax$loadings[9,'ML3']
                             
                           },
                           
                           #solution Q15
                           {
                             faMLquart <- fa(data3, nfactors = 4, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "quartimax") 
                             faMLquart$loadings[15,'PA4']
                             
                           },
                           
                           #solution Q16
                           {
                             faMLquart <- fa(data3, nfactors = 4, fm="ml", SMC=TRUE, residuals = TRUE, rotate = "quartimax") 
                             faMLquart$loadings[3,'ML1']
                             
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}

getSOL_story3_orderedEFAvariances <- function(data, questions){
  
  data1 <- data[[1]]
  data1$Industry <- as.factor(data1$Industry)
  solutions <- numeric(length = length(questions))
  
  data2 <- data[[2]]
  #data2 = scale(data2)  #standardize the numerical variables
  distdata2 <- dist(data2)
  
  data3 <- data[[3]]
  
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           
                           #solution Q1
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale = TRUE)
                             pcatoy$x[ 10, 1]
                             
                           },
                           
                           #solution Q2
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 20, 2]
                             
                           },
                           
                           #solution Q3
                           {
                             exdata = data1[,2:7]
                             pcatoy = prcomp(exdata, scale=TRUE)
                             pcatoy$x[ 30, 3]
                           },
                           
                           
                           #solution Q4
                           {
                             fitl <- lda(Industry ~ experience + ranking , data1, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q5
                           {
                             fitl <- lda(Industry ~ phds + masters , data1, prior = c(0.5, 0.5))
                             fitl$scaling[2]
                           },
                           
                           #solution Q6
                           {
                             fitlcvp <- lda(Industry ~ experience + ranking, data1, prior = c(0.9,0.1), CV=TRUE)
                             table(data1$Industry, fitlcvp$class)[2,1]
                           },
                           
                           #solution Q7
                           {
                             fitlcvp <- lda(Industry ~ experience + ranking ,data1, prior = c(0.8,0.2), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[2,1]
                           }, 
                           
                           #solution Q8
                           {
                             fitlcvp <- lda(Industry ~ phds + masters ,data1, prior = c(0.9,0.1), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[1,2]
                           }, 
                           
                           #solution Q9
                           {
                             fitlcvp<- lda(Industry ~ phds + masters ,data1, prior = c(0.8,0.2), CV=TRUE)
                             table(data1$Industry,fitlcvp$class)[1,2]
                           },
                           
                           #solution Q10
                           {
                             
                             clus.single<-hclust(distdata2, method = "single") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q11
                           {
                             
                             clus.single<-hclust(distdata2, method = "centroid") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q12
                           {
                             
                             clus.single<-hclust(distdata2, method = "ward.D2") 
                             clusts <- cutree(clus.single, k=5) # cut tree into 5 clusters
                             table(clusts) |> data.frame() |> arrange(-Freq) |> slice(1) |> pull(Freq)
                             
                           },
                           
                           #solution Q13
                           {
                             fapavarmax <- fa(data3, nfactors = 4, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
                             fapavarmax$loadings[21, 2]
                             
                             
                           },
                           
                           #solution Q14
                           {
                             fapavarmax <- fa(data3, nfactors = 4, fm="ml", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
                             fapavarmax$loadings[9, 3]
                             
                           },
                           
                           #solution Q15
                           {
                             faMLquart <- fa(data3, nfactors = 4, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "quartimax") 
                             faMLquart$loadings[15, 4]
                             
                           },
                           
                           #solution Q16
                           {
                             faMLquart <- fa(data3, nfactors = 4, fm="ml", SMC=TRUE, residuals = TRUE, rotate = "quartimax") 
                             faMLquart$loadings[3, 1]
                             
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}


getSOL_story_noSTD <- function(data, questions){
  
  data$RETURNED <- as.factor(data$RETURNED)
  solutions <- numeric(length = length(questions)) 
  
  #for each question
  for(i in 1:length(questions)){
    
    
    Q <- questions[i]
    
    solutions[i] <- switch(Q,  
                           #solution Q1
                           {
                             exdata = data[ ,2:5] 
                             s=cov(exdata)
                             ev=eigen(s)                
                             ev$values[1] 
                             
                           },
                           
                           #solution Q2
                           {
                             exdata = data[ ,2:5] 
                             s=cov(exdata)
                             ev=eigen(s)                
                             ev$values[2]
                           },
                           
                           #solution Q3
                           {
                             exdata = data[ ,2:5] 
                             s=cov(exdata)
                             ev=eigen(s)                
                             ev$values[3]
                           },
                           
                           
                           #solution Q4
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=FALSE)
                             pcatoy$x[10, 1]
                             
                           },
                           
                           #solution Q5
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=FALSE)
                             pcatoy$x[20, 2]
                             
                           },
                           
                           #solution Q6
                           {
                             exdata = data[ ,2:5]
                             pcatoy = prcomp(exdata, scale=FALSE)
                             pcatoy$x[30, 3]
                           },
                           
                           
                           #solution Q7
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, data, CV=TRUE)
                             fitl$posterior[2,1]
                             
                           },
                           
                           #solution Q8
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, data, CV=TRUE)
                             fitl$posterior[3,2]
                             
                           },
                           
                           #solution Q9
                           {
                             
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[2, 2] / table(data$RETURNED)[2])
                             
                           },
                           
                           #solution Q10
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[1, 1] / table(data$RETURNED)[1])
                           }, 
                           
                           #solution Q11
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[1, 2] /  table(data$RETURNED)[1])
                           }, 
                           
                           #solution Q12
                           {
                             fitl <- lda(RETURNED ~ weight + volume + price + rating, prior = c(0.5,0.5), data, CV=FALSE)
                             pp <- predict(fitl, data)
                             as.numeric(table(data$RETURNED, pp$class)[2, 1] / table(data$RETURNED)[2])
                           }
                           
    )
  }
  
  return(round(solutions, digits = 3))
  
}


### function to compare answers with rounding 
comp <- function(answer, solution){
  
  grade <- ifelse(isTRUE(all.equal(abs(answer), abs(solution), tolerance = 0.0011, scale = 1)), 1, 0)
  
  return(grade)
}

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

comp2 <- function(answer, solution){
  
  grade <- ifelse(isTRUE(all.equal(round2(abs(answer),2), round2(abs(solution), 2))), 1, 0)
  
  return(grade)
}

### feedback function 
# data frame to store all scores 
all_scores <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(all_scores) <- c("Student ID", "Vraag 1", "Vraag 2", "Vraag 3", "Vraag 4", "Total score")

feedBACK <- function(ID, i, ID_resp, ID_sol, ID_sol4_2){  
  
  # get ID responses  
  
  feedback = vector(mode="character", length=length(ID_sol))
  
  #compare responses with correct results 
  for (q in 1:length(ID_sol)){
    if(q <4 ){
      feedback[q] <- comp(answer = ID_resp[q], solution = ID_sol[q]) 
    }
    
    # if(q == 4 ){
    #   feedback[q] <- comp2(answer = ID_resp[q], solution = ID_sol[q]) 
    # }
    
    if(q == 4){
      if(comp2(answer = ID_resp[q], solution = ID_sol[q]) == 1){
        feedback[q] <- 1
      }
      else if(comp2(answer = ID_resp[q], solution = ID_sol4_2[i]) == 1){
        feedback[q] <- 1
      }
      else{
        feedback[q] <- 0
      }
    }
  }
  
  #store feedback in table 
  
  Head <- paste("Student ID: ", as.character(ID), "\n",
                "Last Name:", user_info |> filter(User.Name == as.character(ID)) |> dplyr::select("Last.Name"), "\n", 
                "First Name:", user_info |> filter(User.Name == as.character(ID)) |> dplyr::select("First.Name"), "\n\n")
  
  feedbacktext = rbind(c("Question", "Your answer", "Correct answer", "Grade"),
                       c("1", ID_resp[1], round(ID_sol[1], digits = 3), feedback[1]),
                       c("2", ID_resp[2], round(ID_sol[2], digits = 3), feedback[2]),
                       c("3", ID_resp[3], round(ID_sol[3], digits = 3), feedback[3]),
                       c("4", ID_resp[4], round(ID_sol[4], digits = 3), feedback[4]),
                       c(""),
                       c(paste("Total score = ", sum(as.numeric(feedback)), "/", length(ID_sol))," ", " ", "")) |>
    as.data.frame()
  
  #write individual feedback file
  filenameW <- paste(user_info |> filter(`User.Name` == ID) |> pull(newid), "_feedback.txt", sep="")
  indfolder= paste0("W:\\TASK4\\3.FEEDBACK")
  filepathW <- paste0(indfolder,"\\",filenameW)
  
  filename <- paste(ID, "_feedback.txt", sep="")
  filepathB <- paste0("2.INDIVIDUAL\\3.FEEDBACK\\", filename)
  
  writeLines(Head, filepathW)
  write.fwf(feedbacktext, file=filepathW, width = 20, colnames = F, justify = 'left', append = T)
  
  writeLines(Head, filepathB)
  write.fwf(feedbacktext, file=filepathB, width = 20, colnames = F, justify = 'left', append = T)
  
  #return grades  
  return(feedback)
  
}


#### OVERALL FUNCTION 
gradingTOOL <- function(responses, solutions, ID_sol4_2){
  
  #respondents ID 
  group_IDS <- as.character(user_info$User.Name)
  group_IDS <- na.omit(group_IDS)
  
  NA_IDS <-  group_IDS[!group_IDS %in% responses$Username]
  
  #dataframe to store all info
  all_info <- as.data.frame(matrix(data = NA, ncol = 18))
  
  #for each respondent 
  for (i in 1:length(group_IDS)){
    
    ID <- group_IDS[i]
    
    #####
    ## Get respondent answers if submitted 
    #####
    if(ID %in% responses$Username){
      ID_resp <- as.numeric(as.character(responses$Answer[responses$Username == ID]))
    } else {
      ID_resp <- rep(NA, 4)
    }
    
    #####
    ## Get respondent solutions and questions
    #####
    
    ID_sol <- solutions |> filter(User.Name == as.character(ID)) |> 
      dplyr::select(-(contains(c("Name", "Q")))) |> 
      mutate(across(where(is.double), \(x) round(x, 3))) |> 
      unlist() |> as.vector() 
    
    ID_quest <- solutions |> filter(User.Name == as.character(ID)) |> 
      dplyr::select(-(contains(c("Name", "S")))) |> unlist() |> as.vector()
    
    #####
    ## Write feedback reports and store overall grades
    #####
    
    feedback <- feedBACK(ID=ID, i=i, ID_resp = ID_resp, ID_sol = ID_sol, ID_sol4_2 = ID_sol4_2)
    all_info[i, ] <- c(ID, ID_quest[1], ID_resp[1], ID_sol[1], feedback[1],
                       ID_quest[2], ID_resp[2], ID_sol[2], feedback[2],
                       ID_quest[3], ID_resp[3], ID_sol[3], feedback[3], 
                       ID_quest[4], ID_resp[4], ID_sol[4], feedback[4], 
                       sum(as.numeric(feedback))) 
  } 
  colnames(all_info) <- c("ID", "Q1", "R1", "S1", "G1", "Q2", "R2", "S2", "G2", 
                          "Q3", "R3", "S3", "G3", "Q4", "R4", "S4", "G4","TOTAL")
  #not participated
  not_PP <- group_IDS %in% NA_IDS
  all_info$PP <- !not_PP
  
  return(all_info)
  
}

