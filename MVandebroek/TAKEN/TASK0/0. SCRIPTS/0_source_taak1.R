########################################
### script containing all functions ####
########################################

#libraries
library("readxl")
library("dplyr")
library("gdata")
library("car")

#No scientific notation
options(scipen = 999)
#print 10 digits (default = 7)
options(digits=10)

### check the type of the data files > this test with txt files !!!
### read in individual dataset
getDATA <- function(ID){

  # WD <- "C:\\Users\\u0004359\\OneDrive - KU Leuven\\Desktop\\TAKEN\\TAAK1\\3. DATA individual\\"
  WD <- "C:\\Users\\Public\\lmaaya\\Projects\\MVandebroek\\TAKEN\\TASK0\\2. INDIVIDUAL\\"
  filename <- paste(WD, "DATA\\data",ID,".txt", sep = "") 
  data <- read.table(file = filename, header = TRUE)
  
  return(data)
}


#setwd('C:\\Users\\u0105757\\Dropbox\\Toledo 2021-2022\\datafiles\\HI_taak1\\ind_datafiles')
#data <- getDATA(ID = "q0980093")

### solution function
getSOL <- function(data, questions){
  
  solutions <- numeric(length = length(questions))
  #for each question
  for(i in 1:length(questions)){
    
    Q <- questions[i]
   
    data$Department <- as.factor(data$Department)
    data$Gender <- as.factor(data$Gender)
    
    solutions[i] <- switch(Q,  
     
      #solution Q1
      {
        mod = lm(Salary~ Experience + Employed + Education, data=data)
        vif(mod)[3] 
      },
      
      #solution Q2
      {
        mod = lm(Salary~ Experience + Employed + Education, data=data)
        vif(mod)[2] 
      },
      
      #solution Q3
      {
        mod = lm(Salary~ Experience + Employed + Education, data=data)
        vif(mod)[1] 
      },
      
      #solution Q4
      {
        model = lm(Salary ~ Gender * Employed, data = data)
        preddata = data.frame(Gender = "F", Employed = 30)
        
        #prediction interval for individual salary
        result <- predict(model, newdata = preddata, interval="prediction") 
        result[3]
      },
      
      #solution Q5
      {
        model = lm(Salary ~ Gender * Employed, data = data)
        preddata = data.frame(Gender = "M", Employed = 30)
        
        #prediction interval for individual salary
        result <- predict(model, newdata = preddata, interval="prediction") 
        result[3]
      },
      
      #solution Q6
      {
        model = lm(Salary ~ Gender * Employed, data = data)
        preddata = data.frame(Gender = "F", Employed = 30)
        
        #prediction interval for individual salary
        result <- predict(model, newdata = preddata, interval="prediction") 
        result[2]
      },
      
      #solution Q7
      {
        outliermod <- lm(Salary~Employed, data=data)
        infl <- influence.measures(outliermod)
        infl$infmat[25, 5] * 1000
        
      },
      
      #solution Q8
      {
        outliermod <- lm(Salary~Experience, data=data)
        infl <- influence.measures(outliermod)
        infl$infmat[35, 5] * 1000
      },
      
      #solution Q9
      {
        outliermod <- lm(Salary~Education, data=data)
        infl <- influence.measures(outliermod)
        infl$infmat[15, 5] * 1000
      }, 
      
      #solution Q10
      {
        outliermod = lm(Salary~Employed, data=data)
        inflmeasures= influence.measures(outliermod)
        inflmeasures$infmat[20, 6]
      }, 
      
      #solution Q11
      {
        outliermod = lm(Salary~Experience, data=data)
        inflmeasures= influence.measures(outliermod)
        inflmeasures$infmat[12, 6]
      },
      
      #solution Q12
      {
        outliermod = lm(Salary~Education, data=data)
        inflmeasures= influence.measures(outliermod)
        inflmeasures$infmat[35, 6]
      },
      
      #solution Q13
      {
        salary_anova = aov(Salary~ Department, data = data)
        tukey = TukeyHSD(salary_anova)
        sum(tukey$'Department'[, 4] < 0.05)
      },
      
      #solution Q14
      {
        salary_anova = aov(Salary~ Department, data = data)
        tukey = TukeyHSD(salary_anova)
        tukey$'Department'[3, 4] 
      },
      
      #solution Q15
      {
        salary_anova = aov(Salary~ Department, data = data)
        tukey = TukeyHSD(salary_anova)
        tukey$'Department'[4, 4] 
      },
      
      #solution Q16
      {
        salary_anova = aov(Salary ~ Department, data = data)
        tukey = TukeyHSD(salary_anova)
        tukey$'Department'[5, 4] 
      },
      
      #solution Q17
      {
        groupmeans <- c(55,60,57,62)
        result <- power.anova.test(groups = length(groupmeans),  
                         between.var = var(groupmeans), within.var = 25, sig.level = 0.05, n = 10)
       result$power 
      },
      
      #solution Q18
      {
        groupmeans <- c(55,60,57,62)
        result <- power.anova.test(groups = length(groupmeans),  
                         between.var = var(groupmeans), within.var = 36, sig.level = 0.05, n = 20)
        result$power 
      },
      
      #solution Q19
      {
        groupmeans <- c(55,60,57,62)
        result <- power.anova.test(groups = length(groupmeans),  
                         between.var = var(groupmeans), within.var = 25, sig.level = 0.10, n = 10)
        result$power
      },
      
      #solution Q20
      {
        twoway = lm(Salary ~ Department * Gender, data = data)
        aov <- anova(twoway)
        aov$`Pr(>F)`[3]
      },
      
      #solution Q21
      {
        twoway = lm(Salary ~ Department * Gender, data = data)
        test <- summary(twoway)
        test$fstatistic[1]
      },
      
      #solution Q22
      {
        twoway = lm(Salary ~ Department * Gender, data = data)
        aov <- anova(twoway)
        aov$`Mean Sq`[4]
      }
    )
  }
    
 return(round(solutions, digits = 5))
  
}


### function to compare answers with rounding 
comp <- function(answer, solution){
  
  if (solution > 1000){ #output question 1 can only have 2 decimals 
    grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.011, scale = 1)), 1, 0)
  }
  
  if (solution < 1000){ #remaining questions 
    grade <- ifelse(isTRUE(all.equal(answer, solution, tolerance = 0.0011, scale = 1)), 1, 0)
  }
 
  
  return(grade)
}
# 
# 
# ### feedback function 
# # data frame to store all scores 
# all_scores <- data.frame(matrix(ncol = 6, nrow = 0))
# colnames(all_scores) <- c("User.Name", "Q1", "Q2", "Q3", "Q4", "Total score")
# 
# 

###-----------------------------------------------------
#### 
gradingTOOL <- function(responses, solutions){
  
  #respondents ID 
  group_IDS <- as.character(user_info$User.Name)
  group_IDS <- na.omit(group_IDS)
  
  NA_IDS <-  group_IDS[!group_IDS %in% responses$User.Name]
  
  #dataframe to store all info and feedback
  all_info <- as.data.frame(matrix(data = NA, ncol = 18))
  feedbackfile = data.frame(matrix(nrow = nrow(user_info), ncol = 2))
  names(feedbackfile) = c("User.Name","Feedback")
 
  
  #for each respondent 
  for (i in 1:length(group_IDS)){
    
    ID <- group_IDS[i]
    
    #####
    ## Get respondent answers if submitted 
    #####
    if(ID %in% responses$User.Name){
      ID_resp <- as.numeric(as.character(responses$Answer[responses$User.Name == ID]))
    } else {
      ID_resp <- rep(NA, 4)
    }
    
    #####
    ## Get respondent solutions and questions
    #####
    ID_sol <- c(solutions$S1[solutions$User.Name == as.character(ID)], solutions$S2[solutions$User.Name == as.character(ID)],
                solutions$S3[solutions$User.Name == as.character(ID)], solutions$S4[solutions$User.Name == as.character(ID)])
    ID_sol <- round(ID_sol, 3)
    ID_quest <- c(solutions$Q1[solutions$User.Name == as.character(ID)], solutions$Q2[solutions$User.Name == as.character(ID)],
                  solutions$Q3[solutions$User.Name == as.character(ID)], solutions$Q4[solutions$User.Name == as.character(ID)])
    
    #####
    ## Write feedback in file and store overall grades
    #####
    
 
      #compare responses with correct results
      feedback = vector(mode="character", length=length(ID_sol))
      
      for (q in 1:length(ID_sol)){
            feedback[q] <- comp(answer = ID_resp[q], solution = ID_sol[q])
      }

      #store feedback in table
      feedbacktext <- paste("Question 1: Your answer = ", ID_resp[1], " Correct answer = ",round(ID_sol[1], digits = 3), " Grade = ", feedback[1],"<br/>",
                            "Question 2: Your answer = ", ID_resp[2], " Correct answer = ",round(ID_sol[2], digits = 3), " Grade = ", feedback[2],"<br/>",
                            "Question 3: Your answer = ", ID_resp[3], " Correct answer = ",round(ID_sol[3], digits = 3), " Grade = ", feedback[3],"<br/>",
                            "Question 4: Your answer = ", ID_resp[4], " Correct answer = ",round(ID_sol[4], digits = 3), " Grade = ", feedback[4],"<br/>",
                            "Total score = ", sum(as.numeric(feedback)))

      #write individual feedback file
      feedbackfile[i,1] = ID
      feedbackfile[i,2] = feedbacktext

    
    
    all_info[i, ] <- c(ID, ID_quest[1], ID_resp[1], ID_sol[1],  feedback[1],
                           ID_quest[2], ID_resp[2], ID_sol[2],  feedback[2],
                           ID_quest[3], ID_resp[3], ID_sol[3],  feedback[3], 
                           ID_quest[4], ID_resp[4], ID_sol[4],  feedback[4], 
                       sum(as.numeric(feedback)))
  } 
  colnames(all_info) <- c("ID", "Q1", "R1", "S1", "G1"
                              , "Q2", "R2", "S2", "G2"
                              , "Q3", "R3", "S3", "G3"
                              , "Q4", "R4", "S4", "G4"
                              ,"TOTAL")
  
  #not participated
#  not_PP <- group_IDS %in% NA_IDS
#  all_info$PP <- !not_PP
  
  write_xlsx(feedbackfile, path = "2.INDIVIDUAL\\FEEDBACK\\feedbackfile_taak1.xlsx")
  
  
  
  return(all_info)
  
}


# 
# 
# feedBACK <- function(ID, i, ID_resp, correct_resp){
#   
#   # get ID responses  
#   feedback <- list()
#   
#   #compare responses with correct results 
#   for (q in 1:length(ID_sol)){
#     feedback[[q]] <- comp(answer = ID_resp[q], solution = ID_sol[q])
#   }
#   
#   #store feedback in table 
#   fmat <- cbind("Question" = c(1:length(ID_sol)),"Your answer" = ID_resp,
#                 "Correct answer" = round(ID_sol, digits = 3), "Grade" = unlist(feedback))
#   rownames(fmat) <- NULL
#   
#   
#   #write individual feedback file
#   filename <- paste("feedback_",ID, "_test_taak1.txt", sep="")
#   
#   file = paste("2. GRADING\\feedback",filename, sep="")
#   capture.output(print(fmat, print.gap=3, quote = FALSE, row.names=FALSE), 
#                  file= file)
#   
#   line= paste("Total score = ", sum(fmat[ , 4]),"/4", sep = "")
#   space = ""
#   write(space,file=file,append=TRUE)
#   write(line,file=file,append=TRUE)
#   
#   #return grades  
#   return(fmat[,4])
#   
# }
