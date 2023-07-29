


#### ESTIMATION OF DCE MODELS USING JAGS ####
#### 1.MNL WTP MODELS ####
rm(list = ls())
library(data.table)
library(R2jags)

setwd('C:/KU_Leuven/DCE/project_coffee')

# script with initial data management for WINBUGS
datamcoffeewinbugs <- parse("scripts/WINBUGS/models with all sets=6, different by blocks/winbugs_data_preparation code_set=6.R") 
eval(datamcoffeewinbugs) 


#### JAGS model formulation #######
cat("model{
    
    for(i in 1:N){ #respondents
    for(s in 1:S){ # choice sets
    
    choices[i,s,1:J] ~ dmulti(pr[i,s,1:J], 1)  #  multinomial likelihood
    choicesN[i,s,1:J]~ dmulti(pr[i,s,1:J], 1)
    # utilities
    log(u[i,s,1]) <- asc0est # opt-out
    log(u[i,s,2]) =  fairtrade*faitradematrix[i,s,2] + biolab*biolabmatrix[i,s,2] + brazilorig*obrazil[i,s,2] + 
    indonesiaorig*oindonesia[i,s,2] + netherlandsprod*pnetherlands[i,s,2] + italyprod*pitaly[i,s,2] + 
    price*cpricematrix[i,s,2] + tastedesert*tdessert[i,s,2] + tastemokka*tmokka[i,s,2] 
    
    log(u[i,s,3]) = fairtrade*faitradematrix[i,s,3] + biolab*biolabmatrix[i,s,3] + brazilorig*obrazil[i,s,3] + 
    indonesiaorig*oindonesia[i,s,3] + netherlandsprod*pnetherlands[i,s,3] + italyprod*pitaly[i,s,3] + 
    price*cpricematrix[i,s,3] + tastedesert*tdessert[i,s,3] + tastemokka*tmokka[i,s,3] 
    
    log(u[i,s,4]) = fairtrade*faitradematrix[i,s,4] + biolab*biolabmatrix[i,s,4] + brazilorig*obrazil[i,s,4] + 
    indonesiaorig*oindonesia[i,s,4] + netherlandsprod*pnetherlands[i,s,4] + italyprod*pitaly[i,s,4] + 
    price*cpricematrix[i,s,4] + tastedesert*tdessert[i,s,4] + tastemokka*tmokka[i,s,4] 
    
    
    log(u[i,s,5]) = fairtrade*faitradematrix[i,s,5] + biolab*biolabmatrix[i,s,5] + brazilorig*obrazil[i,s,5] + 
    indonesiaorig*oindonesia[i,s,5] + netherlandsprod*pnetherlands[i,s,5] + italyprod*pitaly[i,s,5] + 
    price*cpricematrix[i,s,5] + tastedesert*tdessert[i,s,5] + tastemokka*tmokka[i,s,5] 
    
    # denominator for the probabilities
    den[i,s] = u[i,s,1] + u[i,s,2] + u[i,s,3] + u[i,s,4] + u[i,s,5]
    
    # probabilities
    pr[i,s,1]<- u[i,s,1]/den[i,s]
    pr[i,s,2]<- u[i,s,2]/den[i,s]
    pr[i,s,3]<- u[i,s,3]/den[i,s]
    pr[i,s,4]<- u[i,s,4]/den[i,s]
    pr[i,s,5]<- u[i,s,5]/den[i,s]
    
    # likelihood
    for(j in 1:J){
    LL[i,s,j]<-choices[i,s,j]*log(pr[i,s,j])
    LLmeans[i,s,j]<-choicesN[i,s,j]*log(mean(pr[,s,j]))
    }
    } # end choice sets
    } # end respondents
    
    # deviance+DIC
    TL<-sum(LL[,,]); # total Log-likelihood
    D<- -2*sum(LL[,,]); # calculated deviance.
    Dbar<-0.5*pow(sd(LL[,,]),2)
    Dbarmeans<-0.5*pow(sd(LLmeans[,,]),2)
    Dmeans<- -2*sum(LLmeans[,,]); # calculated deviance.
    
    
    # The priors for the fixed effects
    asc0est ~ dnorm(0, 0.000001)
    fairtrade ~ dnorm(0, 0.000001)
    biolab ~ dnorm(0, 0.000001)
    brazilorig ~ dnorm(0, 0.000001)
    indonesiaorig ~ dnorm(0, 0.000001)
    netherlandsprod ~ dnorm(0, 0.000001)
    italyprod ~ dnorm(0, 0.000001)
    price ~ dnorm(0, 0.000001)
    tastedesert~dnorm(0,0.000001)
    tastemokka~dnorm(0,0.000001) 
    
      # WTP space
      asc0estwtp<- -asc0est/price
      fairtradewtp<- -fairtrade/price
      biolabwtp<--biolab/price
      brazilorigwtp<--brazilorig/price
      indonesiaorigwtp<--indonesiaorig/price
      netherlandsprodwtp<--netherlandsprod/price
      italyprodwtp<--italyprod/price
      tastedesertwtp<--tastedesert/price
      tastemokkawtp<--tastemokka/price

    }",
    file="C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MNLWTP.jags")

  # defining the data set
   dcof =    #             
   list(choices=dset, J=J,N=N,S= S,faitradematrix=faitradematrix, biolabmatrix=biolabmatrix,
       obrazil=obrazil, oindonesia= oindonesia, pnetherlands= pnetherlands, pitaly=pitaly, tdessert=tdessert,
       tmokka =tmokka, cpricematrix=cpricematrix)

    # initial values for 3 chains
    inits1 = list(asc0est = 1.27, fairtrade = 0.7, biolab = 0.7, brazilorig = 0.13, indonesiaorig = -0.13, netherlandsprod = -0.28, 
                  italyprod = -0.15, price=-0.33, tastedesert = 0.24, tastemokka = 0.27)
    inits2 = list(asc0est = 0.5, fairtrade = 0.8, biolab = 0.8, brazilorig = 0.15, indonesiaorig = -0.15, netherlandsprod = -0.3, 
                  italyprod = -0.5, price=-0.4, tastedesert = 0.2, tastemokka = 0.2)
    inits3 = list(asc0est = 2.5, fairtrade = 1, biolab = 1, brazilorig = 0, indonesiaorig = -0.0, netherlandsprod = -0.39, 
                  italyprod = -0.53, price=-0.54, tastedesert = 0.12, tastemokka = 0.12)
   mnlwtppar<-c('asc0estwtp','tastedesertwtp','tastemokkawtp','brazilorigwtp','indonesiaorigwtp','netherlandsprodwtp','italyprodwtp',
          'price','biolabwtp','fairtradewtp')

   # running the model in JAGS
   mnlwtp <- jags(data = dcof, inits = list(inits1, inits2, inits3), mnlwtppar,
            n.chains = 3, n.iter = 100000, n.burnin = 50000,n.thin=1, 
            model.file = "C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MNLWTP.jags")

cbind(round(mnlwtp$BUGSoutput$summary[,1],3),round(mnlwtp$BUGSoutput$summary[,2],3), 
      ifelse(abs(mnlwtp$BUGSoutput$summary[,1]/mnlwtp$BUGSoutput$summary[,2])>1.96,'*',''))
