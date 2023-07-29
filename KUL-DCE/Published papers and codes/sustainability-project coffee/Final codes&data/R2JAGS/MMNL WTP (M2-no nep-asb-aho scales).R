
############                  06-08-2018 by LMaaya
############                   MMNL[M2-no nep-asb-aho scales] MODEL-WTP SPACE  #############################
###########                   DIC calculation ##############

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
    choicesN[i,s,1:J] ~ dmulti(pr[i,s,1:J], 1)  #  multinomial likelihood for simulated choices
    choices[i,s,1:J] ~ dmulti(pr[i,s,1:J], 1)  #  multinomial likelihood
    # Lkd[i]<-exp(sum(LL[i,])) # log-likelihood
    
    # utilities
    log(u[i,s,1]) <- asc0est # opt-out
    log(u[i,s,2]) = price[i]*(ftbl[i,1]*faitradematrix[i,s,2] + ftbl[i,2]*biolabmatrix[i,s,2] + brazilorig*obrazil[i,s,2] +
    indonesiaorig*oindonesia[i,s,2] + netherlandsprod*pnetherlands[i,s,2] + italyprod*pitaly[i,s,2] + (-cpricematrix[i,s,2]) + 
    taste[i,1]*tdessert[i,s,2] + taste[i,2]*tmokka[i,s,2]) 
    
    log(u[i,s,3]) = price[i]*(ftbl[i,1]*faitradematrix[i,s,3] + ftbl[i,2]*biolabmatrix[i,s,3] + brazilorig*obrazil[i,s,3] +
    indonesiaorig*oindonesia[i,s,3] + netherlandsprod*pnetherlands[i,s,3] + italyprod*pitaly[i,s,3] + (-cpricematrix[i,s,3]) + 
    taste[i,1]*tdessert[i,s,3] + taste[i,2]*tmokka[i,s,3]) 
    
    log(u[i,s,4]) = price[i]*(ftbl[i,1]*faitradematrix[i,s,4] + ftbl[i,2]*biolabmatrix[i,s,4] + brazilorig*obrazil[i,s,4] +
    indonesiaorig*oindonesia[i,s,4] + netherlandsprod*pnetherlands[i,s,4] + italyprod*pitaly[i,s,4] + (-cpricematrix[i,s,4]) + 
    taste[i,1]*tdessert[i,s,4] + taste[i,2]*tmokka[i,s,4]) 
    
    log(u[i,s,5]) = price[i]*(ftbl[i,1]*faitradematrix[i,s,5] + ftbl[i,2]*biolabmatrix[i,s,5] + brazilorig*obrazil[i,s,5] +
    indonesiaorig*oindonesia[i,s,5] + netherlandsprod*pnetherlands[i,s,5] + italyprod*pitaly[i,s,5] + (-cpricematrix[i,s,5]) + 
    taste[i,1]*tdessert[i,s,5] + taste[i,2]*tmokka[i,s,5])
    
    # denominator for the probabilities
    den[i,s] = u[i,s,1] + u[i,s,2] + u[i,s,3] + u[i,s,4] + u[i,s,5]
    
    # probabilities
    pr[i,s,1]<- u[i,s,1]/den[i,s]
    pr[i,s,2]<- u[i,s,2]/den[i,s]
    pr[i,s,3]<- u[i,s,3]/den[i,s]
    pr[i,s,4]<- u[i,s,4]/den[i,s]
    pr[i,s,5]<- u[i,s,5]/den[i,s]
    
    } # end choice sets
    } # end respondents
    
    # priors for fixed effects
    asc0est ~ dnorm(0, 0.000001)
    brazilorig ~ dnorm(0, 0.000001)
    indonesiaorig ~ dnorm(0, 0.000001)
    netherlandsprod ~ dnorm(0, 0.000001)
    italyprod ~ dnorm(0, 0.000001)
    
    # define mean vectors for each of ftrade and blabel as depending on background variables and scales
    # priors for random effects
    
    for(i in 1:N){ #respondents
    
    # MVFtrade.Biolab
    ftbl[i,1:2] ~ dmnorm(mftbl[i,], tauftbl[,])
    mftbl[i,1] <- mft + bsex[1]*sex[i,1] + bage[1]*age[i,1] + bhnonuniv[1]*hnonuniv[i,1]+ 
    bhuniv[1]*huniv[i,1] 
    
    mftbl[i,2] <- mbl + bsex[2]*sex[i,1] + bage[2]*age[i,1] + bhnonuniv[2]*hnonuniv[i,1] + 
    bhuniv[2]*huniv[i,1] 
    
    # MVtaste
    taste[i,1:2] ~ dmnorm(mtaste[i,], taumtaste[,])
    mtaste[i,1] <- mdes
    mtaste[i,2] <- mmok
    
    # price
    price[i] ~ dlnorm(mprice, tauprice)
    }
    
    # Hyper-priors for random effects
    
    # ftrade and blabel: background and scale parameters
    mft ~ dnorm(0,0.000001)
    mbl ~ dnorm(0,0.000001)
    for(i in 1:2){
    bsex[i]~ dnorm(0, 0.000001)
    bage[i]~ dnorm(0, 0.000001)
    bhnonuniv[i]~ dnorm(0, 0.000001)
    bhuniv[i]~ dnorm(0, 0.000001)
    }
    
    # correlation matrix for Ftrade and Biolable
    tauftbl[1:2,1:2] ~ dwish(FB[,], 3)
    tauftblInv[1:2,1:2] <- inverse(tauftbl[,])
    
    sigma.ftrade <- sqrt(tauftblInv[1,1])
    sigma.biolable <- sqrt(tauftblInv[2,2])
    rho_ftrade_biolable <- tauftblInv[1,2]/sqrt(tauftblInv[1,1]*tauftblInv[2,2])
    
    # tastes
    mdes ~ dnorm(0,0.000001)
    mmok ~ dnorm(0,0.000001)
    
    # tastes correlation matrix
    taumtaste[1:2,1:2] ~ dwish(T[,], 3)
    taumtasteInv[1:2,1:2] <- inverse(taumtaste[,])
    sigma.des <- sqrt(taumtasteInv[1,1])
    sigma.mok <- sqrt(taumtasteInv[2,2])
    rho_des_mok <- taumtasteInv[1,2]/sqrt(taumtasteInv[1,1]*taumtasteInv[2,2])
    
    # price
    mprice ~ dnorm(0,0.000001)
    tauprice ~ dgamma(0.00001,0.00001) 
    
    # # ftrade background and scales coefficients
    ftrade_sex=bsex[1]
    ftrade_age=bage[1]
    ftrade_hnonuniv= bhnonuniv[1]
    ftrade_huniv= bhuniv[1]
    
    # # Biolable background and scales coefficients
    blab_sex=bsex[2]
    blab_age=bage[2]
    blab_hnonuniv= bhnonuniv[2]
    blab_huniv= bhuniv[2]
    }",
    file="C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MMNL2WTP-no nep-asb-aho scales.jags")

# defining the data set            

mmnldcof =    #             
  list(choices=dset, J=J,N=N,S= S,faitradematrix=faitradematrix, biolabmatrix=biolabmatrix,obrazil=obrazil, 
       oindonesia= oindonesia, pnetherlands= pnetherlands, pitaly=pitaly, tdessert=tdessert, tmokka =tmokka, 
       cpricematrix=cpricematrix,sex=sex, age=age, hnonuniv= hnonuniv,
       huniv= huniv,  T=structure(.Data = c(1,0,0, 1),.Dim=c(2,2)), FB=structure(.Data = c(1,0,0, 1),.Dim=c(2,2)))
# initial values
inits1 = list(asc0est= 1.34, brazilorig = 0.13, indonesiaorig = -0.13, netherlandsprod = -0.28, italyprod = -0.15, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2))) 
inits2 = list(asc0est= 0.5, brazilorig = 0.15,
              indonesiaorig = -0.15, netherlandsprod = -0.3, italyprod = -0.5, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)))
inits3 = list(asc0est = 2.5, brazilorig = 0, indonesiaorig = -0.0, netherlandsprod = -0.39, italyprod = -0.53, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)))

inits=list(inits1,inits2,inits3)

mmnl2par<-c('asc0est','brazilorig','indonesiaorig','netherlandsprod','italyprod','mbl','blab_sex','blab_age', 'blab_hnonuniv',
            'blab_huniv','mft','ftrade_sex','ftrade_age','ftrade_hnonuniv','ftrade_huniv','sigma.ftrade','sigma.biolable', 
            'rho_ftrade_biolable',  'mdes','mmok','sigma.des','sigma.mok','rho_des_mok','mprice','tauprice')

# running the model in JAGS
m2 <- R2jags::jags(data = mmnldcof, inits = inits, mmnl2par, n.chains = 3, n.iter = 100000, n.burnin = 50000, n.thin=1,
                   model.file = "C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MMNL2WTP-no nep-asb-aho scales.jags")

cbind(Est=round(m2$BUGSoutput$summary[,1],3),SD=round(m2$BUGSoutput$summary[,2],3), 
      sig=ifelse(abs(m2$BUGSoutput$summary[,1]/m2$BUGSoutput$summary[,2])>1.96,'*',''))
