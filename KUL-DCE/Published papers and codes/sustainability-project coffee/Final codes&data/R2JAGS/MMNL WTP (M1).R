


#### ESTIMATION OF DCE MODELS USING JAGS ####
#### 1. MMNL M1 MODELS ####
rm(list = ls())
library(data.table)
library(R2jags)

setwd('C:/KU_Leuven/DCE/project_coffee')

#### DATA ####

# long format, all alternatives
data_longformat = read.table('datasets/data_longformat.txt', header = TRUE, sep = '\t')

# for every set for every person, we expect one choice, keep relevant variables 
data_longformat = data_longformat[,c(1:5,16:24)]

#### recoding variables ####
## IDs to 1:262
ids = unique(data_longformat$personID)
n= length(unique(data_longformat$personID)) #262

##### long format data
data_longformat$personID = match(data_longformat$personID,table=ids)

##### recode sets in block 2 to be 1:6 instead of 7:12. 
data_longformat$setID<-ifelse(data_longformat$setID<=6,data_longformat$setID,data_longformat$setID-6)

# keep the 1st 20 IDs for testing purposes
data_longformat<-data_longformat[data_longformat$personID %in% c(1:262),]

#### JAGS CODES ####
J = length(unique(data_longformat$altID)) # number of alternatives
N = length(unique(data_longformat$personID)) # number of observations (respondents)
S= length(unique(data_longformat$setID)) # number of sets

#### data as a matrices for use in WINBUGS code
#### choices
dset = array(0, dim = c(length(unique(data_longformat$personID)),length(unique(data_longformat$setID)),
                        length(unique(data_longformat$altID))))

for(n in 1:max(data_longformat$personID)){# loop over respondents
  for(s in 1:length(unique(data_longformat$setID))){# loop over alternatives
    for(j in 1:length(unique(data_longformat$altID))){#loop over sets
      dset[n,s,j]<-
        as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$select)
    }
  }
}

# fairtrade
faitradematrix = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      faitradematrix[n,s,j]<- as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$fairtradelabel_yes)
    }
  }
}

#biolable
biolabmatrix = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      biolabmatrix[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$biolabel_yes)
    }
  }
}

#origin-brazil
obrazil = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      obrazil[n,s,j]<- as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$countryorigin_brazilia)
    }
  }
}

#origin-indonesia
oindonesia = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      oindonesia[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$countryorigin_indonesia)
    }
  }
}

# production-netherlands
pnetherlands = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      pnetherlands[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$countryproduction_netherlands)
    }
  }
}

# production-italy
pitaly = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      pitaly[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$countryproduction_italy)
    }
  }
}

# taste-dessert
tdessert = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      tdessert[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$taste_dessert)
    }
  }
}

# taste-mokka
tmokka = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      tmokka[n,s,j]<-as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$taste_mokka)
    }
  }
}

# $price
cpricematrix = array(0, dim = c(N,S,J))
for(n in 1:N){
  for(s in 1:S){
    for(j in 1:J){
      cpricematrix[n,s,j]<- as.numeric(data_longformat[data_longformat$personID==n & data_longformat$setID==s & data_longformat$altID==j,]$cprice)
    }
  }
}

#### JAGS model formulation #######
cat("model{
    
    for(i in 1:N){ #respondents
    for(s in 1:S){ # choice sets
    
    choices[i,s,1:J] ~ dmulti(pr[i,s,1:J], 1)  #  multinomial likelihood
    
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
    
    # likelihood
    for(j in 1:J){
    LL[i,s,j]<-choices[i,s,j]*log(pr[i,s,j])
    }
    } # end choice sets
    } # end respondents
    
    D<- -2*sum(LL[,,]); # calculated deviance.
    
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
    mftbl[i,1] <- mft
    mftbl[i,2] <- mbl
    
    # MVtaste
    taste[i,1:2] ~ dmnorm(mtaste[i,], taumtaste[,])
    mtaste[i,1] <- mdes
    mtaste[i,2] <- mmok
    
    # price
    price[i] ~ dlnorm(mprice, tauprice)
    }
    
    # Hyper-priors for random effects
    mft ~ dnorm(0,0.000001)
    mbl ~ dnorm(0,0.000001)
    
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
    
    }",
    file="C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MMNL1WTP.jags")

# defining the data set            
mmnldcof =    #             
  list(choices=dset, J=J,N=N,S= S,faitradematrix=faitradematrix, biolabmatrix=biolabmatrix,
       obrazil=obrazil, oindonesia= oindonesia, pnetherlands= pnetherlands, pitaly=pitaly, tdessert=tdessert,
       tmokka =tmokka, cpricematrix=cpricematrix,T=structure(.Data = c(1,0,0, 1),.Dim=c(2,2)),
       FB=structure(.Data = c(1,0,0, 1),.Dim=c(2,2)))
# initial values
inits1 = list(asc0est= 1.34, brazilorig = 0.13, indonesiaorig = -0.13, netherlandsprod = -0.28, italyprod = -0.15, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2))) 
inits2 = list(asc0est= 0.5, brazilorig = 0.15,
              indonesiaorig = -0.15, netherlandsprod = -0.3, italyprod = -0.5, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)))
inits3 = list(asc0est = 2.5, brazilorig = 0, indonesiaorig = -0.0, netherlandsprod = -0.39, italyprod = -0.53, 
              tauftbl=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)),taumtaste=structure(.Data = c(0.0001,0,0,0.0001),.Dim=c(2,2)))

m1parameters<-c('asc0est','mdes','mmok','sigma.des','sigma.mok','rho_des_mok','brazilorig','indonesiaorig',
                'netherlandsprod','italyprod','mprice','tauprice','mbl','sigma.biolable',
                'mft','sigma.ftrade', 'rho_ftrade_biolable')

# running the model in JAGS
m1 <- jags(data = mmnldcof, inits = list(inits1, inits2, inits3), m1parameters,
                  n.chains = 3, n.iter = 100000, n.burnin = 50000,n.thin = 1, 
           model.file = "C:/KU_Leuven/DCE/project_coffee/scripts/WINBUGS/R2JAGS/MMNL1WTP.jags")

cbind(round(m1$BUGSoutput$summary[,1],3),round(m1$BUGSoutput$summary[,2],3), 
      ifelse(abs(m1$BUGSoutput$summary[,1]/m1$BUGSoutput$summary[,2])>1.96,'*',''))
