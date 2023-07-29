
#### ESTIMATION OF DCE MODELS USING WINBUGS ####
#### 1. MNL MODELS ####
rm(list = ls())
library(data.table)
library(runjags)
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

##### wide format data
data.coffee$personIDseq = match(data.coffee$personID,table=ids)

##### long format data
data_longformat$personID = match(data_longformat$personID,table=ids)

##### recode sets in block 2 to be 1:6 instead of 7:12. 
data_longformat$setID<-ifelse(data_longformat$setID<=6,data_longformat$setID,data_longformat$setID-6)

#### WINBUGS CODES ####
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

mnlcoffeeset<- '
model{

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

}'

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

monitor<-c('asc0est','tastedesert','tastemokka','brazilorig','indonesiaorig','netherlandsprod','italyprod', 'price','biolab','fairtrade')

# running the model
mnl = run.jags(model = mnlcoffeeset, modules = 'glm',data = dcof,
                  monitor = c(monitor,'D', 'deviance', 'dic'),  
                   burnin = 50000, n.chains = 3, sample = 100000, 
                  inits = list(inits1,inits2,inits3))
mnl$summary$statistics
xtable::xtable(cbind(round(mnl$summary$statistics[,1],3),round(mnl$summary$statistics[,2],3),ifelse(abs(mnl$summary$statistics[,1]/mnl$summary$statistics[,2])>1.96,'*','')))
beepr::beep(sound = 8) #ppc=0.24(0.43)

# calculate deviances at the mean parameters
meanpars=rbind(as.data.frame(mnl$mcmc[[1]][,colnames(mnl$mcmc[[1]][,monitor])]),as.data.frame(mnl$mcmc[[2]][,colnames(mnl$mcmc[[1]][,monitor])]),
               as.data.frame(mnl$mcmc[[3]][,colnames(mnl$mcmc[[1]][,monitor])]))
meanpars0=apply(meanpars, 2, function(x) list(mean=mean(x) , sd=sd(x)))

# calculate DIC manually
m1=function(x){
  u= array(0, dim = c(N,S,J))
  den= array(0, dim = c(N,S))
  pr= array(0, dim = c(N,S,J))
  LLmeans= array(0, dim = c(N,S,J))
  for(i in 1:N){ #respondents
    for(s in 1:S){ # choice sets
      # utilities
      u[i,s,1] = exp(x$asc0est$mean) # opt-out
      u[i,s,2] =  exp(x$fairtrade$mean*faitradematrix[i,s,2] + x$biolab$mean*biolabmatrix[i,s,2] + x$brazilorig$mean*obrazil[i,s,2] + 
                        x$indonesiaorig$mean*oindonesia[i,s,2] + x$netherlandsprod$mean*pnetherlands[i,s,2] + x$italyprod$mean*pitaly[i,s,2] + 
                        x$price$mean*cpricematrix[i,s,2] + x$tastedesert$mean*tdessert[i,s,2] + x$tastemokka$mean*tmokka[i,s,2] )
      
      u[i,s,3] = exp(x$fairtrade$mean*faitradematrix[i,s,3] + x$biolab$mean*biolabmatrix[i,s,3] + x$brazilorig$mean*obrazil[i,s,3] + 
                       x$indonesiaorig$mean*oindonesia[i,s,3] + x$netherlandsprod$mean*pnetherlands[i,s,3] + x$italyprod$mean*pitaly[i,s,3] + 
                       x$price$mean*cpricematrix[i,s,3] + x$tastedesert$mean*tdessert[i,s,3] + x$tastemokka$mean*tmokka[i,s,3]) 
      
      u[i,s,4] = exp(x$fairtrade$mean*faitradematrix[i,s,4] + x$biolab$mean*biolabmatrix[i,s,4] + x$brazilorig$mean*obrazil[i,s,4] + 
                       x$indonesiaorig$mean*oindonesia[i,s,4] + x$netherlandsprod$mean*pnetherlands[i,s,4] + x$italyprod$mean*pitaly[i,s,4] + 
                       x$price$mean*cpricematrix[i,s,4] + x$tastedesert$mean*tdessert[i,s,4] + x$tastemokka$mean*tmokka[i,s,4] )
      
      u[i,s,5] = exp(x$fairtrade$mean*faitradematrix[i,s,5] + x$biolab$mean*biolabmatrix[i,s,5] + x$brazilorig$mean*obrazil[i,s,5] + 
                       x$indonesiaorig$mean*oindonesia[i,s,5] + x$netherlandsprod$mean*pnetherlands[i,s,5] + x$italyprod$mean*pitaly[i,s,5] + 
                       x$price$mean*cpricematrix[i,s,5] + x$tastedesert$mean*tdessert[i,s,5] + x$tastemokka$mean*tmokka[i,s,5]) 
      
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
        LLmeans[i,s,j]<-dset[i,s,j]*log(pr[i,s,j])
      }
    } # end choice sets
  } # end respondents
  
  # Dhat
  Dhat<- -2*sum(LLmeans[,,]); # calculated deviance at mean parameter values
  return(list(Dhat=Dhat))
}

pD=as.numeric(mnl$summary$statistics['D',1])-m1(meanpars0)$Dhat
DIC<- pD+as.numeric(mnl$summary$statistics['D',1])

# option 2
d=rbind(as.data.frame(mnl$mcmc[[1]][,'D']),as.data.frame(mnl$mcmc[[2]][,'D']),as.data.frame(mnl$mcmc[[3]][,'D']))

pD1=0.5*var(d$var1)
DIC1=mean(d$var1)+pD1

