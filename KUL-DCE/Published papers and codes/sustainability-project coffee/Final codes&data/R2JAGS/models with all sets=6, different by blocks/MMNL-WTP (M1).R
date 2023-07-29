############                  1st version: 26-07-2018 by LMaaya
############                  MMNL[M1] MODEL-WTP SPACE #############################
############                  1.correlated tastes' dummies            
############                  2.correlated Ftrade and Biolable, 
############                  3.biolable/tastes/ftrade as normals, 
############                  4.price fixed 
############                  5. country variables fixed
############                  6. wishart priors for covariance matrices

setwd('C:/KU_Leuven/DCE/project_coffee')

# script with initial data management for WINBUGS
datamcoffeewinbugs <- parse("scripts/WINBUGS/models with all sets=6, different by blocks/winbugs_data_preparation code_set=6.R") 
eval(datamcoffeewinbugs) 

############       winbugs code ####

mmnl_WTP_cor_taste_ftbl_lg_price_model<- '
model{

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

# deviance+DIC
TL<-sum(LL[,,]); # total Log-likelihood
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

}' # end of model

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

#### time taken ####
# samples=3000, burnin=1000: 35mins[DIC=3304.879, PED=4766.398]
# samples 5000, burnin=2500: 1.2hrs [no DIC,no PED]. same for higher samples

#### running the model####
mmnl_M1_WTP= run.jags(model = mmnl_WTP_cor_taste_ftbl_lg_price_model, data = mmnldcof,
                                       monitor =  c('asc0est','mdes','mmok','sigma.des','sigma.mok','rho_des_mok','brazilorig','indonesiaorig',
                                                    'netherlandsprod','italyprod','mprice','tauprice','mbl','sigma.biolable',
                                                    'mft','sigma.ftrade', 'rho_ftrade_biolable','dic','deviance','D','pd','popt','ped'), 
                                       modules='glm',burnin = 50000, n.chains = 3, sample = 100000,
                                       inits = list(inits1,inits2,inits3))

cbind(round(mmnl_M1_WTP$summary$statistics[,1:2],3),
      ifelse(abs(mmnl_M1_WTP$summary$statistics[,1]/
                   mmnl_M1_WTP$summary$statistics[,2])>=1.96,"*",""))


############            calculate DICs manually #######
# for MMNL, using only means for parameters and not the fact that they come from a distribution?

monitor=c('asc0est','mdes','mmok','sigma.des','sigma.mok','rho_des_mok','brazilorig','indonesiaorig',
          'netherlandsprod','italyprod','mprice','tauprice','mbl','sigma.biolable',
          'mft','sigma.ftrade', 'rho_ftrade_biolable')


# calculate deviances at the mean parameters
meanpars=rbind(as.data.frame(mmnl_M1_WTP$mcmc[[1]][,colnames(mmnl_M1_WTP$mcmc[[1]][,monitor])]),as.data.frame(mmnl_M1_WTP$mcmc[[2]][,colnames(mmnl_M1_WTP$mcmc[[1]][,monitor])]),
               as.data.frame(mmnl_M1_WTP$mcmc[[3]][,colnames(mmnl_M1_WTP$mcmc[[1]][,monitor])]))
meanpars0=apply(meanpars, 2, function(x) list(mean=mean(x) , sd=sd(x)))

# calculate DIC manually
m1=function(x){
  u= array(0, dim = c(N,S,J))
  den= array(0, dim = c(N,S))
  pr= array(0, dim = c(N,S,J))
  LLmeans= array(0, dim = c(N,S,J))
  ftbl<- mvrnorm(1,c(x$mft$mean,x$mbl$mean),matrix(c(x$sigma.ftrade$mean**2,x$sigma.ftrade$mean*x$sigma.biolable$mean*x$rho_ftrade_biolable$mean,
                                                     x$sigma.ftrade$mean*x$sigma.biolable$mean*x$rho_ftrade_biolable$mean,x$sigma.biolable$mean**2),2,2))
  desmok<-mvrnorm(1,c(x$mdes$mean,x$mmok$mean),matrix(c(x$sigma.des$mean**2,x$sigma.des$mean*x$sigma.mok$mean*x$rho_des_mok$mean,
                                                        x$sigma.des$mean*x$sigma.mok$mean*x$rho_des_mok$mean,x$sigma.mok$mean**2),2,2))
  priced<- rnorm(1,exp(2*x$mprice$mean+x$tauprice$mean**2),(exp(2*x$mprice$mean+x$tauprice$mean**2)*(exp(x$tauprice$mean**2)-1))**0.5)
  for(i in 1:N){ #respondents
    for(s in 1:S){ # choice sets
      # utilities
      u[i,s,1] = exp(x$asc0est$mean) # opt-out
      u[i,s,2] =  exp(ftbl[1]*faitradematrix[i,s,2] +ftbl[2]*biolabmatrix[i,s,2] + 
                     x$brazilorig$mean*obrazil[i,s,2] + x$indonesiaorig$mean*oindonesia[i,s,2] + x$netherlandsprod$mean*pnetherlands[i,s,2] + 
                     x$italyprod$mean*pitaly[i,s,2] + priced*cpricematrix[i,s,2] + 
                       desmok[1]*tdessert[i,s,2] + desmok[2]*tmokka[i,s,2])
      
      u[i,s,3] = exp(ftbl[1]*faitradematrix[i,s,3] +ftbl[2]*biolabmatrix[i,s,3] + 
                       x$brazilorig$mean*obrazil[i,s,3] + x$indonesiaorig$mean*oindonesia[i,s,3] + x$netherlandsprod$mean*pnetherlands[i,s,3] + 
                       x$italyprod$mean*pitaly[i,s,3] + priced*cpricematrix[i,s,3] + 
                       desmok[1]*tdessert[i,s,3] + desmok[2]*tmokka[i,s,3])
      
      u[i,s,4] =exp(ftbl[1]*faitradematrix[i,s,4] +ftbl[2]*biolabmatrix[i,s,4] + 
                      x$brazilorig$mean*obrazil[i,s,4] + x$indonesiaorig$mean*oindonesia[i,s,4] + x$netherlandsprod$mean*pnetherlands[i,s,4] + 
                      x$italyprod$mean*pitaly[i,s,4] + priced*cpricematrix[i,s,4] + 
                      desmok[1]*tdessert[i,s,4] + desmok[2]*tmokka[i,s,4])
      
      u[i,s,5] = exp(ftbl[1]*faitradematrix[i,s,5] +ftbl[2]*biolabmatrix[i,s,5] + 
                       x$brazilorig$mean*obrazil[i,s,5] + x$indonesiaorig$mean*oindonesia[i,s,5] + x$netherlandsprod$mean*pnetherlands[i,s,5] + 
                       x$italyprod$mean*pitaly[i,s,5] + priced*cpricematrix[i,s,5] + 
                       desmok[1]*tdessert[i,s,5] + desmok[2]*tmokka[i,s,5])
      
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

pD=as.numeric(mmnl_M1_WTP$summary$statistics['D',1])-m1(meanpars0)$Dhat
DIC<- 2*pD+as.numeric(mmnl_M1_WTP$summary$statistics['D',1])

#### DIC option 2 ####
d=rbind(as.data.frame(mmnl_M1_WTP$mcmc[[1]][,'D']),as.data.frame(mmnl_M1_WTP$mcmc[[2]][,'D']),as.data.frame(mmnl_M1_WTP$mcmc[[3]][,'D']))

pD1=0.5*var(d$var1)
DIC1=mean(d$var1)+pD1
