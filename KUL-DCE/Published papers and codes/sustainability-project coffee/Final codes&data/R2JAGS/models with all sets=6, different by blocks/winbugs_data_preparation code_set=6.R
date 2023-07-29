
#### DATA MANAGEMENT FOR WINBUGS ####
rm(list = ls())
library(data.table)
library(runjags)
setwd('C:/KU_Leuven/DCE/PhD.projects/1st_yr/project_coffee')

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

#### products attributes ####
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

#### socio-economic variables ####
data_socio_econ<- read.table('datasets/data_socio_econ.csv', header = TRUE, sep = ',')
data_socio_econ= data_socio_econ[,-c(1)]

data_socio_econ$age= ifelse(!is.na(data_socio_econ$dob), 2017 - data_socio_econ$dob, 2017-2017)
data_socio_econ = data_socio_econ[,-c(8)]

# centering age so that resulting models
data_socio_econ$cage= (data_socio_econ$age-mean(data_socio_econ$age))/sd(data_socio_econ$age)

# age
age = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  age[i]= data_socio_econ$cage[i]
}
# sex
sex = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  sex[i]= data_socio_econ$sex[i]
}

# education
# effect coded education levels
table(data_socio_econ$Q45)
data_socio_econ$efcodedhnonuniv[data_socio_econ$Q45=='Hoger niet-universitair onderwijs']<-1
data_socio_econ$efcodedhnonuniv[data_socio_econ$Q45=='Lager onderwijs'|data_socio_econ$Q45=='Secundair onderwijs']<--1
data_socio_econ$efcodedhnonuniv[data_socio_econ$Q45=='Universitair onderwijs']<-0
table(data_socio_econ$efcodedhnonuniv)

# higher education
data_socio_econ$efcodedhuniv[data_socio_econ$Q45=='Hoger niet-universitair onderwijs']<--0
data_socio_econ$efcodedhuniv[data_socio_econ$Q45=='Lager onderwijs'|data_socio_econ$Q45=='Secundair onderwijs']<--1
data_socio_econ$efcodedhuniv[data_socio_econ$Q45=='Universitair onderwijs']<-1
table(data_socio_econ$efcodedhuniv)

# higher-education[nonuniversity]
hnonuniv = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  hnonuniv[i]= data_socio_econ$efcodedhnonuniv[i]
}
# higher-education[university]
huniv = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  huniv[i]= data_socio_econ$efcodedhuniv[i]
}
# income between 1500-2000
inc15_20 = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  inc15_20[i]= data_socio_econ$income1500_2000[i]
}

# income between 2000-3000
inc20_30 = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  inc20_30[i]= data_socio_econ$income2000_3000[i]
}
# income between 3000+
inc30plus = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){
  inc30plus[i]= data_socio_econ$income3000_plus[i]
}

# scales data sets [NEP, AHO and ASB]: in all these scales, use the 1st factor from factor analysis
# since they all have high internal consistencies [high cronbach's indices]

nep=read.table('datasets/nep.csv', header = TRUE, sep=',')
nep=nep[,-1]
nep=nep[order(nep$personID),]
nepmatrix = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){ # where the mean is missing, replace it with 0. such a case did not answer the question
  nepmatrix[i]= ifelse(!is.na(nep$PA1_nep[i]),nep$PA1_nep[i],0)
}

# AHO
aho= read.table('datasets/aho.csv', header = TRUE, sep = ',')
aho=aho[,-1]
aho=aho[order(aho$personID),]
ahomatrix = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){ # where the mean is missing, replace it with 0. such a case did not answer the question
  ahomatrix[i]= ifelse(!is.na(aho$PA1_aho[i]),aho$PA1_aho[i],0)
}

# ASB
asb= read.table('datasets/asb.csv', header = TRUE, sep = ',')
asb=asb[,-1]
asb=asb[order(asb$personID),]
asbmatrix = matrix(0, nrow = N, ncol = 1)
for(i in 1:N){# where the mean is missing, replace it with 0. such a case did not answer the question
  asbmatrix[i]= ifelse(!is.na(asb$PA1_asb[i]),asb$PA1_asb[i],0)
}
