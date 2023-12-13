options(width = 100, useFancyQuotes = FALSE)
rm(list=ls()) 

############################################################
# part 4: multivariate statistics 
############################################################
#install.packages('nFactors',dependencies=TRUE) 
#install.packages('psych',dependencies=TRUE)
#install.packages('candisc',dependencies=TRUE)
#install.packages('readxl',dependencies=TRUE)
#install.packages('MASS',dependencies=TRUE)
#install.packages('corrplot',dependencies=TRUE)
#install.packages('fastDummies',dependencies=TRUE)
#install.packages('matrixcalc',dependencies=TRUE)
#install.packages("ICSNP",dependencies=TRUE)
library(nFactors) 
library(psych)
library(candisc)
library(readxl)
library(MASS)
library(corrplot)
library(fastDummies)
library(matrixcalc)
library(ICSNP)
library(GPArotation)

#set the default directory with setwd, for instance
setwd("C:/Users/u0118298/OneDrive/Projects/MVandebroek/TAKEN")  
#make sure the data are in the data subdirectory (here C:\Documents\ATSTAT\data)
rm(list=ls())    
############################################################
# Principal Component Analysis
############################################################

#----------------------------------------------------------------------------
# toy example
#----------------------------------------------------------------------------
toydata = read.table( "data/examplePCA.txt", header = TRUE)
head(toydata)
plot(toydata$X1,toydata$X2)
s=cov(toydata)                         #compute covariance matrix
ev=eigen(s)                            #compute eigenvalues and eigenvectors
ev  

pcatoy = prcomp(toydata, scale=FALSE)  #no standardization!
pcatoy
pcatoy$x  #remark the opposite signs (irrelevant)

cor(cbind(toydata,pcatoy$x))
round(cor(cbind(toydata,pcatoy$x)),digits = 4)
rm(list = ls()) 

#----------------------------------------------------------------------------
# foodprices (with standardized data)
#----------------------------------------------------------------------------
dpca = read.table("data/foodpricedata.txt", header=T)
head(dpca,n=2)                 

s=cor(dpca[,2:6])           #compute correlation matrix  
ev=eigen(s)                 #compute eigenvalues and eigenvectors
ev  
plot(ev$values,type="o",pch=16)
abline(h=1,col="red")       #cut off at 1

#plot the eigenvalues and correct for sampling with Horn's parallel procedure
plot(ev$values,col="blue",xlim=c(0,6),ylim = c(0,3),ylab="eigenvalues")
#(Horn used the 95th percentile, the 5th percentile is the default?)
ap= parallel(subject = nrow(dpca), var = 5, rep= 1000,cent = 0.95)
lines(ap$eigen$qevpea,type = "o",pch = 15, lwd = 2,col = "red")
title(main= "Horn's parallel procedure - 95% percentile",cex.main=0.75)


#with scale=TRUE the data are standardized (= start from correlation matrix)
pcaout<- prcomp(dpca[,2:6], scale=TRUE)
pcaout        
#names(pcaout)  #contains several variables that we do not need              
head(pcaout$x,n=3)  #PC scores
pcwithnames = cbind.data.frame(dpca[,1],pcaout$x) #adding cities to scores
names = as.character(dpca[,1])

par(mar=c(3,3,3,3))
plot(pcwithnames$PC1,pcwithnames$PC2, main="PC scores",
     xlab="PC1 ", ylab="PC2",  xlim = c(-2,5),ylim = c(-3,3),pch=19)
text(pcwithnames$PC1,pcwithnames$PC2, pos=3, labels=names ,cex = 0.3)


par(mar=c(4,4,4,4))
biplot(pcaout, choices=1:2,xlabs = names,cex=0.3,cex.axis=0.7,cex.lab=0.7)


rm(list=ls()) 

#----------------------------------------------------------------------------
# foodprices (without standardization)
#----------------------------------------------------------------------------
dpca = read.table("data/foodpricedata.txt", header=T)
head(dpca,n=2)                 

pcaout<- prcomp(dpca[,2:6], scale=FALSE)
pcaout        

#structuralloadings:
sl=cor(cbind(dpca[,2:6],pcaout$x))
sl[1:5,6:10]

pcwithnames = cbind.data.frame(dpca[,1],pcaout$x) #adding cities to scores
names = as.character(dpca[,1])

par(mar=c(4,4,4,4))
biplot(pcaout, choices=1:2,xlabs = names,cex=0.3,cex.axis=0.7,cex.lab=0.7)


rm(list = ls()) 


############################################################
# exploratory factor analysis
############################################################
#----------------------------------------------------------------------------
# toy example
#----------------------------------------------------------------------------
cm <- matrix(c(1, 0.62, 0.54, 0.32, 0.284, 0.37, 
               0.62, 1, 0.51, 0.38, 0.351, 0.43,
               0.54, 0.51, 1, 0.36, 0.336, 0.405,
               0.32, 0.38, 0.36, 1, 0.686, 0.730,
               0.284, 0.351, 0.336, 0.686, 1, 0.735,
               0.37, 0.43, 0.405, 0.730, 0.735, 1),nrow = 6, ncol = 6)
rownames(cm) <- c("m","p","c","e","h","f")
colnames(cm) <- c("m","p","c","e","h","f")
#cm

#basic principal factoring 
#we use the fa function which does iterative factoring but with max.iter=1 and method fm = "pa"
#R gives the warning "maximum iteration exceeded" which means that he can improve by iterating further
princ = fa(cm,fm="pa",nfactors= 2,rotate = "none",residuals = TRUE,max.iter=1,SMC=FALSE)

#in the output: h2 are communalities, u2 are specificities
#and com is a complexity measure indicating how many factors are used to explain each variable on average
princ

#residual correlation matrix with uniqueness on the diagonal:
princ$residual 


#iterative principal component analysis
#SMC=FALSE, so starting from communalities = 1
faout <- fa(cm,nfactors=2, fm="pa", SMC=FALSE,residuals = TRUE, rotate = "none") 
faout           
round(faout$residual ,digits =5)


faoutrot <- fa(cm,nfactors=2, fm="pa", SMC=TRUE, residuals = TRUE, rotate = "varimax") 
faoutrot
faoutrot$rot.mat

#similarly one can get the ML solution
#faoutml <- fa(cm,nfactors=2, fm="ml", SMC=TRUE,residuals = TRUE, rotate = "none") 
#faoutml

KMO(cm)

rm(list = ls()) 



#----------------------------------------------------------------------------
# detergent data
#----------------------------------------------------------------------------
detergent = read.table("data/detergent.txt",header =FALSE)
#detergent
KMO(detergent)

fadetpa = fa(detergent,nfactors=2, fm="pa", SMC=TRUE, 
             residuals = FALSE, rotate = "quartimax") 
#fadetpa  #output not shown

fadetml<- fa(detergent,nfactors=2, fm="ml", SMC=FALSE, 
             residuals = TRUE, rotate = "quartimax") 
fadetml  #slightly different results than in slides > less iterations?
rm(list = ls()) 



############################################################
# discriminant analysis
############################################################
#----------------------------------------------------------------------------
# canonical discriminant analysis 
#----------------------------------------------------------------------------
ddiscr = read_xlsx( "data/accident.xlsx")
head(ddiscr,n=3)

HotellingsT2(cbind(ddiscr$experience,ddiscr$hp)~ ddiscr$accident)

cand.mod <- lm(cbind(hp,experience)~ accident, data=ddiscr)
cand.out <- candisc(cand.mod)
summary(cand.out)

plot(cand.out,cex=0.25,cex.lab=0.5)
#coefficients (and scores) have opposite signs than in the slides
cand.out$coeffs.raw 
cand.out$structure
scores = cand.out$scores
sortedscores = scores[order(scores$Can1),]
sortedscores

rm(list = ls()) 


#----------------------------------------------------------------------------
# linear and quadratic classification functions 
#----------------------------------------------------------------------------
ddiscr = read_xlsx( "data/accident.xlsx")


#lda uses by default priors proportional to sample sizes
#the first prior value corresponds to the first factor level
ddiscr$accident = as.factor(ddiscr$accident)
fitl<- lda(accident ~ hp + experience ,ddiscr,prior=c(0.5,0.5))
fitl

##scores, posterior probabilities and classification:
pp = predict(fitl,ddiscr)  
pp
scoresout = pp$x  
#check that here the scores are rescaled such that the average of both mean group scores is zero:
together = cbind(scoresout,ddiscr)
tapply(together$LD1,ddiscr$accident,mean)

#classification, resubstitution
table(observed = ddiscr$accident,predicted = pp$class) 

#CV = TRUE returns the results of leave-one-out crossvalidation 
#and the corresponding posterior probabilities
#remark that when CV is used, the predict function can no longer be used!
fitlcv<- lda(accident ~ hp + experience ,ddiscr,prior=c(0.5,0.5), CV=TRUE)
head(fitlcv$posterior)
table(observed=ddiscr$accident,predicted=fitlcv$class )

#different priors, resubstitution
fitpr<- lda(accident ~ hp + experience ,ddiscr,prior=c(0.1,0.9), CV=FALSE)
pp = predict(fitpr,ddiscr)  
table(observed = ddiscr$accident,predicted = pp$class) 

#different priors, crossvalidation
fitlcvp<- lda(accident ~ hp + experience ,ddiscr,prior=c(0.1,0.9), CV=TRUE)
head(fitlcvp$posterior)
table(ddiscr$accident,fitlcvp$class )

#quadratic classification rule
fitq<- qda(accident ~ experience + hp,ddiscr,prior=c(0.5,0.5),CV=F)
pp = predict(fitq,ddiscr)  
table(ddiscr$accident,pp$class) 

fitq<- qda(accident ~ experience + hp,ddiscr,prior=c(0.1,0.9),CV=T)
head(fitq$posterior)
table(ddiscr$accident,fitq$class )

#test the equality of the covariance matrices
boxM(cbind(ddiscr$experience,ddiscr$hp),ddiscr$accident)

rm(list = ls()) 


############################################################
# cluster analysis
############################################################
#----------------------------------------------------------------------------
# toy example
#----------------------------------------------------------------------------
clus.dat <- matrix(c(5,5,6,6,15,14,16,15,25,20,30,19),
                   nrow = 6, ncol=2,byrow=TRUE)
clus.dat <- cbind.data.frame(c("s1","s2","s3","s4","s5","s6"),clus.dat)
colnames(clus.dat) <- c("names","income","education")
#clus.dat

par(mfrow=c(1,1)); par(mar=c(4,4,4,4))
plot(education~income,data =clus.dat,xlim=c(0,30),ylim = c(0,30),cex=0.2)  
text(education~income,data =clus.dat, pos=3,labels=names ,cex = 0.40)


#input is the distance matrix!
clus.compl<-hclust(dist(clus.dat[,2:3]), method = "complete") 
clus.compl
clus.compl$merge  #shows the iterations
clust3 <- cutree(clus.compl, k=3) # cut tree at 3 clusters
clust3


plot(clus.compl,labels = clus.dat$names, hang = -1,cex=0.8)  #plot dendrogram


clus.single<-hclust(dist(clus.dat[,2:3]), method = "single") 
#plot(clus.single,labels = clus.dat$names, hang = -1,cex=0.8) 

clus.centr<-hclust(dist(clus.dat[,2:3]), method = "centroid") 
#plot(clus.centr,labels = clus.dat$names, hang = -1,cex=0.8) 

clus.ward<-hclust(dist(clus.dat[,2:3]), method = "ward.D2") 
#plot(clus.ward,labels = clus.dat$names, hang = -1,cex=0.8)   



#compare 2 sets of initial seeds in K-means clustering
initial = clus.dat[1:3,2:3]
clus.kmeans <- kmeans(clus.dat[-1], centers=initial) 
clus.kmeans

initial = subset(clus.dat, names %in% c("s1","s3" ,"s5"))
initial = initial[,-1]
clus.kmeans <- kmeans(clus.dat[-1], centers=initial) 
clus.kmeans

rm(list = ls()) 

#----------------------------------------------------------------------------
# food data
#----------------------------------------------------------------------------
dcal <- read.table("data/calories.txt", header=T)
head(dcal,n=5)

plot(dcal)
dcal[,2:6]=scale(dcal[,2:6])  #standardize the numerical variables
distcal <- dist(dcal[,2:6])
clus.single<-hclust(distcal, method = "single") 

par(mar=c(5,5,5,5))
plot(clus.single,labels = dcal$fooditem,hang = -1,cex=0.5)  #plot dendrogram

clust5s <- cutree(clus.single, k=5) # cut tree into 5 clusters
clust5s
solution = data.frame(cbind(clust5s,dcal[,1]))
names(solution) =c("cluster","name")
percluster= solution[order(solution$cluster),]
percluster

clus.compl<-hclust(distcal, method = "complete") 

plot(clus.compl,labels = dcal$fooditem,hang = -1,cex=0.4)  #plot dendrogram

clust5c <- cutree(clus.compl, k=5) # cut tree into 5 clusters
clust5c

clus.centr<-hclust(distcal, method = "centroid") 
#plot(clus.centr,labels = dcal$fooditem,hang = -1,cex=0.4)  #plot dendrogram
clust5ct <- cutree(clus.centr, k=5) # cut tree into 5 clusters
clust5ct

clus.ward<-hclust(distcal, method = "ward.D2") 
#plot(clus.ward,labels = dcal$fooditem,hang = -1,cex=0.4)  #plot dendrogram
clust5cw <- cutree(clus.ward, k=5) # cut tree into 5 clusters
clust5cw

#----------------------------------------------------------------------------
# sumsqs computes the R squared value of a clustering and is used 
# in the function plotRsq: you don't have to code this yourself, 
# you can use the function plotRsq(clout,D,m) with 
# - clout the output of a hierarchical clustering procedure
# - D the original dataset
# - m plot for 1, ... m clusters
#----------------------------------------------------------------------------
sumsqs = function (D){
  Y= scale(D[,-1],scale = FALSE)
  Y = as.matrix(Y)
  du = dummy_cols(D[,1])
  X = as.matrix(du[,-1])
  beta = solve(t(X)%*%X)%*%t(X)%*%Y
  P = X%*%beta
  R = Y - P
  W = t(R)%*%R ;   T = t(Y)%*%Y ;   B = t(P)%*%P              
  SSt = matrix.trace(T)
  SSw = matrix.trace(W)
  SSb = matrix.trace(B)
  Rsq= SSb/SSt
  rrr = cbind(SSb,SSw,SSt,Rsq)
  return(rrr)
}

#to plot evolution in Rsquared 
plotRsq = function (clout,D,m){
  Rs = NULL
  for (i in 1:m){
    sol = cutree(clout, k=i) 
    dat = cbind(sol,D )
    Rs = cbind(Rs,sumsqs(dat)[4])
  }
  plot(1:m,Rs,type="l")
}

plotRsq(clus.compl,dcal[,2:6],15)
title(main = "complete linkage")

#----------------------------------------------------------------------------
# k means clustering as in the slides
# it is complicated because I had to remove beaf heart and canned sardines,
# rescale the dataset again and I had to compute the centroids of the hierarchical
# clustering results > code not to be known
#----------------------------------------------------------------------------
#remove beafheart and canned sardines
dcalwithcluster = cbind(dcal,clust5c)
dcalreduced = dcalwithcluster[!(dcalwithcluster$fooditem %in% 
                                  c("Canned_sardines","Beef_heart")),]
#rownames(dcalreduced)=NULL #if you want to adapt the row numbers

dcalreduced[,2:6] = scale(dcalreduced[,2:6])  #rescale again

#compute cluster means as initial seeds for kmeans
initial = aggregate(dcalreduced[, 2:6], list(dcalreduced$clust5c), mean)

# 3 cluster solution starting from previous solution
clus.kmeans <- kmeans(dcalreduced[,2:6], centers=initial[2:6]) 
clus.kmeans

solution = cbind(dcalreduced[,1], clus.kmeans$cluster)
head(solution)
rm(list = ls()) 

dcal <- read.table("data/calories.txt", header=T)
head(dcal,n=5)

#----------------------------------------------------------------------------
# k means clustering on all observations (with beaf heart and canned sardines)
#----------------------------------------------------------------------------
#starting with random initial clusters
#to get the same solution every time use set.seed()
#without specifying a seed, the solution can be different each time

#remark that this "seed" is not the same as the seed which is used as synonym for initical centroids!
#this seed controls how random numbers are generated, if the seed is fixed by set.seed, every run of 
#the program will produce the same random numbers

dcal <- read.table("data/calories.txt", header=T)
head(dcal,n=5)
dcal[,2:6]=scale(dcal[,2:6])  
set.seed(3) 
clus.kmeans <- kmeans(dcal[,2:6], centers=5) 
clus.kmeans

#specify with nstart how many random initial seeds to take and report the best solution
clus.kmeans <- kmeans(dcal[,2:6], centers=5,nstart=50) 
clus.kmeans

clusternr = clus.kmeans$cluster
together= as.data.frame(cbind(dcal$fooditem,clusternr))
together[order(together$clusternr),]
rm(list = ls()) 
