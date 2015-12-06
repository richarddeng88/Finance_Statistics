######################### Get/Set/Show working directory ###########################

# Get working directory, where your scripts, data, output files are saved
getwd() 

# Name my working directory, an example
myWD = "W:/Data/S&P500"

# Set my working directory as myWD, an example
setwd(myWD)

# Show the names of the files, folders under the directory 'myWD', an exmaple
dir(myWD)

#################### PCA for S&P500 Stocks data #######################
library(quantmod)
library(lattice)
library(stats)
load("SP500 company data 2012.RData")

SPcompany = read.table("SP500 companies.csv", header=T, sep=",")
colnames(SPcompany)
table(SPcompany[,"GICS.Sector"])  # summary of the # of companies in each sector

# calculate daily returns for all 500 stocks
SP500dailyReturn = t(apply(Close, MARGIN=1, function(a) t(dailyReturn(a))))
SP500dailyReturn = SP500dailyReturn[,-1]
colnames(SP500dailyReturn) = colnames(Close)[-1]

SP500tickers <- rownames(SP500dailyReturn)

table(SPcompany[,"GICS.Sector"])
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Consumer Discretionary")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Consumer Staples")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Energy")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Financials")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Health Care")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Industrials")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Information Technology")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Materials")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Telecommunications Services")]
SP500tickers[which(SPcompany[,"GICS.Sector"]=="Utilities")]

#choosetickers = c("GS","MCD","PEP","CVX","PFE","CAT","MSFT","MON","T","NU")
#choose = c(211, 294, 356, 99, 360, 89, 305, 309, 46, 335)

choosetickers = c("GS","JPM","MS","C","WFC","BAC","USB")
choose = c(211, 255, 312, 106, 480, 56, 455)

subdata = SP500dailyReturn[choose,]
Ndays   = dim(subdata)[2]

#covmatrix = cov(t(subdata[,Ndays-126:Ndays]))
#cormatrix = cor(t(subdata[,Ndays-126:Ndays]))
covmatrix = cov(t(subdata[,1:Ndays]))
cormatrix = cor(t(subdata[,1:Ndays]))

setwd('W:/RCode/Utils')

source("heatmap.R")
#png("SectorsHeatmap.png", width=1024, height=768)
myheatmap(cormatrix,0,1,choosetickers)
#dev.off()

setwd(myWD)

#PCA1 = princomp(t(subdata[,Ndays-126:Ndays]), cor=T)
PCA1 = princomp(t(subdata[,1:Ndays]), cor=F)

summary(PCA1)
PCA1$loadings
PCA1_var = PCA1$sdev^2
PCA1_var_perc = PCA1_var/sum(PCA1_var)

#png("PCAsectors.png", width=1024, height=500)
par(mfrow=c(1,2))
barplot(PCA1_var_perc, ylim=c(0,1), ylab="Var(%)",cex.lab=1.5,cex.axis=2)
barplot(PCA1_var, ylim=c(0,sum(PCA1_var)),ylab="var",cex.lab=2,cex.axis=2)
#dev.off()
par(mfrow=c(1,1))

screeplot(PCA1)
biplot(PCA1, pc.biplot=T)

### Compute the determinant of the sample covariance matrix for all S&P500 stocks
det(var(t(SP500dailyReturn)))  # the determinant is 0 which implies a strong multi-collinearity

### weights for first PC factor
w1=PCA1$loadings[,1]/sum(PCA1$loadings[,1])
### weights for second PC factor
w2=PCA1$loadings[,2]/sum(PCA1$loadings[,2])
### weights for third PC factor
w3=PCA1$loadings[,3]/sum(PCA1$loadings[,3])

### Returns on the first 3 PC factors (use matrix multiplication operator %*% )
Rf1 = t(subdata) %*% w1
Rf2 = t(subdata) %*% w2
Rf3 = t(subdata) %*% w3

### double check the orthogonality
cov(Rf1, Rf2)
cov(Rf2, Rf3)
cov(Rf1, Rf3)

mydates = as.Date(colnames(subdata))
#png("SectorsTS2.png", width=768, height=1024)
par(mfrow=c(3,3))
for (i in 1:7){
  subTS = zoo(subdata[i,], mydates)
  subTS = xts(subTS, index(subTS))
  plot(subTS[1:Ndays], main=choosetickers[i],mai=c(-3,0,-3,0))  
}
par(mfrow=c(3,3))
subTS = zoo(Rf1, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='1st PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(Rf2, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='2nd PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(Rf3, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='3rd PC factor returns',mai=c(-3,0,-3,0)) 
#dev.off()
par(mfrow=c(1,1))
