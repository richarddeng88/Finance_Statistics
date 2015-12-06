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
load("SP500 company dailyReturn 2008.RData")
setwd('W:/Rcode/Utils')
source("heatmap.R")
setwd(myWD)

SPcompany = read.table("SP500 companies.csv", header=T, sep=",")
colnames(SPcompany)
table(SPcompany[,"GICS.Sector"])  # summary of the # of companies in each sector

SP500tickers <- rownames(SP500dailyReturn)

from = match('2012-01-03', colnames(SP500dailyReturn))
to = match('2012-06-29', colnames(SP500dailyReturn))
SP500dailyReturn = SP500dailyReturn[,from:to]
Ndays   = dim(SP500dailyReturn)[2]
select = !is.na(rowSums(SP500dailyReturn)) # select non-NaN stocks
subdata = SP500dailyReturn[select,]
N = dim(subdata)[1]

### PCA for all 500 stocks except those with NaNs
covmatrix = cov(t(subdata))
cormatrix = cor(t(subdata))

## check the multi-collinearity
eigen(cormatrix)$values

PCA2 = prcomp(t(subdata), cor=F)
PCA2_var_perc = PCA2$sdev^2/sum(PCA2$sdev^2)
barplot(PCA2_var_perc, ylim=c(0,1), ylab="Var(%)",cex.lab=1.5,cex.axis=2)
## define the cutoff of the explaining variability of selected PCs relative to
##   the variability of the first PC
cutoff = 0.05
nchoose = sum(PCA2_var_perc/max(PCA2_var_perc)>=cutoff) ## choose first nchoose PCs

RF = t(subdata) %*% (PCA2$rotation[,1:nchoose])
##  for (i in 1:nchoose) RF[,i]=RF[,i]/sum(PCA2$rotation[,i])

mydates = as.Date(colnames(subdata))

par(mfrow=c(3,1))
subTS = zoo(RF[,1], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='1st PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(RF[,2], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='2nd PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(RF[,3], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='3rd PC factor returns',mai=c(-3,0,-3,0)) 
#dev.off()
par(mfrow=c(1,1))

### get the regression coefficients beta's
betaT = matrix(NA, nrow=N, ncol=nchoose)
residuals = 0*subdata
for (i in 1:N){
  temp_lm = lm(subdata[i,]~RF)
  betaT[i,] = temp_lm$coefficients[-1]
  residuals[i,] = temp_lm$residuals
}
colnames(betaT) = colnames(RF)
rownames(betaT) = rownames(subdata)

hist(cor(t(residuals)))  ## histogram of the sample correlation on errors
hist(cor(t(subdata)))   ## histogram of the sample correlation on returns

## Approximated covariance and correlation matrix on returns
Sigma_R_est = betaT %*% cov(RF) %*% t(betaT) + 
              diag(diag(cov(t(residuals))))
Sigma_R = cov(t(subdata))

Rho_R_est = diag(1/sqrt(diag(Sigma_R_est))) %*% 
            Sigma_R_est %*%
            diag(1/sqrt(diag(Sigma_R_est)))

Rho_R = matrix(as.numeric(cor(t(subdata))), nrow=N, ncol=N)

## compare the distribution of the correlation matrix

par(mfrow=c(2,1))
hist(Rho_R_est)
hist(Rho_R)

par(mfrow=c(1,1))
hist(Rho_R_est-Rho_R)

### the heatmap of the true correlation matrix
myheatmap(Rho_R, -1, 1, 1:N)

### the heatmap of the estimated correlation matrix
myheatmap(Rho_R_est, -1, 1, 1:N)

### the heatmap of the estimation error for the correlation matrix
myheatmap(Rho_R-Rho_R_est, -1, 1, 1:N)

### print the eigen values of the estimated correlation matrix
eigen(Rho_R_est)$values

