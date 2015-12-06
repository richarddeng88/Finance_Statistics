# Name my working directory, an example
myWD = "W:/Data/S&P500"

# Set my working directory as myWD, an example
setwd(myWD)

# Show the names of the files, folders under the directory 'myWD', an exmaple
dir(myWD)

library(copula)   # for copula functions
library(fGarch)   # need for standardized t density
library(MASS)     # need for fitdistr and kde2d
library(fCopulae) # additional copula functions (pempiricalCopula
# and ellipticalCopulaFit)
library(quantmod)
library(xts)
library(timeDate)

#################### Load S&P500 Stocks data #######################
library(quantmod)
load("SP500 company dailyReturn 2008.RData")

SPcompany = read.table("SP500 companies.csv", header=T, sep=",")
colnames(SPcompany)
#table(SPcompany[,"GICS.Sector"])  # summary of the # of companies in each sector

mydates = as.Date(colnames(SP500dailyReturn))

ATT_Ret = SP500dailyReturn[46,]
VZ_Ret  = SP500dailyReturn[468,]
S_Ret   = SP500dailyReturn[421,]                    ### Sprint

png("ATT vs VZ.png", width=1024, height=768)
plot(ATT_Ret, VZ_Ret, pch=16, cex.axis=1,cex.lab=1,cex.main=1,
     xlab="returns on AT&T", ylab="returns on Verizon",
     main="Scatter Plot")
dev.off()

par(mfrow=c(3,1))
subTS = zoo(ATT_Ret, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS,main='AT&T')
subTS = zoo(VZ_Ret, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS,main='Verizon')
subTS = zoo(S_Ret, mydates)
subTS = xts(subTS, index(subTS))
plot(subTS,main='Sprint')

### fit marginal distributions by Gaussian
est.ATT = as.numeric(fitdistr(ATT_Ret,"normal")$estimate)
est.VZ  = as.numeric(fitdistr(VZ_Ret,"normal")$estimate)
# transformed by fitted marginal distributions
data1 = cbind( pnorm(ATT_Ret, mean=est.ATT[1], sd=est.ATT[2]),
               pnorm(VZ_Ret,mean=est.VZ[1], sd=est.VZ[2]) )

pairs(cbind(ATT_Ret,VZ_Ret,S_Ret),pch=20)

png("Histogram_Normal_Transformed.png", width=1024, height=768)
par(mfrow=c(1,2))
hist(data1[,1],main="Normal transformed returns",breaks=15,xlab='AT&T')
hist(data1[,2],main="Normal transformed returns",breaks=15,xlab='Verizon')
dev.off()

#plot(data1[,1],data1[,2], pch=20,
#     xlab="Normal-transformed AT&T returns",
#     ylab="Normal-transformed Verizon returns",
#     main="Bivariate uniforms")

### fit marginal distributions by T
est.ATT = as.numeric(fitdistr(ATT_Ret,"t")$estimate)
est.VZ = as.numeric(fitdistr(VZ_Ret,"t")$estimate)
est.S = as.numeric(fitdistr(S_Ret,"t")$estimate)
est.ATT[2] = est.ATT[2]*sqrt(est.ATT[3]/(est.ATT[3]-2))
est.VZ[2] = est.VZ[2]*sqrt(est.VZ[3]/(est.VZ[3]-2))
est.S[2] = est.S[2]*sqrt(est.S[3]/(est.S[3]-2))
# transformed by fitted marginal distributions
data2 = cbind( pstd(ATT_Ret, mean=est.ATT[1],sd=est.ATT[2],nu=est.ATT[3]),
               pstd(VZ_Ret,mean=est.VZ[1],sd=est.VZ[2],nu=est.VZ[3]) )

data3 = cbind( pstd(ATT_Ret, mean=est.ATT[1],sd=est.ATT[2],nu=est.ATT[3]),
               pstd(VZ_Ret,mean=est.VZ[1],sd=est.VZ[2],nu=est.VZ[3]),
               pstd(S_Ret,mean=est.S[1],sd=est.S[2],nu=est.S[3])     )

png("Histogram_T_Transformed.png", width=1024, height=768)
par(mfrow=c(1,2))
hist(data2[,1],main="T-transformed returns",breaks=15,xlab='AT&T')
hist(data2[,2],main="T-transformed returns",breaks=15,xlab='Verizon')
dev.off()

plot(data2[,1],data2[,2], pch=20, 
     xlab="T-transformed AT&T returns",
     ylab="T-transformed Verizon returns",
     main="Bivariate uniforms")

### Estimating df parameter for t-copula
cor_tau = cor(ATT_Ret, VZ_Ret, method="kendall")
t_omega = sin(pi*cor_tau/2)
cop_t_dim2 = tCopula(t_omega, dim = 2, dispstr = "un", df = 4)

### Fit Copula based on data2
fnorm = fitCopula(data=data2,  
                  method = "ml", optim.method="BFGS", 
                  copula=normalCopula(-0.3, dim=2), start=0.5)
ft2 = fitCopula(cop_t_dim2, data=data2,
                method = "ml", optim.method="L-BFGS-B", 
                start=c(t_omega,5),lower=c(0,2.5),upper=c(.5,15) )
#fgumbel = fitCopula(data=data2,
#                    method = "itau", 
#                    copula=gumbelCopula(dim=2),start=1)
fgumbel = fitCopula(data=data2,
                    method = "ml", optim.method="BFGS",
                    copula=gumbelCopula(3,dim=2),start=1)
ffrank = fitCopula(data=data2,
                   method = "ml", optim.method="BFGS", 
                   copula=frankCopula(3,dim=2),start=1)
fclayton = fitCopula(data=data2,
                     method = "ml", optim.method="BFGS", 
                     copula=claytonCopula(1,dim=2),start=1)

AICfit = round(cbind(AIC(fnorm),AIC(ft2),AIC(fgumbel),AIC(ffrank),AIC(fclayton)),2)
colnames(AICfit) = c('Normal', 'T', 'Gumbel', 'Frank', 'Clayton') 
AICfit

# Compare the estimated PDF with the empirical copula density
u1 = data2[,1]
u2 = data2[,2]
png("Copula Density.png", width=1024, height=768)
par(mfrow=c(2,3))
contour(kde2d(u1,u2),main="KDE",nlevels=10,cex.main=2,cex.axis=2,labcex=1.5)
contour(normalCopula(coef(fnorm)),dcopula,
        main="Normal",nlevels=50,cex.main=2,cex.axis=2,labcex=1.5)
contour(tCopula(param=coef(ft2)[1],df=round(coef(ft2)[2])),
        dcopula,main="t",nlevels=50,cex.main=2,cex.axis=2,labcex=1.5)
contour(gumbelCopula(coef(fgumbel),dim=2),
        dcopula,main="Gumbel",nlevels=30,cex.main=2,cex.axis=2,labcex=1.5)
contour(frankCopula(coef(ffrank),dim=2),
        dcopula,main="Frank",nlevels=25,cex.main=2,cex.axis=2,labcex=1.5)
contour(claytonCopula(coef(fclayton),dim=2),
        dcopula,main="Clayton",nlevels=30,cex.main=2,cex.axis=2,labcex=1.5)
dev.off()

####################  Multivariate Copula  #######################
### fit marginal distributions by T

### Fit Copula based on data3
fnorm = fitCopula(data=data3,  
                  method = "ml", optim.method="BFGS", 
                  copula=normalCopula(dim=3), start=0.5)
ft2 = fitCopula(data=data3,
                method = "ml", optim.method="L-BFGS-B", 
                copula=tCopula(dim=3), lower=c(0,2.5),upper=c(.5,15) )
fgumbel = fitCopula(data=data3,
                    method = "ml", 
                    copula=gumbelCopula(dim=3))
ffrank = fitCopula(data=data3,
                   method = "ml", optim.method="BFGS", 
                   copula=frankCopula(dim=3))
fclayton = fitCopula(data=data3,
                     method = "ml", optim.method="BFGS", 
                     copula=claytonCopula(dim=3))

AICfit = round(cbind(AIC(fnorm),AIC(ft2),AIC(fgumbel),AIC(ffrank),AIC(fclayton)),2)
colnames(AICfit) = c('Normal', 'T', 'Gumbel', 'Frank', 'Clayton') 
AICfit

BICfit = round(cbind(BIC(fnorm),BIC(ft2),BIC(fgumbel),BIC(ffrank),BIC(fclayton)),2)
colnames(BICfit) = c('Normal', 'T', 'Gumbel', 'Frank', 'Clayton') 
BICfit