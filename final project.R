library(quantmod)
stocklist = c('PPG',  'STZ',  'AIG',  'LB'   ,'AXP'  ,'RRC'  ,'ORCL', 'AVB',  'NTRS', 'HIG',  'MAS','STX','KMI','BHI','FFIV', 'PRU',  'WEC'  ,'MAC',"MKC","CMI")

for (i in stocklist) {
    
    x = get(getSymbols(i, src = 'yahoo', from = '2014-12-04', adjust=TRUE))
    xr = dailyReturn(x)
    assign(i, xr)
    
}


dat <- cbind(PPG, STZ,	AIG,	LB,	AXP,	RRC,	ORCL,	AVB,	NTRS,	HIG,	MAS,	STX,	KMI,	BHI,	FFIV,	PRU,	WEC,	MAC,	MKC,	CMI)
dat <- as.data.frame(dat)
names(dat) <- stocklist 

library(MASS)
est_mean <- apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[1]})
est_sd <-  apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[2]})

data1 <- matrix(0,253,20)
for (i in 1:20){
    data1[,i] <- pnorm(dat[,i], mean=est_mean[i], sd=est_sd[i])
}

################################ placeholder ######################

library(copula)   # for copula functions

### Fit Copula based on data1
fnorm = fitCopula(data=data1,  
                  method = "ml", optim.method="BFGS", 
                  copula=normalCopula(-0.3, dim=20), start=0)

copula <- normalCopula(-0.3, dim=20)

### Estimating df parameter for t-copula
cor_tau = cor(data1, method="kendall")
t_omega = sin(pi*cor_tau/2)
cop_t_dim2 = tCopula(t_omega, dim = 20, dispstr = "un", df = 190)

ft2 = fitCopula(cop_t_dim2, data=data1,
                method = "ml", optim.method="L-BFGS-B", 
                start=c(t_omega,5),lower=c(0,2.5),upper=c(.5,15) )

ncopula <- normalCopula(dim=20, param = c())

########## Best Copula ###############
bestcopula <- fnorm

########## VaR ########################
mean1 <- apply(data1,2, mean)
sd1 <- apply(data1,2,sd)
list <-   list(list(mean= mean1[1],sd=sd1[1]),
               list(mean= mean1[2], sd=sd1[2]),
               list(mean= mean1[3], sd=sd1[3]),
               list(mean= mean1[4], sd=sd1[4]),
               list(mean= mean1[5], sd=sd1[5]),
               list(mean= mean1[6], sd=sd1[6]),
               list(mean= mean1[7], sd=sd1[7]),
               list(mean= mean1[8], sd=sd1[8]),
               list(mean= mean1[9], sd=sd1[9]),
               list(mean= mean1[10], sd=sd1[10]),
               list(mean= mean1[11], sd=sd1[11]),
               list(mean= mean1[12], sd=sd1[12]),
               list(mean= mean1[13], sd=sd1[13]),
               list(mean= mean1[14], sd=sd1[14]),
               list(mean= mean1[15], sd=sd1[15]),
               list(mean= mean1[16], sd=sd1[16]),
               list(mean= mean1[17], sd=sd1[17]),
               list(mean= mean1[18], sd=sd1[18]),
               list(mean= mean1[19], sd=sd1[19]),
               list(mean= mean1[20], sd=sd1[20]))


mvdc_norm <- mvdc(copula = copula, margins = rep("norm", 20), paramMargins = list)

set.seed(2015)

rand_mvdc <-rMvdc(n=10, mvdc=mvdc_norm)

pairs(rand_mvdc)
    
    
    
    ####################### Equal Weighted Portfolio ########################
    
    meanRet <- mean(apply(dat, 2, mean))
    sdRet <- sqrt(sum(apply(dat, 2, sd)^2)/400)
    
    ####################  Investiment $1000,000 ###########################
    invest = 1000000
    VaR5perc.port <- invest*(-meanRet+1.645*sdRet)
    ES5per.port <- invest*(-meanRet+0.103*sdRet/0.05)
    
    
    