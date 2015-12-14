library(quantmod)
library(copula)   # for copula functions
library(fGarch)   # need for standardized t density
library(MASS)     # need for fitdistr and kde2d
library(fCopulae) # additional copula functions
library(quantmod)
library(xts)
library(timeDate)

####  choose 20 stocks from S^P 500
    SPcompany = read.table("data/finance_stats/SP500 companies.csv", header=T, sep=",")
    colnames(SPcompany)
    table(SPcompany[,"GICSector"])  
    stocklist = c('AMZN',  'COH',  'EXPE',  'MCD'   ,'NKE'  ,'RL'  ,'CCE', 'PEP',  'AXP', 
                  'AIG',  'BLK','COF','GS','JPM','MCO', 'WFC',  'FDX'  ,'MMM','AAPL','FB')
# consumer discretionary industry:  amazon, coach, expedia, 
# consumer staples industry :       McDonald's, NIKE, Ralph Lauren, coca-cola, Pepsico.inc, 
# financial industry:               American express, AIG, black stone, capital one, goldman sachs group, JPmogan, moody's, wells fargo, 
# industrials:                      Fedex,3M,
# information technology industry : apple, facebook. 

### DATA PREPROCESSING
    par(mfrow=c(4,5))
    for (i in stocklist) {
        
        x = get(getSymbols(i, src = 'yahoo', from = '2014-12-04', adjust=TRUE))
        xr = dailyReturn(x, type = "log")
        plot(xr, main=i)
        #plot(x[,4],main=i)
        assign(i, xr)
    }
    
    dat <- cbind(AMZN,  COH,  EXPE, MCD ,NKE ,RL ,CCE, PEP, AXP, 
                 AIG, BLK,COF,GS,JPM,MCO, WFC, FDX ,MMM,AAPL,FB)
    dat <- as.data.frame(dat)
    names(dat) <- stocklist 

        # heatmap
        library(lattice)
        myheatmap <- function(myMAT,low,high,uselabel){
            colorFun <- colorRampPalette(c("blue","white","red")) 
            
            b <- boxplot(myMAT, plot = FALSE)
            thr <- c(low,high)
            colbins <- 100
            step <- abs(thr[2] - thr[1])/50
            
            myAT <- seq(thr[1], thr[2], step)
            
            myCOLregions <- colorFun(length(myAT))
            
            levelplot(myMAT, at = myAT, col.regions = myCOLregions, 
                      scales=list(x=list(rot=90)), labels=uselabel)
        }
        
        correlation <- cor(dat)
        myheatmap(correlation,-1,1,F)

        # QQ - plot
        for (i in 1:20){
            qqnorm(dat[,2])
        }
        
 
# DATA TRANSFORMATION       
        library(MASS)
        est_mean <- apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[1]})
        est_sd <-  apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[2]})
        
        
        # tranfore the log daily return to fited normal marginal distribution
        data1 <- matrix(0,dim(dat)[1],20)
        for (i in 1:20){
            data1[,i] <- pnorm(dat[,i], mean=est_mean[i], sd=est_sd[i])
        }
            
            # show transformed histgram compared to the original one
            par(mfrow=c(1,2))
            #hist(dat[,1], main="original daily returns",breaks=15,xlab='PPG')
            hist(data1[,1],main="Normal transformed returns",breaks=15,xlab='PPG')
            hist(data1[,2],main="Normal transformed returns",breaks=15,xlab='STZ')
            
            # transfored data histgram
            for (i in 1:20)
            {
                hist(dat[,i], xlab="ori data",main = stocklist[i])
            }
            
    


### MODEL BUILDING -  Fit normalCopula
        fnorm = fitCopula(data=data1[,1:20],  
                          method = "itau", optim.method="BFGS", 
                          copula=normalCopula(dim=20, dispstr="un"))

        ### Estimating df parameter for t-copula
        #cor_tau = cor(data1, method="kendall")
        #t_omega = sin(pi*cor_tau/2)
        #cop_t_dim2 = tCopula(t_omega, dim = 20, dispstr = "un", df = 190)
        
        #ft2 = fitCopula(cop_t_dim2, data=data1,
         #               method = "ml", optim.method="L-BFGS-B", 
          #              start=c(t_omega,5),lower=c(0,2.5),upper=c(.5,15) )
        
        #ncopula <- normalCopula(dim=20, param = c())


########## VaR ########################
mean1 <- apply(dat,2, mean)
sd1 <- apply(dat,2,sd)
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


#mvdc_norm <- mvdc(copula = normalCopula(coef(fnorm),dim=20,dispstr="un"), rep("norm", 4), 
#                  list(list(mean=0.503,sd=0.271),list(mean=0.494,sd=0.276),list(mean=0.500,sd=0.269),
#                       list(mean=.0499,sd=0.277)))

mvdc_norm <- mvdc(copula = normalCopula(coef(fnorm),dim=20,dispstr="un"), rep("norm", 20), list)

## SIMULATING NEW DATA SET :  simulate 10000 daily returns
    set.seed(2015)
    number=10000
    rand_mvdc <- rMvdc(n=number, mvdc=mvdc_norm)
        #pairs(rand_mvdc)
        # simulated data histgram
        for (i in 1:20)
        {
            hist(rand_mvdc[,i], main = stocklist[i])
        }
    
# CALCULATION : 
    # Based on the simulation data, we make a portfolio with each stock equally weighted
    meanRet <- apply(rand_mvdc, 1, mean)
    x <- meanRet[order(meanRet)]
    # assuem we invest $1000,000, what is the Var and ES
    invest = 1000000
    VaR5perc.port <- -invest*x[number*0.05]
    ES5per.port <- -invest*(mean(x[1:number*0.05]))


    ## assume our portofolio has 20 stocks and they are Equally Weighted
    # calculate mean and sd 
    meanRet <- mean(apply(dat, 2, mean))
    sdRet <- sqrt(sum(apply(dat, 2, sd)^2)/400)

    # assuem we invest $1000,000, what is the Var and ES using parametric method 
    invest = 1000000
    VaR5perc.port <- invest*(-meanRet+1.645*sdRet)
    ES5per.port <- invest*(-meanRet+0.103*sdRet/0.05)

