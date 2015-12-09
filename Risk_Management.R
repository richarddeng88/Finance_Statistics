library(quantmod)
library(xts)
library(timeDate)
library(MASS)

## specify the output directory for figures
dir_figures = paste(dir_RM_db, "figures", sep="/")

## set the working directory and load dependent scripts
source('Finance_Statistics/uni/VaR_unit.R')
source('Finance_Statistics/uni/ES_unit.R')

## load the GTAT data
load("Finance_Statistics/GTAT.RData")
price = GTAT[,'GTAT.Close']

plot(price, main='price dynamics', ylab='$/share')

## plot the 1-day PnL assuming long 1 unit share
plot(-diff(price), main='1-day Loss (-PnL for longing 1 share)', ylab='$/share')

windowsize = 125
alpha = 0.01
k = 1

#####################
### plot the VaR  ###
#####################

VaR1 = VaR_unit(price, windowsize, k, alpha, 'Normal')
VaR2 = VaR_unit(price, windowsize, k, alpha, 'Lognormal')
VaR3 = VaR_unit(price, windowsize, k, alpha, 'T')
VaR4 = VaR_unit(price, windowsize, k, alpha, 'Empirical')

par(mfrow=c(1,1))

plot(-diff(price), main='1-day LOSS (-PnL for longing 1 share)',
     ylab='$/share')
lines(VaR1[,'VaR_long'], type="l", col='red')
lines(VaR2[,'VaR_long'], type="l", col='blue')
lines(VaR3[,'VaR_long'], type="l", col='brown')
lines(VaR4[,'VaR_long'], type="l", col='green')
legend('topleft', lty = c(1,1,1,1), col = c('red','blue','brown','green'), 
       c('VaR_normal','VaR_lognormal','VaR_T','VaR_empirical'), bty='n')

plot(diff(price), main='1-day LOSS (-PnL for shorting 1 share)',
     ylab='$/share')
lines(VaR1[,'VaR_short'], type="l", col='red')
lines(VaR2[,'VaR_short'], type="l", col='blue')
lines(VaR3[,'VaR_short'], type="l", col='brown')
lines(VaR4[,'VaR_short'], type="l", col='green')
legend('bottomleft', lty = c(1,1,1,1), col = c('red','blue','brown','green'), 
       c('VaR_normal','VaR_lognormal','VaR_T','VaR_empirical'), bty='n')

###################
### plot the ES ###
###################

ES1 = ES_unit(price, windowsize, k, alpha, 'Normal')
ES2 = ES_unit(price, windowsize, k, alpha, 'Lognormal')
ES3 = ES_unit(price, windowsize, k, alpha, 'T')
ES4 = ES_unit(price, windowsize, k, alpha, 'Empirical')

par(mfrow=c(1,1))

PnL_long = (-diff(price))
PnL_long = PnL_long[index(VaR4[,'VaR_long'])]
extreme_loss_long = (PnL_long > VaR4[,'VaR_long'])*PnL_long
extreme_loss_long = extreme_loss_long[extreme_loss_long>0]

PnL_short = (diff(price))[index(VaR4[,'VaR_short'])]
extreme_loss_short = (PnL_short > VaR4[,'VaR_short'])*PnL_short
extreme_loss_short = extreme_loss_short[extreme_loss_short>0]

plot.xts(-diff(price), main='1-day LOSS (-PnL for longing 1 share)',
     ylab='$/share')
o1=points(extreme_loss_long, col='purple')
lines(ES1[,'ES_long'], type="l", col='red')
lines(ES2[,'ES_long'], type="l", col='blue')
lines(ES3[,'ES_long'], type="l", col='brown')
lines(ES4[,'ES_long'], type="l", col='green')
legend('topleft', pch = c('-','-','-','-','o'), 
       col = c('red','blue','brown','green','purple'), 
       c('ES_normal','ES_lognormal','ES_T','ES_empirical','extreme loss'), 
       bty='n')

plot.xts(diff(price), main='1-day LOSS (-PnL for shorting 1 share)',
     ylab='$/share')
points(extreme_loss_short, col='purple')
lines(ES1[,'ES_short'], type="l", col='red')
lines(ES2[,'ES_short'], type="l", col='blue')
lines(ES3[,'ES_short'], type="l", col='brown')
lines(ES4[,'ES_short'], type="l", col='green')
legend('bottomleft', pch = c('-','-','-','-','o'), 
       col = c('red','blue','brown','green','purple'), 
       c('ES_normal','ES_lognormal','ES_T','ES_empirical','extreme loss'), 
       bty='n')

