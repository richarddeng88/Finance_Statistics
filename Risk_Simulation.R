library(quantmod)
library(xts)
library(timeDate)
library(MASS)


## specify the output directory for figures
dir_figures = paste(dir_RM_db, "figures", sep="/")

## set the working directory and load dependent scripts
source('Finance_Statistics/uni/VaR_unit_boot.R')
source('Finance_Statistics/uni/VaR_unit.R')
source('Finance_Statistics/uni/ES_unit.R')
#source('ES_unit_boot.R')

## get the name of the file
filename = dir()

## load the GTAT data
load("Finance_Statistics/GTAT.RData")
price = GTAT[,'GTAT.Close']

plot(price, main='price dynamics', ylab='$/share')

## plot the 1-day PnL assuming long 1 unit share
plot(-diff(price), main='1-day Loss (-PnL for longing 1 share)',
     ylab='$/share')

windowsize = 125
alpha = 0.01
k = 1

#######################################################
### plot the VaR and Boostrapped Upper Bound of VaR ###
#######################################################

B = 500
beta = 0.05

t_start = proc.time()
VaR1 = VaR_unit_boot(price, windowsize, k, alpha, 'Normal', B, beta)
t_end = proc.time()
t_start = proc.time()
VaR2 = VaR_unit_boot(price, windowsize, k, alpha, 'Empirical', B, beta)
t_end = proc.time()

par(mfrow=c(1,1))

plot(-diff(price), main='1-day LOSS (-PnL for longing 1 share)',
     ylab='$/share')
lines(VaR1[,'VaR_long'], type="l", lty=1, col='red')
lines(VaR1[,'VaR_long_Upper'], type="l", lty=2, col='red')
lines(VaR2[,'VaR_long'], type="l", lty=1, col='green')
lines(VaR2[,'VaR_long_Upper'], type="l", lty=2, col='green')
legend('topleft', lty = c(1,2,1,2), col = c('red','red','green','green'), 
       c('VaR_normal_Boostrapped mean','VaR_normal_Bootstrapped Upper Bound',
         'VaR_Empirical_Boostrapped mean','VaR_Empirical_Boostrapped Upper Bound'), 
       bty='n')

#############################################
### Standard MC and Importance Sampling   ###
#############################################
S = 500
VaR1 = VaR_unit(price, windowsize, k, alpha, 'Normal')
ES1 = ES_unit(price, windowsize, k, alpha, 'Normal')

ret = dailyReturn(price)[-1]
N = length(ret)
e=N
temp_ret = ret[(e-windowsize+1):e]
n=length(temp_ret)

# fit the returns by the normal model
mean_ret = mean(temp_ret)
sd_ret = sd(as.vector(temp_ret))

# Simulate S replicates of returns series and 
# compute the standard MC estimator for ES
ES_long = rep(NA, S)
for (s in 1:S){
  R.sim=qnorm(runif(n),mean=mean_ret,sd=sd_ret)
  indi_long = R.sim <= quantile(R.sim, alpha)
  if (sum(indi_long)>0){
    ES_long[s] = price[e+1] *
      (-sum(indi_long*R.sim)/sum(indi_long))        
  }else{ES_long[s]=0}
}
hist(ES_long, main='Histogram of Simulated ES by Standard MC')
abline(v=mean(ES_long), col='red', lwd=2)
abline(v=quantile(ES_long, 0.025), col='red', lty=2, lwd=2)
abline(v=quantile(ES_long, 0.975), col='red', lty=2, lwd=2)
abline(v=tail(ES1[,'ES_long'],1), col='blue', lty=1, lwd=2)
legend('topright', pch = c('-','-'), 
       col = c('red','blue'), 
       c('MC','Formula'), bty='n')

### Importance Sampling by G=NORM(mean=mean_ret+qnorm(alpha)*sd_ret, sd=sd_ret)
### Remark: the distribution G used in this IS procedure is the same as F except
###         the mean return is shifted by qnorm(alpha) times return's S.D.

mean_ret_shift = mean_ret + qnorm(alpha)*sd_ret
hist(rnorm(1000, mean=mean_ret, sd=sd_ret), col='grey', freq=F,
     xlim=c(mean_ret-10*sd_ret, mean_ret+6*sd_ret), xlab='Ri',
     main='compare F and G')
hist(rnorm(1000, mean=mean_ret_shift, sd=sd_ret), add=T, freq=F)
abline(v=mean_ret_shift, col='blue', lty=1, lwd=2)

ES_long_IS = rep(NA, S)
VaR_long_IS = rep(NA, S)
for (s in 1:S){
  R.sim=qnorm(runif(n),mean=mean_ret_shift,sd=sd_ret)
  R.sim.sort = sort(R.sim)
  omega.sim = dnorm(R.sim.sort,mean=mean_ret,sd=sd_ret)/
    dnorm(R.sim.sort,mean=mean_ret_shift,sd=sd_ret)/n  ## compute the likelihood ratios divided by sample size n
  if (sum(omega.sim)>=alpha){
    k.sim = min(index(R.sim)[(cumsum(omega.sim)-alpha)>=0]) ## get minimum index k such that k=inf{m: omega.sim[1]+...+omega.sim[m]>=alpha}
  }else{k.sim = n}
  cutoff = R.sim.sort[k.sim]
  indi_long = R.sim.sort < cutoff
  ES_long_IS[s]= - price[e+1]*
                  ( sum(omega.sim*R.sim.sort*indi_long)+
                   (alpha-sum(omega.sim*indi_long))*cutoff ) / alpha
  VaR_long_IS[s]=-price[e+1]*cutoff
}
hist(ES_long_IS, main='Histogram of Simulated ES by Importance Sampling')
abline(v=mean(ES_long_IS), col='red', lwd=2)
abline(v=quantile(ES_long_IS, 0.025), col='red', lty=2, lwd=2)
abline(v=quantile(ES_long_IS, 0.975), col='red', lty=2, lwd=2)
abline(v=tail(ES1[,'ES_long'],1), col='blue', lty=1, lwd=2)
legend('topright', pch = c('-','-'), 
       col = c('red','blue'), 
       c('IS','Formula'), bty='n')

