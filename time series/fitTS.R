######################### Get/Set working directory ###########################

# Get working directory, where your scripts, data, output files are saved
getwd() 
# Name my working directory, an example
myWD = "E:/commodities"
# Set my working directory as myWD, an example
setwd(myWD)

#################### Load S&P500 Stocks data #######################
library(quantmod)
load("SP500 company data 2008.RData")
load("SP500 company dailyReturn 2008.RData")

#################### Fitting multivariate TS data #######################

library(fGarch)
library(MASS)     

ATT_Ret = SP500dailyReturn[46,]
VZ_Ret  = SP500dailyReturn[468,]

# Use ATT_Ret and VZ_Ret, which are stationary

Odata1 = cbind(ATT_Ret, VZ_Ret)
Oname1 = colnames(Odata1)
Odata1 = cbind(ATT_Ret-mean(ATT_Ret), VZ_Ret-mean(VZ_Ret))

mydates = as.Date(rownames(Odata1))
Ndays = length(mydates)

#png("SectorsTS2.png", width=768, height=1024)
par(mfrow=c(2,1))
for (i in 1:2){
  subTS = zoo(Odata1[,i], mydates)
  subTS = xts(subTS, index(subTS))
  plot(subTS, main="daily stock returns",ylab=Oname1[i],mai=c(-3,0,-3,0))  
}
#dev.off()
par(mfrow=c(1,1))

library(tseries)
library(forecast)

par(mfrow=c(2,1))
acf(Odata1[,1], main="ACF plot of return on ATT", lag.max=10)
pacf(Odata1[,1], main="PACF plot of return on ATT", lag.max=10)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
acf(Odata1[,2], main="ACF plot of return on VZ", lag.max=10)
pacf(Odata1[,2], main="PACF plot of return on VZ", lag.max=10)
par(mfrow=c(1,1))

att_model_1 = auto.arima(Odata1[,1], max.p=10, max.q=10, stationary=T)
att_model_1 = arima(Odata1[,1], order=c(0,0,2))
att_model_1 = arima0(Odata1[,1], order=c(0,0,2))
#att_model_1$coef
#att_model_1$sigma2
#(1+sum((att_model_1$coef)^2))*att_model_1$sigma2

vz_model_1 = auto.arima(Odata1[,2], max.p=10, max.q=10, stationary=T)
vz_model_1 = arima(Odata1[,2], order=c(0,0,2))
#vz_model_1$coef
#vz_model_1$sigma2
#(1+sum((vz_model_1$coef)^2))*vz_model_1$sigma2

par(mfrow=c(2,1))
acf(att_model_1$residuals, main="ACF plot of ARMA residuals for ATT", lag.max=10)
pacf(att_model_1$residuals, main="PACF plot of ARMA residuals for ATT", lag.max=10)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
acf((att_model_1$residuals)^2, main="ACF plot of squared ARMA residuals for ATT", lag.max=10)
pacf((att_model_1$residuals)^2, main="PACF plot of squared ARMA residuals for ATT", lag.max=10)
par(mfrow=c(1,1))

att_R_1 = att_model_1$residuals

par(mfrow=c(2,1))
acf(vz_model_1$residuals, main="ACF plot of ARMA residuals for VZ", lag.max=10)
pacf(vz_model_1$residuals, main="PACF plot of ARMA residuals for VZ", lag.max=10)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
acf((vz_model_1$residuals)^2, main="ACF plot of squared ARMA residuals for VZ", lag.max=10)
pacf((vz_model_1$residuals)^2, main="PACF plot of squared ARMA residuals for vZ", lag.max=10)
par(mfrow=c(1,1))

vz_R_1 = vz_model_1$residuals

library(fGarch)

att_model_g_1 = garchFit(att_R_1~garch(1,1),trace=F)
att_GR_1 = att_model_g_1@residuals
par(mfrow=c(2,1))
acf(att_GR_1, main="ACF plot of GARCH residuals for ATT", lag.max=10)
pacf(att_GR_1, main="PACF plot of GARCH residuals for ATT", lag.max=10)
par(mfrow=c(1,1))

vz_model_g_1 = garchFit(vz_R_1~garch(1,1),trace=F)
vz_GR_1 = vz_model_g_1@residuals
par(mfrow=c(2,1))
acf(vz_GR_1, main="ACF plot of GARCH residuals for VZ", lag.max=10)
pacf(vz_GR_1, main="PACF plot of GARCH residuals for VZ", lag.max=10)
par(mfrow=c(1,1))

###############################
###    Simulate the TS      ###
###############################

#################################################
### A NEW SIMULATION FUNCTION for ARIMA model ###
### Author: Junyi Zhang                       ###
### REMARK: the original funciton arima.sim   ###
###      in package tseries is problematic.   ###
#################################################

arima.sim2 <- function(model, n, rand.gen = rnorm,
                       innov = rand.gen(n, ...), n.start = NA, ...)
{
  if(!is.list(model)) stop("'model' must be list")
  p <- model$arma[1]
  if(p) {
    minroots <- min(Mod(polyroot(c(1, -model$coef[1:p]))))
    if(minroots <= 1) stop("'ar' part of model is not stationary")
  }
  q <- model$arma[2]
  if(is.na(n.start)) n.start <- p + q +
    ifelse(p > 0, ceiling(6/log(minroots)), 0)
  if(n.start < p + q) stop("burn-in 'n.start' must be as long as 'ar + ma'")
  d <- 0
  if(!is.null(ord <- model$order)) {
    if(length(ord) != 3) stop("'model$order' must be of length 3")
    if(p != ord[1]) stop("inconsistent specification of 'ar' order")
    if(q != ord[3]) stop("inconsistent specification of 'ma' order")
    d <- ord[2]
    if(d != round(d) || d < 0)
      stop("number of differences must be a positive integer")
  }
  x <- ts(c(rand.gen(n.start, ...), innov[1:n]), start = 1 - n.start)*sqrt(model$sigma2)
  if(q) x <- filter(x, c(1, model$coef[(p+1):(p+q)]), sides = 1)
  if(p) x <- filter(x, model$coef[1:p], method = "recursive")
  if(n.start > 0) x <- x[-(1:n.start)]
  if(d > 0) x <- diffinv(x, differences = d)
  as.ts(x)
}

###########################################################
### ATT: simulate the innovations by using white noise  ###
###########################################################

att_sim1 = arima.sim2(att_model_1, n=Ndays, rand.gen=rnorm, n.start=300)

par(mfrow=c(2,1))
subTS2 = zoo(Odata1[,1], mydates)
subTS2 = xts(subTS2, index(subTS2))
plot(subTS2, main="Observed",ylab=Oname1[1],mai=c(-3,0,-3,0))
subTS1 = zoo(att_sim1, mydates)
subTS1 = xts(subTS1, index(subTS1))
plot(subTS1, main="Simulated by MA(2)",ylab="simulated ATT_Ret",mai=c(-3,0,-3,0))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(Odata1[,1],main=c("Normal Q-Q Plot","Observed: lots of Outliers"))
qqline(Odata1[,1],lwd=2,col="red")
qqnorm(att_sim1,main=c("Normal Q-Q Plot","Simulated by MA(2)"))
qqline(att_sim1,lwd=2,col="red")
par(mfrow=c(1,1))

#####################################################
### ATT: simulate the innovations by GARCH model  ###
#####################################################

names(att_model_g_1@fit)
att_condVar_1 = filter((att_R_1-att_model_g_1@fit$coef[1])^2,att_model_g_1@fit$coef[2:3])
att_condVar_1 = filter(att_condVar_1,att_model_g_1@fit$coef[4],method = "recursive")
att_innov_1 = sqrt(att_condVar_1)*rnorm(Ndays)+att_model_g_1@fit$coef[1]
att_sim2 = arima.sim2(att_model_1, n=Ndays, innov=att_innov_1/sqrt(att_model_1$sigma2), n.start=300)

par(mfrow=c(2,1))
subTS2 = zoo(Odata1[,1], mydates)
subTS2 = xts(subTS2, index(subTS2))
plot(subTS2, main="Observed",ylab=Oname1[1],mai=c(-3,0,-3,0))
subTS1 = zoo(att_sim2, mydates)
subTS1 = xts(subTS1, index(subTS1))
plot(subTS1, main="Simulated by MA(2)+GARCH(1,1)",ylab="simulated ATT_Ret",mai=c(-3,0,-3,0))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(Odata1[,1],main=c("Normal Q-Q Plot","Observed: lots of Outliers"))
qqline(Odata1[,1],lwd=2,col="red")
qqnorm(att_sim2,main=c("Normal Q-Q Plot","Simulated by MA+GARCH"))
qqline(att_sim2,lwd=2,col="red")
par(mfrow=c(1,1))

#########################################################
### VZ: simulate the innovations by using white noise ###
#########################################################

vz_sim1 = arima.sim2(vz_model_1, n=Ndays, rand.gen=rnorm, n.start=300)

par(mfrow=c(2,1))
subTS2 = zoo(Odata1[,2], mydates)
subTS2 = xts(subTS2, index(subTS2))
plot(subTS2, main="Observed",ylab=Oname1[2],mai=c(-3,0,-3,0))
subTS1 = zoo(vz_sim1, mydates)
subTS1 = xts(subTS1, index(subTS1))
plot(subTS1, main="Simulated by MA(2)",ylab="simulated VZ_Ret",mai=c(-3,0,-3,0))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(Odata1[,2],main=c("Normal Q-Q Plot","Observed: lots of Outliers"))
qqline(Odata1[,2],lwd=2,col="red")
qqnorm(vz_sim1,main=c("Normal Q-Q Plot","Simulated by MA(2)"))
qqline(vz_sim1,lwd=2,col="red")
par(mfrow=c(1,1))

###################################################
### VZ: simulate the innovations by GARCH model ###
###################################################

names(vz_model_g_1@fit)
vz_condVar_1 = filter((vz_R_1-vz_model_g_1@fit$coef[1])^2,vz_model_g_1@fit$coef[2:3])
vz_condVar_1 = filter(vz_condVar_1,vz_model_g_1@fit$coef[4],method = "recursive")
vz_innov_1 = sqrt(vz_condVar_1)*rnorm(Ndays)+vz_model_g_1@fit$coef[1]
vz_sim2 = arima.sim2(vz_model_1, n=Ndays, innov=vz_innov_1/sqrt(vz_model_1$sigma2), n.start=300)

par(mfrow=c(2,1))
subTS2 = zoo(Odata1[,2], mydates)
subTS2 = xts(subTS2, index(subTS2))
plot(subTS2, main="Observed",ylab=Oname1[2],mai=c(-3,0,-3,0))
subTS1 = zoo(vz_sim2, mydates)
subTS1 = xts(subTS1, index(subTS1))
plot(subTS1, main="Simulated by MA(2)+GARCH(1,1)",ylab="simulated VZ_Ret",mai=c(-3,0,-3,0))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(Odata1[,2],main=c("Normal Q-Q Plot","Lots of Outliers"))
qqline(Odata1[,2],lwd=2,col="red")
qqnorm(vz_sim2,main=c("Normal Q-Q Plot","Simulated by MA+GARCH"))
qqline(vz_sim2,lwd=2,col="red")
par(mfrow=c(1,1))
