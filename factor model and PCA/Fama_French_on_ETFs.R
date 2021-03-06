## specify the path for the Fama-French database
dir_FF_db = "W:/Data/F-F"

## specify the output directory for figures
dir_figures = paste(dir_FF_db, "figures", sep="/")

## go to the folder of daily data
setwd(paste(dir_FF_db, "F-F_Research_Data_Factors_daily", sep="/"))

## get the name of the file
filename = dir()

## read the file and write it into a table
data_daily = read.csv(filename, skip=4, sep="	")

## specify the directory of source code
dir_code = "W:/Rcode/Utils"
source(paste(dir_code, "Fama_French_research_daily.R", sep="/"))

#################### Download Sector ETFs #######################
library(quantmod)  # build library for package 'quantmod', which has been installed  
op <- options(warn = (-1)) # suppress warnings

# use function 'getSymbols' in quantmod and the tick name to download price info 
XLB = get(getSymbols('XLB', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLE = get(getSymbols('XLE', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLF = get(getSymbols('XLF', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLI = get(getSymbols('XLI', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLK = get(getSymbols('XLK', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLP = get(getSymbols('XLP', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLU = get(getSymbols('XLU', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLV = get(getSymbols('XLV', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))
XLY = get(getSymbols('XLY', from = "2007-01-03", to="2014-07-31", src = 'yahoo', adjust=TRUE))

XLB_ret = dailyReturn(XLB)*252
XLE_ret = dailyReturn(XLE)*252
XLF_ret = dailyReturn(XLF)*252
XLI_ret = dailyReturn(XLI)*252
XLK_ret = dailyReturn(XLK)*252
XLP_ret = dailyReturn(XLP)*252
XLU_ret = dailyReturn(XLU)*252
XLV_ret = dailyReturn(XLV)*252
XLY_ret = dailyReturn(XLY)*252

sectorETF = cbind(XLB_ret, XLE_ret, XLF_ret, XLI_ret, XLK_ret, XLP_ret,
	XLU_ret, XLV_ret, XLY_ret)

names(sectorETF)=c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")
sectornames = c("Basic Materials", "Energy", "Financial", "Industrial",
			"Technology", "Consumer Staples", "Utilities", "Healthcare", "Consumer Discretionary")

###############################################################################
###  Fit a single 3-factor model for XLF by using all data, as an example	#
###############################################################################

### fit the CAPM model (one-factor model)

cor(data_daily_s[,"Mkt.RF.daily"],XLF_ret)
plot(XLF_ret)

model01 = lm(XLF_ret~data_daily_s[,"Mkt.RF.daily"])
residuals01 = model01$residuals
plot(residuals01)
summary(model01)$r.squared
summary(model01)$adj.r.squared

### residual checking for one-factor (CAPM) model

cor(residuals01, as.vector(data_daily_s[,"SMB.daily"]))   ### r= -0.1172
cor(residuals01, as.vector(data_daily_s[,"HML.daily"]))   ### r= 0.6286
### pairs plot for residuals of CAPM against Fama-French 3 factors
dir_temp = getwd(); setwd(dir_figures);
pdf("pairs_plot_XLF_CAPMresi_vs_3_F.pdf", width=12, height=9)
pairs(data.frame(residuals01,data_daily_s[,c("Mkt.RF.daily","SMB.daily","HML.daily")]))
dev.off()
setwd(dir_temp)

### pairs plot for Fama French 3 factors
dir_temp = getwd(); setwd(dir_figures);
pdf("pairs_plot_3_F.pdf", width=12, height=9)
pairs(data.frame(data_daily_s[,c("Mkt.RF.daily","SMB.daily","HML.daily")]))
dev.off()
setwd(dir_temp)

### fit the Fama-French 3-factor models

model02 = lm(XLF_ret~data_daily_s[,c("Mkt.RF.daily","SMB.daily","HML.daily")])
residuals02 = model02$residuals
plot(residuals02)
summary(model02)$r.squared
summary(model02)$adj.r.squared

### residual plots against time
dir_temp = getwd(); setwd(dir_figures);
pdf("residual_plot_CAPM_and_FF.pdf", width=12, height=9)
par(mfrow=c(2,1))
plot(residuals01,main="CAPM residuals for XLF")
legend(x="topright", 
	legend=c("adjusted R2=", 
		   round(summary(model01)$adj.r.squared,3)),
      bty="n")
plot(residuals02,main="FAMA-FRENCH residuals for XLF")
legend(x="topright", 
	legend=c("adjusted R2=", 
		   round(summary(model02)$adj.r.squared,3)),
      bty="n")
dev.off()
setwd(dir_temp)

###########################################################
### Fit the Fama-French factor model on a monthly basis ###
###                    Sept 16, 2014, by Junyi Zhang    ###
###########################################################

timestamp = index(XLF_ret)
month = substr(timestamp, start=1, stop=7)
month.unique = unique(month)
nperiod = length(month.unique)
nETF = dim(sectorETF)[2]
newdata = cbind(data_daily_s, sectorETF)

beta.M = matrix(NA, nrow=nperiod, ncol=nETF)
beta.SMB = matrix(NA, nrow=nperiod, ncol=nETF)
beta.HML = matrix(NA, nrow=nperiod, ncol=nETF)
r.squared = matrix(NA, nrow=nperiod, ncol=nETF) 
colnames(beta.M) = colnames(beta.SMB) = colnames(beta.HML) = colnames(r.squared) = names(sectorETF)

for (i in 1:nperiod){
  
  select = grep(month.unique[i],month)
  		
  model03 = lm(newdata[select,names(sectorETF)]~newdata[select,c("Mkt.RF.daily","SMB.daily","HML.daily")])
  beta.M[i,] = model03$coefficients[2,]
  beta.SMB[i,] = model03$coefficients[3,]
  beta.HML[i,] = model03$coefficients[4,]

  for (j in 1:nETF) 
    r.squared[i,j] = summary(model03)[[j]]$r.squared
}

beta.M = xts(beta.M, order.by=as.yearmon(month.unique))
beta.SMB = xts(beta.SMB, order.by=as.yearmon(month.unique))
beta.HML = xts(beta.HML, order.by=as.yearmon(month.unique))
r.squared = xts(r.squared, order.by=as.yearmon(month.unique))

dir_temp = getwd(); setwd(dir_figures);

pdf("sector_beta_M.pdf", width=12, height=9)
par(mfrow=c(3,3))
for (j in 1:nETF) plot(beta.M[,j], main=sectornames[j])
dev.off()

pdf("sector_beta_SMB.pdf", width=12, height=9)
par(mfrow=c(3,3))
for (j in 1:nETF) plot(beta.SMB[,j], main=sectornames[j])
dev.off()

pdf("sector_beta_HML.pdf", width=12, height=9)
par(mfrow=c(3,3))
for (j in 1:nETF) plot(beta.HML[,j], main=sectornames[j])
dev.off()

pdf("sector_R_squared.pdf", width=12, height=9)
par(mfrow=c(3,3))
for (j in 1:nETF) plot(r.squared[,j], main=sectornames[j])
dev.off()

setwd(dir_temp)

