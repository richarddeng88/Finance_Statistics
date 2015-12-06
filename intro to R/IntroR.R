######################### Get/Set/Show working directory ###########################

# Get working directory, where your scripts, data, output files are saved
getwd() 

# Name my working directory, an example
myWD = "W:/Data/IntroR"

# Set my working directory as myWD, an example
setwd(myWD)

# Show the names of the files, folders under the directory 'myWD', an exmaple
dir(myWD)

################################ Open/Save Data ####################################

# Open *.csv files and read the data into the object mydata by function read.table, an example
mydata <- read.table("YahooEnergy.csv", header=T, sep=",")

# show the first row of mydata
mydata[1:10,]

# Creat a new data object which contains the first 30 rows of mydata 
newdata <- mydata[1:30,]

# get the colume names of mydata
colnames(newdata)
# rename the columes
colnames(newdata) <- c("Symbol","Name","P","V","Hi","Lo","Date","Time","PP")

# Save the newdata in a CSV file under the current working directory, an example
write.table(newdata, "newdata.csv", row.names=F, col.names=T)

#################### Download Stock Data #######################
library(quantmod)  # build library for package 'quantmod', which has been installed  
op <- options(warn = (-1)) # suppress warnings

# use function 'getSymbols' in quantmod and the tick name to download price info 
SPY = get(getSymbols('SPY', src = 'yahoo', from = '2007-01-01', adjust=TRUE))

SPY[1:5,]

chartSeries(SPY, subset='last 4 months') # candlestick plot

dailyRetSPY = dailyReturn(SPY) # calculate daily returns
weeklyRetSPY = weeklyReturn(SPY) # calculate weekly returns

plot(dailyRetSPY)
plot(weeklyRetSPY)

#################### Download Sector ETFs #######################
library(quantmod)  # build library for package 'quantmod', which has been installed  
op <- options(warn = (-1)) # suppress warnings

# use function 'getSymbols' in quantmod and the tick name to download price info 
XLB = get(getSymbols('XLB', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLE = get(getSymbols('XLE', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLF = get(getSymbols('XLF', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLI = get(getSymbols('XLI', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLK = get(getSymbols('XLK', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLP = get(getSymbols('XLP', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLU = get(getSymbols('XLU', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLV = get(getSymbols('XLV', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))
XLY = get(getSymbols('XLY', src = 'yahoo', from = '2010-01-01', to="2013-03-19", adjust=TRUE))

save(XLB, XLE, XLF, XLI, XLK, XLP, XLU, XLV, XLY, file="Sector ETFs 2010-2013.RData")

###################### Download S&P500 Stocks data #######################
 library(RQuantLib)
 library(quantmod)
 
 SPcompany = read.csv("SP500 companies.csv", header=T, sep=",")
 colnames(SPcompany)
 table(SPcompany[,"GICS.Sector"])  # summary of the # of companies in each sector
 
 Tickers   = as.vector(SPcompany[,1])  # get the 500 tickers
 Tickers[which(Tickers=="BRK.B")] = "BRK-B"  # Yahoo finance use "BRK-B" instead of "BRK.B"
 Tickers[which(Tickers=="BF.B")] = "BF-B"  # Yahoo finance use "BF-B" instead of "BF.B"
 
 #start = "2013-01-02"
 start = "2006-01-02"
 from  = as.Date(start, order="ymd")
 to    = Sys.Date()
 #Ndays = businessDaysBetween("UnitedStates", from, to)  # number of business days
 data = get(getSymbols("QQQ", from = from, adjust=TRUE))
 Ndays = length(data[,1])
 dates = index(data)
 
 # Initilization
 Open = rep(NA, Ndays); Low = rep(NA, Ndays) ;High = rep(NA, Ndays); Close = rep(NA, Ndays); Volume = rep(NA, Ndays)
 
 # get data
 data = get(getSymbols("QQQ", from = from, adjust=TRUE))
 temp0 = 0*t(data[,1])
 for (i in 1:dim(SPcompany)[1])
 {  
   data = get(getSymbols(Tickers[i], from = from, adjust=TRUE))
   data = data[dates,]
   temp   = temp0;temp[Ndays-length(data[,1])+1:Ndays]=t(data[,1])
   Open   = rbind(Open, temp);
   temp   = temp0;temp[Ndays-length(data[,1])+1:Ndays]=t(data[,2])
   High   = rbind(High, temp);
   temp   = temp0;temp[Ndays-length(data[,1])+1:Ndays]=t(data[,3])
   Low    = rbind(Low, temp);
   temp   = temp0;temp[Ndays-length(data[,1])+1:Ndays]=t(data[,4])
   Close  = rbind(Close, temp);
   temp   = temp0;temp[Ndays-length(data[,1])+1:Ndays]=t(data[,5])
   Volume = rbind(Volume, temp);
 }
 Open=Open[-1,];High=High[-1,];Low=Low[-1,];Close=Close[-1,];Volume=Volume[-1,]
 rownames(Open)=t(Tickers);rownames(High)=t(Tickers);rownames(Low)=t(Tickers);rownames(Close)=t(Tickers);rownames(Volume)=t(Tickers)
 
 save(Open, High, Low, Close, Volume, file="SP500 company data 2006.RData")

#################### Load S&P500 Stocks data #######################
library(quantmod)
load("SP500 company dailyReturn 2008.RData")

SPcompany = read.table("SP500 companies.csv", header=T, sep=",")
colnames(SPcompany)
table(SPcompany[,"GICS.Sector"])  # summary of the # of companies in each sector

#  # calculate daily returns for all 500 stocks
# SP500dailyReturn = t(apply(Close, MARGIN=1, function(a) t(dailyReturn(a))))
# SP500dailyReturn = SP500dailyReturn[,-1]
# colnames(SP500dailyReturn) = colnames(Close)[-1]

#################### Compute the Efficient Frontier #######################
library(lattice)
library(MASS)
setwd('W:/RCode/Utils')
source('heatmap.R')
setwd(myWD)

choose <- which(SPcompany[,"GICS.Sector"]=="Telecommunications Services")
choosedates <- which(as.Date(colnames(SP500dailyReturn)) > as.Date('2013-01-01') &
                  as.Date(colnames(SP500dailyReturn)) < as.Date('2013-04-01') ) 

mu = apply(SP500dailyReturn[choose,choosedates], 1, mean)  # calculate mean return

Sigma = var(t(SP500dailyReturn[choose,choosedates]), na.rm=T, 
            use="pairwise.complete.obs")

Sigmainv = ginv(Sigma)

corr  = cor(t(SP500dailyReturn[choose,choosedates]))

ones  = rep(1,length(mu))

myheatmap(corr,-1,1,F)
eigen(corr, only.values=T)

a=t(ones) %*% Sigmainv %*% ones; 
b=t(ones) %*% Sigmainv %*% mu; 
c=t(mu) %*% Sigmainv %*% mu

efficientsigma <- function(mup,a,b,c) {
  return (sqrt(((c-b*mup)+(a*mup-b)*mup)/(a*c-b^2)))
}

mup <- seq(0.00001, 0.008, 0.000005)
sigmap <- sapply(mup, function(t) efficientsigma(t,a,b,c))

png("TeleServicesEfficient.png", width=1024, height=768)
plot(sigmap*sqrt(252), mup*252, col="white", cex.axis=1.5, cex.lab=1.5, cex.main=2, xlim=c(0,0.80),
     xlab="annualized volatility",ylab="annualized return",
     main="Telecomm Services (from 2013-01 to 2013-03)")
lines(sigmap*sqrt(252), mup*252, lwd=2, col="blue")
dev.off()

#################### Compute the Tangent Portfolio #######################

# set the risk-free rate
rf = 0.10

# Solve the Tangent Portfolio
omega0 = Sigmainv %*% (mu*252-rf)
omegaT = omega0/sum(omega0)

muT = t(omegaT) %*% mu *252
sigmaT = sqrt(t(omegaT) %*% Sigma %*% omegaT * 252)  
sharpeT = (muT-rf)/sigmaT

png("TeleServicesTangent.png", width=1024, height=768)
plot(sigmap*sqrt(252), mup*252, col="white", cex.axis=1.5, cex.lab=1.5, cex.main=2, xlim=c(0,0.80),
     xlab="annualized volatility",ylab="annualized return",
     main="Telecomm Services (from 2013-01 to 2013-03)")
lines(sigmap*sqrt(252), mup*252, lwd=2, col="blue")
abline(a=rf, b=sharpeT,lwd=2,col="red"); abline(v=0,lty=2); abline(h=0,lty=2)
points(sigmaT,muT,pch=20, cex=3, col="dark green")
dev.off()
