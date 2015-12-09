library(quantmod)
library(xts)

## specify the path for the Fama-French database
dir_FF_db = "W:/Data/F-F"

## specify the output directory for figures
dir_figures = paste(dir_FF_db, "figures", sep="/")

## set the working directory
setwd(dir_FF_db)

## get the names of the folders and put into "directory" variable
directory = dir()

## go to the folder of daily data
setwd(paste(dir_FF_db, "F-F_Research_Data_Factors_daily", sep="/"))

## get the name of the file
filename = dir()

## read the file and write it into a table
data_daily = read.csv(filename, skip=4, sep="", stringsAsFactors=F)

## get the date info and make it a timestamp object
timestamp = rownames(data_daily)
year_daily = substr(timestamp,start=1,stop=4)
mon_daily = substr(timestamp, start=5,stop=6)
day_daily = substr(timestamp, start=7,stop=8)
timestamp = paste(year_daily, mon_daily, day_daily, sep="-")
timestamp = as.Date(timestamp, format='%Y-%m-%d')  ## use "as.Date" in package "xts"

## rename the column name of the daily data
names(data_daily) = c("Mkt.RF.daily", "SMB.daily", "HML.daily", "RF.daily")

## use "xts" function to create an irregular T-S object
data_daily = xts(data_daily, order.by=timestamp)

## plot the factors on daily basis (US market premium, SMB, HML)
dir_temp = getwd(); setwd(dir_figures);
data_daily_s = data_daily[grep("2007-01-03",timestamp):length(timestamp), ]
#pdf("US_daily.pdf", width=12, height=9)
par(mfrow=c(2,2))
plot(data_daily_s[,1], main="Market Premium")
plot(data_daily_s[,2], main="SMB")
plot(data_daily_s[,3], main="HML")
plot(data_daily_s[,4], main="risk-free rate")
dev.off()
setwd(dir_temp)
