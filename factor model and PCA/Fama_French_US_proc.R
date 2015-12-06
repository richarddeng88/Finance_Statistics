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

## go to the folder of US factors
setwd(paste(dir_FF_db, "F-F_Benchmark_Factors_Monthly", sep="/"))

## get the name of the file
filename = dir()

## read the file and write it into a table
data_us = read.csv(filename, skip=0, sep="	", header=T)

## get the month info and make it a timestamp object
month_us = rownames(data_us)
year_us = substr(month_us,start=1,stop=4)
mon_us = substr(month_us, start=5,stop=6)
month_us = paste(year_us, mon_us, sep="-")
yearmon_us = as.yearmon(month_us)  ## use "as.yearmon" in package "xts"

## rename the column name of the US data
names(data_us) = c("Mkt.RF.US", "SMB.US", "HML.US")

## use "xts" function to create an irregular T-S object
data_us = xts(data_us, order.by=yearmon_us)

## plot the US factors (US market premium, US SMB, US HML)
data_us_90jul = data_us[grep("Jul 1990", yearmon_us):length(yearmon_us),]
dir_temp = getwd(); setwd(dir_figures);
pdf("US_factors.pdf", width=12, height=9)
par(mfrow=c(2,2))
plot(data_us_90jul[,1], main="US market Premium")
plot(data_us_90jul[,2], main="US SMB")
plot(data_us_90jul[,3], main="US HML")
dev.off()
setwd(dir_temp)

