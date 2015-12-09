library('xts')
library('tseries')
library('quantmod')

dir_data = "W:/Data/Pairtrading"
dir_code = "W:/Rcode/Chp15_Pair_Trading"

setwd(dir_code)
source("pairtrade.R")

setwd(dir_data)
load('testdata.RData')

names(test.data) = c("CVX.price","XOM.price","CVX.side","XOM.side",
                     "CVX.share","XOM.share","pnl",
                     "CVX.avg.price","XOM.avg.price")

test.data

CVX.sharediff = diff(test.data[,"CVX.share"])
XOM.sharediff = diff(test.data[,"XOM.share"])

## check average price on June 3rd
(as.numeric(XOM.sharediff[2]*test.data[2,"XOM.price"])+
   XOM.sharediff[3]*test.data[3,"XOM.price"])/test.data[3,"XOM.share"]

## check average price on June 3rd
(as.numeric(CVX.sharediff[2]*test.data[2,"CVX.price"])+
   CVX.sharediff[3]*test.data[3,"CVX.price"])/test.data[3,"CVX.share"]

## check pnl on June 3rd
XOM.sharediff[4]*(-test.data[4,"XOM.price"]+test.data[4,"XOM.avg.price"])+
  CVX.sharediff[4]*(-test.data[4,"CVX.price"]+test.data[4,"CVX.avg.price"])

## check average price on June 4th
(XOM.sharediff[5]*test.data[5,"XOM.price"]+
   as.numeric(test.data[4,"XOM.share"]*test.data[4,"XOM.avg.price"]))/
  (test.data[5,"XOM.share"])
  
## check average price on June 4th
(CVX.sharediff[5]*test.data[5,"CVX.price"]+
   as.numeric(test.data[4,"CVX.share"]*test.data[4,"CVX.avg.price"]))/
  (test.data[5,"CVX.share"])

## check pnl on June 5th
-as.numeric(test.data[5,"XOM.share"])*(-test.data[6,"XOM.price"]+as.numeric(test.data[5,"XOM.avg.price"]))-
  as.numeric(test.data[5,"CVX.share"])*(-test.data[6,"CVX.price"]+as.numeric(test.data[5,"CVX.avg.price"]))


## check pnl on June 10th
-as.numeric(test.data[8,"XOM.share"])*(-test.data[9,"XOM.price"]+as.numeric(test.data[8,"XOM.avg.price"]))-
  as.numeric(test.data[8,"CVX.share"])*(-test.data[9,"CVX.price"]+as.numeric(test.data[8,"CVX.avg.price"]))

## check pnl on June 12th
-as.numeric(test.data[10,"XOM.share"])*(-test.data[11,"XOM.price"]+as.numeric(test.data[10,"XOM.avg.price"]))-
  as.numeric(test.data[10,"CVX.share"])*(-test.data[11,"CVX.price"]+as.numeric(test.data[10,"CVX.avg.price"]))