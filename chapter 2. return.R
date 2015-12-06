## generate a path of random walk
par(mfrow=c(1,2))
set.seed(1)
t <- 1000
x <- 0:t
y <- c(0,cumsum(rnorm(t,0,1)))
plot(x,y,type="l")

## generate a path of geometric random walk
set.seed(1)
t <- 1000
x <- 0:t
y <- exp(c(0,cumsum(rnorm(t,0,1))))-1
plot(x,y,type="l")

###================== playing GM and FORD return ===============================================
df <- read.csv("data/finance_stats/Stock_bond.csv", header = T)

par(mfrow=c(1,2))
plot(df$GM_AC, type="l", pch=20) # GM COMPANY
plot(df$F_AC,type = "l", pch=20)  # FORD COMPANY

n <- dim(df)[1]
GM_return <- df$GM_AC[-1]/df$GM_AC[-n]-1  # GM return
FR_return <- df$F_AC[-1]/df$F_AC[-n]-1  # Ford return
par(mfrow=c(1,1))
plot(GM_return,FR_return, pch=20)
cor(GM_return,FR_return)

## linear regression
gl <- glm(GM_return~FR_return)

par(mfrow=c(1,1))
plot(GM_return,FR_return, pch=20)
abline(gl, col="blue",lwd=3)

par(mfrow=c(2,2))
plot(gl)

#log return
n <- dim(df)[1]
log_GM_return <- log(df$GM_AC[-1]/df$GM_AC[-n])  # GM return
log_FR_return <- log(df$F_AC[-1]/df$F_AC[-n])  # Ford return
par(mfrow=c(1,1))
plot(log_GM_return,log_FR_return, pch=20)

##======================================== simulations =======================================

    # this section explore the effects of leverage n a simplified setting. 
    # suppose the hedge fund owns $1,000,000
    # suppose the daily log returns on the stock have a mean of 0.05/year and a sd of 0.23/year. 
    # for daily log return, mean=0.05/253, sd = 0.23/sqrt(253)
    niter <- 100000          # number of iteration
    below <- rep(0,niter)  # set up storage
    set.seed(2009)
    for (i in 1:niter){
        r=rnorm(45, mean=0.05/253, sd=0.23/sqrt(253))  # generate random numbers
        log_price <- log(1000000)+cumsum(r) # log return follow a geo random walk model \
        minlog_p <- min(log_price) # minimum price over next 45 days
        below[i] <- as.numeric(minlog_p < log(950000))
    }
    cat("the odds that the value of the stock will be beloww $950,000 is" , mean(below))
    

## ==================== SIMULATING A GEOMETRIC RANDOM WALK ====================================
    set.seed(2012)
    n = 253
    par(mfrow=c(3,3))
    for (i in (1:9))
    {
        logr = rnorm(n, 0.05 / 253, 0.2 / sqrt(253))
        price = c(120, 120 * exp(cumsum(logr)))
        plot(price, type = "l")
    }

## look at McDonald's stock 
    data = read.csv("data/finance_stats/MCD_PriceDaily.csv")
    head(data)
    adjPrice = data[, 7]
    mean(adjPrice)
    
    # try to plot the time series data. 
    par(mfrow=c(1,1))
    plot(data$Date, data$Close, type="l")
    a<-ts(c(data$Date, data$Close))
    plot(a)





