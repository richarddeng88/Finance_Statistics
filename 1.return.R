## generate a path of random walk
par(mfrow=c(1,2))
set.seed(2011)
t <- 1000
x <- 0:t
y <- c(0,cumsum(rnorm(t,0,1)))
plot(x,y,type="l")

## generate a path of geometric random walk
set.seed(2011)
t <- 1000
x <- 0:t
y <- exp(c(0,cumsum(rnorm(t,0,1))))-1
plot(x,y,type="l")

##
df <- read.csv("data/fin/Stock_bond.csv", header = T)

par(mfrow=c(1,2))
plot(df$GM_AC, type="l", pch=20) 
plot(df$F_AC,type = "l", pch=20) 

n <- dim(df)[1]
GM_return <- df$GM_AC[-1]/df$GM_AC[-n]-1  # GM return
FR_return <- df$F_AC[-1]/df$F_AC[-n]-1  # Ford return
par(mfrow=c(1,1))
plot(GM_return,FR_return, pch=20)

## linear regression
gl <- glm(GM_return~FR_return)
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
niter <- 1e5           # number of iteration
below <- rep(0,niter)  # set up storage
set.seed(2009)
for (i in 1:niter){
    r=rnorm(45, mean=0.05/253, sd=0.23/sqrt(253))  # generate random numbers
    log_price <- log(1e6)+cumsum(r)
    minlog_p <- min(log_price) # minimum price over next 45 days
    below[i] <- as.numeric(minlog_p < log(850000))
}
mean(below)











