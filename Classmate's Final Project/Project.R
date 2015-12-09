# Set Working Directory ---------------------------------------------------

setwd("C:/Users/wei/Documents/test")

# Install and Load Packages -----------------------------------------------

install.packages("quantmod")
install.packages("RQuantLib")
install.packages("MASS")
library(quantmod)
library(RQuantLib)
library(MASS)
library(lattice)

# Get equity Data from Yahoo Finance --------------------------------------

ticker = c('AAPL','XOM','MSFT','JNJ','GE','WFC','PG','JPM','CVX','VZ',
           'PFE','INTC','BAC','T','MRK','KO','C','IBM','GOOGL','GILD')
start = "2007-01-01"; end = "2014-12-01"
for (i in 1:length(ticker)) {
  data = getSymbols(ticker[i], src="yahoo", from=start, to=end, adjust=T)
  P_i = t(get(data)[,4])
  R_i =  t(dailyReturn(get(data),type='arithmetic',leading=T))
  if (i == 1) { P = P_i; R = R_i } 
  else { P  = rbind(P, P_i); R = rbind(R, R_i) }
}
getSymbols('SPY', src="yahoo", from=start, to=end, adjust=T)
P_SPY = t(get('SPY')[,4])
R_SPY = t(dailyReturn(get('SPY'),type='arithmetic',leading=T)) * 252
rownames(P) = rownames(R) = ticker

# Get 90-day t-bill (risk-free) rate from St. Louis Federal Reserve Bank --

r_f = as.numeric(tail(get(getSymbols("TB3MS", src="FRED", adjust=T)),n=1))         

# Descriptive Statistics --------------------------------------------------

mu = apply(R, 1, mean) * 252  # means of asset returns
covar = cov(t(R)) * 252 # equivalent to var(t(R))*252
covarinv = ginv(covar) # Moore-Penrose Generalized Inverse of matrix.

# Compute the Efficient Frontier ------------------------------------------

unity  = rep(1,length(mu))
A = as.numeric(t(unity) %*% covarinv %*% unity) # or sum(covarinv)
B = as.numeric(t(unity) %*% covarinv %*% mu) # or sum(covarinv %*% mu)
C = as.numeric(t(mu) %*% covarinv %*% mu)
D = A*C-B^2 # equivalent to determinant

# Tangency Portfolio with a Risk Free Asset -------------------------------

omega_T = covarinv %*% (mu - r_f*unity) / as.numeric(B - A %*% r_f)
mu_T = as.numeric(t(omega_T) %*% mu) # or ((C-B*r_f)/(B-A*r_f))
sigma_T = as.numeric(sqrt(t(omega_T) %*% covar %*% omega_T)) 
sharpe_T = as.numeric((mu_T - r_f) / sigma_T)
R_j= t(omega_T) %*% (R*252)

# This section is to calculate the Turn Over Rate
# Turn_Over_Rate= Sum of Absolute Value  Stock [i] Weight [i] [t+1] - weight[i][t]*(1+r[i][t])/(1+r[t]*W[t])
# The change of Return Rate compared to Portfolio Return 
O=(1+r_f)/(1+sharpe_T*r_f)
# The change of Return Rate compared to Portfolio Return * Stock Weight 
P=sharpe_T*O
# The t+1 weight - Adj Weight of t 
Q=sigma_T-P
# Total sum of the Absolute value of the difference of weights 
sum(abs(Q))


for (i in 1:length(ticker)) { 
 K=R_j[i]-r_f
}
#

Sharp_Ratio=(K-r_f)/sigma_T
#


sum(Sharp_Ratio)





















