# Set Working Directory ---------------------------------------------------

setwd("D:/Baruch/Financial Statistics/Financial_Statistics")

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
for (i in 1:length(ticker)) 
  {
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
R_j = t(omega_T) %*% (R*252)



########PCA######################################
covmatrix = cov(t(R))
cormatrix = cor(t(R))

eigen(cormatrix)$values

PCA2 = prcomp(t(R), cor=F)
PCA2_var_perc = PCA2$sdev^2/sum(PCA2$sdev^2)
barplot(PCA2_var_perc, ylim=c(0,1), ylab="Var(%)",cex.lab=1.5,cex.axis=2)

cutoff = 0.01
nchoose = sum(PCA2_var_perc/max(PCA2_var_perc)>=cutoff) ## choose first nchoose PCs

RF = t(R) %*% (PCA2$rotation[,1:nchoose])

mydates = as.Date(colnames(R))

par(mfrow=c(3,1))
subTS = zoo(RF[,1], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='1st PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(RF[,2], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='2nd PC factor returns',mai=c(-3,0,-3,0)) 
subTS = zoo(RF[,3], mydates)
subTS = xts(subTS, index(subTS))
plot(subTS[1:Ndays], main='3rd PC factor returns',mai=c(-3,0,-3,0)) 
#dev.off()
par(mfrow=c(1,1))

### get the regression coefficients beta's
betaT = matrix(NA, nrow=20, ncol=nchoose)
residuals = 0*R
for (i in 1:20){
  temp_lm = lm(R[i,]~RF)
  betaT[i,] = temp_lm$coefficients[-1]
  residuals[i,] = temp_lm$residuals
}
colnames(betaT) = colnames(RF)
rownames(betaT) = rownames(R)

hist(cor(t(residuals)))  
hist(cor(t(R)))   

## Approximated covariance and correlation matrix on returns
Sigma_R_est = betaT %*% cov(RF) %*% t(betaT) + 
  diag(diag(cov(t(residuals))))

Sigma_R_est=Sigma_R_est*252
Sigma_R_est_inv=ginv(Sigma_R_est)
Mu_R_est = apply(R, 1, mean) * 252  # means of asset returns

pca_unity  = rep(1,length(Mu_R_est))
pca_A = as.numeric(t(pca_unity) %*% Sigma_R_est_inv %*% pca_unity) # or sum(covarinv)
pca_B = as.numeric(t(pca_unity) %*% Sigma_R_est_inv %*% Mu_R_est) # or sum(covarinv %*% mu)
pca_C = as.numeric(t(Mu_R_est) %*% Sigma_R_est_inv %*% Mu_R_est)
pca_D = pca_A*pca_C-pca_B^2 # equivalent to determinant

# Tangency Portfolio with a Risk Free Asset -------------------------------

pca_omega_T = Sigma_R_est_inv %*% (Mu_R_est - r_f*pca_unity) / as.numeric(pca_B - pca_A %*% r_f)
pca_mu_T = as.numeric(t(pca_omega_T) %*% Mu_R_est) # or ((C-B*r_f)/(B-A*r_f))
pca_sigma_T = as.numeric(sqrt(t(pca_omega_T) %*% Sigma_R_est %*% pca_omega_T)) 
pca_sharpe_T = as.numeric((pca_mu_T - r_f) / pca_sigma_T)
pca_R_j = t(pca_omega_T) %*% (R*252)

