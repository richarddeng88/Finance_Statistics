dat = read.csv("data/finance_stats/Stock_Bond.csv", header = T)
prices = cbind(dat$GM_AC, dat$F_AC, dat$CAT_AC, dat$UTX_AC,
               dat$MRK_AC, dat$IBM_AC)
n = dim(prices)[1]
returns = 100 * (prices[2:n, ] / prices[1:(n-1), ] - 1)
pairs(returns)
mean_vect = colMeans(returns)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))







