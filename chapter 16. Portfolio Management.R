library(Ecdat)
library(quadprog)
data(CRSPday)
R = 100*CRSPday[ ,4:6] # convert to percentages
mean_vect = apply(R, 2 ,mean)
cov_mat = cov(R)
sd_vect = sqrt(diag(cov_mat))
Amat = cbind(rep(1, 3), mean_vect) # set the constraints matrix
muP = seq(0.05, 0.14, length = 300) # target portfolio means

# for the expect portfolio return
sdP = muP # set up storage for std dev's of portfolio returns
weights = matrix(0, nrow = 300, ncol = 3) # storage for weights
for (i in 1:length(muP)) # find the optimal portfolios
    {
     bvec = c(1, muP[i]) # constraint vector
     result =solve.QP(Dmat = 2 * cov_mat, dvec = rep(0, 3),
                         Amat = Amat, bvec = bvec, meq = 2)
        sdP[i] = sqrt(result$value)
        weights[i,] = result$solution
        }
pdf("Finance_Statistics/quad_prog_plot.pdf", width = 6, height = 5)

plot(sdP, muP, type = "l", xlim = c(0, 2.5),ylim = c(0, 0.15), lty = 3) # plot efficient frontier (and
 # inefficient portfolios below the min var portfolio)

mufree = 1.3 / 253 # input value of risk-free interest rate
points(0, mufree, cex = 4, pch = "*") # show risk-free asset
sharpe = (muP - mufree) / sdP # compute Sharpe's ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe's ratio
weights[ind, ] # print the weights of the tangency portfolio
lines(c(0, 2), mufree + c(0, 2) * (muP[ind] - mufree) / sdP[ind],
         lwd = 4, lty = 1, col = "blue") # show line of optimal portfolios
points(sdP[ind], muP[ind], cex = 4, pch = "*") # tangency portfolio
ind2 = (sdP == min(sdP)) # find minimum variance portfolio
points(sdP[ind2], muP[ind2], cex = 2, pch = "+") # min var portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3], muP[ind3], type = "l", xlim = c(0, 0.25),
         ylim = c(0, 0.3), lwd = 3, col = "red") # plot efficient frontier
text(sd_vect[1], mean_vect[1], "GE", cex = 1.15)
text(sd_vect[2], mean_vect[2], "IBM", cex = 1.15)
text(sd_vect[3], mean_vect[3], "Mobil", cex = 1.15)

graphics.off()















