library(xts)
library(timeDate)
library(MASS)

### k-th step prediction of VaR by bootstrapping
### alpha is the risk threshold
### (1-beta)*100 is the confidence level for the boostrapped 
###    Upper bound of VaR.

VaR_unit_boot <- function(price, windowsize, k, alpha, method, B, beta)
{
  ret = dailyReturn(price)[-1]
  log_ret = dailyReturn(price, type='log')[-1]
  N = length(ret)
  VaR_long = rep(NA, N-windowsize+1)
  VaR_long_U = rep(NA, N-windowsize+1)
  VaR_short = rep(NA, N-windowsize+1)
  VaR_short_U = rep(NA, N-windowsize+1)
  
  if (method=='Normal'){
    ### assume the gross returns are I.I.D. normal
    for (e in windowsize:N){
      temp_ret = ret[(e-windowsize+1):e]
      VaR_long_boot = rep(NA, B)
      VaR_short_boot = rep(NA, B)
      for (b in 1:B){
        resamp_ret = sample(temp_ret, replace=TRUE)
        mean_ret = mean(resamp_ret)
        sd_ret = sd(as.vector(resamp_ret))
        VaR_long_boot[b] = price[e+1] *
          (-k*mean_ret - sqrt(k)*qnorm(alpha)*sd_ret)
        VaR_short_boot[b] = price[e+1] *
          (k*mean_ret - sqrt(k)*qnorm(alpha)*sd_ret)
      }
      VaR_long[e-windowsize+1]=mean(VaR_long_boot)
      VaR_long_U[e-windowsize+1]=quantile(VaR_long_boot, 1-beta)
      VaR_short[e-windowsize+1]=mean(VaR_short_boot)
      VaR_short_U[e-windowsize+1]=quantile(VaR_short_boot, 1-beta)
    }
  }   
  
  if (method=='Lognormal'){
    ### assume the log-returns are I.I.D. normal
    for (e in windowsize:N){
      temp_ret = log_ret[(e-windowsize+1):e]
      mean_ret = mean(temp_ret)
      sd_ret = sd(as.vector(temp_ret))
      VaR_long[e-windowsize+1] = price[e+1] *
        (1-exp(k*mean_ret + sqrt(k)*qnorm(alpha)*sd_ret))
      VaR_short[e-windowsize+1] = price[e+1] *
        (exp(k*mean_ret - sqrt(k)*qnorm(alpha)*sd_ret)-1) 
    }
  }
  
  if (method=='T'){
    ret_lag_k = (price - lag(price, k))/lag(price, k)[-1]
    fit = fitdistr(ret_lag_k[k:N], "t")[1]
    nu = fit$estimate[3]
    ### assume the gross returns are I.I.D. T-distr
    for (e in (windowsize+k-1):N){
      temp_ret = ret_lag_k[(e-windowsize+1):e]
      if (is.na(sum(temp_ret))) {
        print('error')
      }
      fit = fitdistr(temp_ret, "t")[1]
      temp_mean = fit$estimate[1]
      temp_lambda = fit$estimate[2]
      temp_nu = fit$estimate[3]
      #temp_nu = fit$estimate[3]
      temp_nu = nu
      VaR_long[e-windowsize+1] = price[e+1] *
        (-temp_mean - qt(alpha, temp_nu)*temp_lambda)
      VaR_short[e-windowsize+1] = price[e+1] *
        (temp_mean - qt(alpha, temp_nu)*temp_lambda) 
    }
  }
  
  if (method=='Empirical'){
    ret_lag_k = (price - lag(price, k))/lag(price, k)[-1]
    ### assume the gross returns are I.I.D. empirical CDF
    for (e in (windowsize+k-1):N){
      temp_ret = ret_lag_k[(e-windowsize+1):e]
      VaR_long_boot = rep(NA, B)
      VaR_short_boot = rep(NA, B)
      for (b in 1:B){
        resamp_ret = sample(temp_ret, replace=TRUE)
        VaR_long_boot[b] = price[e+1] *
          (-quantile(resamp_ret,alpha))
        VaR_short_boot[b] = price[e+1] *
          (quantile(resamp_ret,1-alpha))        
      }
      VaR_long[e-windowsize+1]=mean(VaR_long_boot)
      VaR_long_U[e-windowsize+1]=quantile(VaR_long_boot, 1-beta)
      VaR_short[e-windowsize+1]=mean(VaR_short_boot)
      VaR_short_U[e-windowsize+1]=quantile(VaR_short_boot, 1-beta)
    }
  }  
  
  all_date = index(ret)
  
  ### adding k days after the original dates; 
  ### Note that here I didn't exclude the holidays and weekends;
  ### A more robust code will be implemented in the future.
  all_date = c(all_date, 
               seq(tail(all_date,1)+1, tail(all_date,1)+k, 1))
  
  predict_date = all_date[(windowsize+k):(N+k)]
  VaR_long = xts(VaR_long, order.by=predict_date)
  VaR_long_U = xts(VaR_long_U, order.by=predict_date)
  VaR_short = xts(VaR_short, order.by=predict_date)
  VaR_short_U = xts(VaR_short_U, order.by=predict_date)
  out = cbind(VaR_long, VaR_short, VaR_long_U, VaR_short_U)
  colnames(out) = c('VaR_long', 'VaR_short', 
                    'VaR_long_Upper', 'VaR_short_Upper')
  return(out)
}