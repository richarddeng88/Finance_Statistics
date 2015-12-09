library(xts)
library(timeDate)
library(MASS)

### function ES_unit is computing the expected short-fall for one single investment

ES_unit <- function(price, windowsize, k, alpha, method)
{
  ret = dailyReturn(price)[-1]
  log_ret = dailyReturn(price, type='log')[-1]
  N = length(ret)
  ES_long = rep(NA, N-windowsize+1)
  ES_short = rep(NA, N-windowsize+1)
  
  if (method=='Normal'){
    ### assume the gross returns are I.I.D. normal
    for (e in windowsize:N){
      temp_ret = ret[(e-windowsize+1):e]
      mean_ret = mean(temp_ret)
      sd_ret = sd(as.vector(temp_ret))
      ES_long[e-windowsize+1] = price[e+1] *
        (-k*mean_ret + sqrt(k)*dnorm(qnorm(alpha))*sd_ret/alpha)
      ES_short[e-windowsize+1] = price[e+1] *
        (k*mean_ret + sqrt(k)*dnorm(qnorm(alpha))*sd_ret/alpha)
    }
  }   
  
  if (method=='Lognormal'){
    ### assume the log-returns are I.I.D. normal
    for (e in windowsize:N){
      temp_ret = log_ret[(e-windowsize+1):e]
      mean_ret = mean(temp_ret)
      sd_ret = sd(as.vector(temp_ret))
      ES_long[e-windowsize+1] = price[e+1] *
        (1 - exp(k*mean_ret+k*sd_ret^2/2)*
           pnorm(qnorm(alpha)-sqrt(k)*sd_ret)/alpha)
      ES_short[e-windowsize+1] = price[e+1] *
        (exp(k*mean_ret+k*sd_ret^2/2)*
           pnorm(qnorm(alpha)+sqrt(k)*sd_ret)/alpha-1)
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
      #temp_nu = nu
      ES_long[e-windowsize+1] = price[e+1] *
        (-temp_mean + dt(qt(alpha, temp_nu),temp_nu)/alpha*
           (temp_nu+qt(alpha, temp_nu)^2)/(temp_nu-1)*temp_lambda)
      ES_short[e-windowsize+1] = price[e+1] *
        (temp_mean + dt(qt(alpha, temp_nu),temp_nu)/alpha*
           (temp_nu+qt(alpha, temp_nu)^2)/(temp_nu-1)*temp_lambda) 
    }
  }
  
  if (method=='Empirical'){
    ret_lag_k = (price - lag(price, k))/lag(price, k)[-1]
    ### assume the gross returns are I.I.D. empirical CDF
    for (e in (windowsize+k-1):N){
      temp_ret = ret_lag_k[(e-windowsize+1):e]
      indi_long = temp_ret<quantile(temp_ret, alpha)
      indi_short = temp_ret>quantile(temp_ret,1-alpha)
      if (sum(indi_long)>0){
        ES_long[e-windowsize+1] = price[e+1] *
          (-sum(indi_long*temp_ret)/sum(indi_long))        
      }else{ES_long[e-windowsize+1]=0}
      if (sum(indi_short)>0){
        ES_short[e-windowsize+1] = price[e+1] *
          (sum(indi_short*temp_ret)/sum(indi_short))  
      }
    }
  }  
  
  all_date = index(ret)
  
  ### adding k days after the original dates; 
  ### Note that here I didn't exclude the holidays and weekends;
  ### A more robust code will be implemented in the future.
  all_date = c(all_date, 
               seq(tail(all_date,1)+1, tail(all_date,1)+k, 1))
  
  predict_date = all_date[(windowsize+k):(N+k)]
  ES_long = xts(ES_long, order.by=predict_date)
  ES_short = xts(ES_short, order.by=predict_date)
  out = cbind(ES_long, ES_short)
  colnames(out) = c('ES_long', 'ES_short')
  return(out)
}