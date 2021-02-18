#03_logistic.R

#Logistic model:
pred_logistic <- function(theta, x)
{
  z <- exp(theta[3] + theta[4] *x)
  Ey <- theta[1] + theta[2] *z / (1 + z) 
}

#Fit logistic model:
fit_logistic <- function(dat, par){
  
  ## define log likelihood
  lnL_logistic <- function(theta,dat){
    -sum(dnorm(dat$gcc_mean, mean = pred_logistic(theta, dat$doy), sd = dat$gcc_std, log = TRUE))
  }
  
  ## fit by numerical optimization
  optim(par, fn = lnL_logistic, dat = dat)
}