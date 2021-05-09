# Intro####
## We want to trade S&P500, daily
## For that purpose we rely on two forecast models and buy/sell tomorrows
## returns based on the sign of tomorrows forecast
##   - We rely on a MGARCH-Model
##   - We reyl on a modifed MGARCH where the (filtered) volume replaces the
##     volatility
## both models are benchmarked against buy-and-hold
## Particular attention is devoted to the downturn following the COVID-Breackout
## (mitigating 'crisis' is the main purpose of both models)
#
## MGARCH:
## The idea of the MGARCH-model is that (long-positioned) market players tend
## to oversell during down-turns
##
## Overselling (herding) means that transaction volumes will inflate and thus
## the volatility will inflate too.
##
## Therefore, high-volatility might be indicative of possible downturns,
## especially in uncertain times (recession, crisis)
##
## We expect the sign of the parameter e in the MGARCH-regresseion to be negative
##
##       r_t = mu + e*sig_t + eps_t


#.####
# 1. Data####
library(xts)
library(fGarch)


## 1.1. Plot####
par(mfrow=c(2,2))
plot(x_level)
plot(log_x_level)
plot(x_log_ret)

load("data/log_ret_27_03_21.rda")
head(log_ret_27_03_21)

tail(dat$Bid)
x_log_ret <- log_ret_27_03_21
head(x_log_ret)
tail(x_log_ret)
## 1.2. Slicing xts-object####
# Impose start date prior to financial crisis because GARCH-in-mean is strong
# for the crisis (otherwise the effect vanishes)
# start_date <- "2007-01-01"
start_date <- "2017-01-01"
# Focus on COVID break-out
# end_date <- "2020-09-25"
# in_sample <- "2015-01-01"
end_date <- "2021-03-27"
in_sample <- "2020-01-01"
x_all <- na.exclude(x_log_ret[paste(start_date,"/",sep="")])
x_all <- x_all[paste("/",end_date,sep="")]
# Specify in-sample and out-sample time spans and fit models on in-sample span

### 1.2.1. in sample####
## Fitting (in-sample):
## We identify and fit a ARMA-GARCH model to the log-return to obtain sig_t
## We fit a MGARCH regression model. regressing sig_t on r_t
## We fit a VOLUME regression model, regressing V_t-1 on r_t
## Q: Why did we not lag sig_t in the MGARCH-regression
x_in <- x_all[paste("/",in_sample,sep="")]
head(x_in,1)
tail(x_in,1)
# x_in ist von 2007-01-03 bis 2014-12-31


### 1.2.2. out sample####
## Forecasting (out-of-sample)
## We set up one-step ahead out-of-sample forecasts based on both (in-sample)
## regression models
## We thus have to compute sig_t out-of-sample based on the in-sample (ARMA-)
## GARCH-model
x_out <- x_all[paste(in_sample,"/",sep="")]
head(x_out,1)
tail(x_out,1)
# x_out ist von 2015-01-02 bis 2020-09-25

### 1.2.3. Proceeding: Trading####
## Trading:
##   Based on the above forecasts we derive simple trading rules for both models
##   (long/short depending on sign of forecast)
#
##   We derive performance numbers (Sharpe ratios) and compare buy-and-hold with
##   both forecast models
#
##   We then use the GARCH for adjusting position-sizes (large positions if vola
##   /risk is small, samll position if vola/risk is high)
#
##   Since the performances differ, we also implement a simple equally
##   weighted portfolio (of all active contenders)
#
## Note: Selection of In/Out-Sample Spans
## Since both models emphasize recessive/crises dynamics (protraced
## downturns combined with high-vola, high-volume) we need to include
## the financial crisis in our in-sample span.
##
## We split in/out-of-sample spans such that the series are sufficiently
## long on both spans
##
## We terminate the out-of-sample span in Fall 2020 in order to focus on the
## COVID-breakout

#.####
# 2. Fitting the Models in-sample####
## 2.1. ARMA-GARCH####
# We fit the ARMA-GARCH moddel to obtain sig_t
par(mfrow=c(1,1))
acf(x_in)
plot(x_in)
pacf(x_in)
# ARMA-GARCH
# Only AR(1) required with more recent data (no AR with older/shorter sample)!
# No weekly effect anymore after accounting for vola-clusters
y.garch_11 <- garchFit(~arma(1,0) + garch(1,1),
                       data=x_in, delta=2, include.delta=F, include.mean=T, trace=F)
summary(y.garch_11)

# Alle Model-Parameter sind signifkant -> relevant für den Fitt, alle < 0.05

# Jarque-Bera ist sehr schlecht -> keine Gaussverteilung
# Ljung-Box:
#   R:    Sind alle über 0.05, somit ARMA-Fit gut (betriffts u's)
#   R^2:  Sind alle über 0.05, somit GARCH-Fit gut (betriffts u^2's)




## 2.2. MGARCH####
# Fit MGARCH with sig_t of ARMA-GARCH
sigma_t_in <- y.garch_11@sigma.t
names(sigma_t_in) <- index(x_in)
sigma_t_in <- as.xts(sigma_t_in)
index(sigma_t_in) <- index(x_in)

summary()

# Note that sigma_t depends on lagged data:
# therefore we can regress it directly onto x_in (without lagging)
lm_obj <- lm(x_in ~ sigma_t_in)

summary(lm_obj)
## Intercept und sigma_t für Formel aus Slide 3 -> Forecast machen
# r_t = mu + e*sig_t + eps_t


#.####
# 3. Compute sig_t####
# We now compute sigma_t Out-of-Sample (Based on ARMA-GARCH Model)
# Sig_t-Funktion####
compute_sigma_t_out_of_sample_func <- function(x_out, y.garch_11, x_in)
{ 
  # Vola based on in-sample parameters
  sigma_t <- rep(NA, length(x_out))
  a <- y.garch_11@fit$coef["beta1"]
  b <- y.garch_11@fit$coef["alpha1"]
  d <- y.garch_11@fit$coef["omega"]
  
  # First data point: based on last in-sample data
  # Formula: variance-equation
  sigma_t[1] <- sqrt(d + a*y.garch_11@sigma.t[length(y.garch_11@sigma.t)]^2 + 
                       b*x_in[length(x_in)]^2)
  
  # On out-of-sample span
  for (i in 2:length(x_out))
  {
    sigma_t[i] <- sqrt(d+a*sigma_t[i-1]^2+b*x_out[i-1]^2)
  }
  # Transform sigma_t into xts object
  names(sigma_t) <- index(x_out)
  sigma_t <- as.xts(sigma_t)
  index(sigma_t) <- index(x_out)
  return(list(sigma_t=sigma_t))
}
# Compute sigma_t out-of-sample
# Note: we ignore the ARMA-part because the coefficient is small
# (as is the case of most financial applications)
sigma_t <- compute_sigma_t_out_of_sample_func(x_out, y.garch_11, x_in)$sigma_t


#.####
# 4. Computing Forecasts####
## Once sig_t is obtained out-of-sample we can plug it into the fitted
## MGARCH-regression for obtaining forecasts of the future returns
## out-of-sample
##      hat(r)_t+1 = hat(mu) + hat(e) * hat(sig)_t+1

## Note: we do not have to lag hat(sig)_t+1 in this equation because this
## expression relies on lagged returns r_t, r_t-1, ... already!

# Predictions: based on sigma_t and in-sample regression model
# Note: since sigma_t depends on lagged data we do not need to lag sigma_t
# for the prediction
mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t
plot(mgarch_predict)
#.####
# 5. Trading####
## 5.1. Derive Returns####
## Based on the forecasts hat(r)_t+1 we buy/sell r_t+1 depending on the sign
## of hat(r)_t+1
## The returns of the MGARCH-model are thus sign(hat(r)_t+1)*r_t+1
returns_mgarch <- sign(mgarch_predict)*x_out
plot(returns_mgarch)

## 5.2 Compute Sharpes####
sharpe_buy_hold <- as.double(sqrt(365)*mean(x_out)/sqrt(var(x_out)))
sharpe_mgarch <- as.double(sqrt(365)*mean(returns_mgarch,na.rm=T)/sqrt(var(returns_mgarch,na.rm=T)))
# Compare: buy-and-hold, AR(5) classic, AR(1)-GARCH
par(mfrow=c(2,1))
plot(cumsum(x_out),main=paste("Buy-and-hold, sharpe=",round(sharpe_buy_hold,2),sep=""))
plot(cumsum(na.exclude(returns_mgarch)),main=paste("MGarch-model, sharpe=",round(sharpe_mgarch,2),sep=""))

## We canb see that the active strategy is mitigating the down-turn in early
## 2020 (Spring)
## However, performances are still 'peaky' because the size of the (absolute)
## returns is huge during the high-vola episode


## 5.3 Position-Sizing with GARCH####
## We would like to size/modulate the position according to the current
## risk-profile:
##    - Down-size the position during high-risk (high vola) episodes
##    - Up-size the position during low-risk (low vola) episodes
##    - Size proportional to 1/sig_t
##    - Note that sig_t has already been computed (out-of-sample)

# New trading rule: reduce position if vola is high (high risk),
# enlarge position if vola is small (low risk)
# Size vector inverse proportional to sigma_t and mean-value one
position_size_unscaled <- 1/sigma_t
position_size <- position_size_unscaled/mean(position_size_unscaled)
position_size
mean(position_size)
# Purpose: smooth trading performances, avoid huge movements during high-vola
# phases

new_rule <- position_size*sign(mgarch_predict)
returns_mgarch_sized <- new_rule*x_out
sharpe_mgarch_sized <- as.double(sqrt(365)*mean(returns_mgarch_sized,na.rm=T)/sqrt(var(returns_mgarch_sized,na.rm=T)))



## 5.4. Plot of the three models####
# Different dynamics: portfolio
par(mfrow=c(2,2))
plot(cumsum(x_out),main=paste("Buy-and-hold, sharpe=",round(sharpe_buy_hold,2),sep=""))

plot(cumsum(na.exclude(returns_mgarch)),main=paste("MGARCH-model, sharpe=",round(sharpe_mgarch,2),sep=""))
# Schwierig zu verkaufen, am Schluss die Spikes!!!!

plot(cumsum(na.exclude(returns_mgarch_sized)),main=paste("MGARCH-model, sized trades, sharpe=",round(sharpe_mgarch_sized,2),sep=""))
# Comment: the huge outliers during the COVID-breakout are smoothed out,
# as expected

## The previously peaky performances are smoothed out by the proposed risk-
## adjustment rule
##    - This is very (VERY) importatn feature of any actively managed product/fund

## Since the performances of the two active strategies (with/withoout risk-
## adjustment) differ we can examine mixing them (equally-weighted portfolio)

## 5.5. Equally weighted portfolio####
equally_weighted_portfolio <- (cumsum(x_out)+cumsum(na.exclude(returns_mgarch)) +
                                 cumsum(na.exclude(returns_mgarch_sized)))/3
sharpe_portfolio<-sqrt(365)*mean(diff(equally_weighted_portfolio),na.rm=T)/sqrt(var(diff(equally_weighted_portfolio),na.rm=T))
# par(mfrow=c(1,1))
plot(equally_weighted_portfolio,main=paste("Equally-weighted portfolio, sharpe=",round(sharpe_portfolio,2),sep=""))
# Meist ein toller Benchmark
# Schwierig zum besser machen.

