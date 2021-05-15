##### Volatility prediction using GARCH(1,1) ####
# Windows size 365 days
# Rolling window with refit after every months

##### Prep #####
# Load
source("add/libraries.R")
source("add/Functions.R")
load("data/BTC_USD_27_03_21.rda")
load("data/log_ret_27_03_21.rda")

# Volatility prediction for trading signals using rugarch package 

# Data prep prices
BTC <- BTC_USD_27_03_21$`BTC-USD.Close`
BTC <- BTC[-1,]
BTC <- na.omit(BTC)

# Data prep log returns
dat_xts <- dat <- log_ret_27_03_21
colnames(dat) <- "BTC log returns"
head(dat)

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)
dat <- na.omit(dat)

# Define date from which we start to predict volatilities
# Define index for dat (class = data frame)
# 1.7.2020 - 27.03.2021

split <- as.Date("2016-01-01")

ind_1 <- index(dat)[which(rownames(dat) == split)]
dat[ind_1,]

# QQ-Plot normal
GARCH_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "norm")
GARCH_fit <- ugarchfit(GARCH_spec, data = dat)
# Define residuals for normal
eps <- GARCH_fit@fit$residuals # residuals
z_t <- eps / GARCH_fit@fit$sigma # standardized residuals
theor <- pdist(distribution = "norm", sort(z_t))
emp <- rank(sort(z_t))/length(z_t)
par(mfrow = c(2,1))
plot(x = emp, y = theor, main = "QQ-Plot normal distribution")

# QQ-Plot student t
GARCH_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "std")
GARCH_fit <- ugarchfit(GARCH_spec, data = dat)
# Define residuals for student t
eps <- GARCH_fit@fit$residuals # residuals
z_t <- eps / GARCH_fit@fit$sigma # standardized residuals
theor <- pdist(distribution = "std", sort(z_t), shape = GARCH_fit@fit$coef['shape'])
emp <- rank(sort(z_t))/length(z_t)
plot(x = emp, y = theor, main = "QQ-Plot student t") # student t looks slightly better

# Therefore, we use GARCH with student t
GARCH_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "std")

# GARCH one-step-ahead forecasts with rolling window
# Window size = 365
# Refit GARCH every months (30 time steps)

mod <- ugarchroll(GARCH_spec, data = dat, n.ahead = 1, 
                 n.start = ind_1,  refit.every = 30, refit.window = "moving", 
                 window.size = 365, 
                 fit.control = list(),
                 calculate.VaR = FALSE,
                 keep.coef = TRUE)

par(mfrow = c(1,1))
plot(mod)
2
0

# Save results in dataframe

GARCH_vola <- mod@forecast$density

# Transform index dates for dataframe

ind_0 <- index(dat)[which(rownames(dat) == split)]
ind_1 <- nrow(dat)

rownames(GARCH_vola) <- index(dat_xts[(ind_0+1):ind_1])

# Trading strategy: Trading based on historical volatility as threshold 
# Generate trading signal if predicted vola >= 0.95 level of historical vola

th <- sd(dat$`BTC log returns`[1:(ind_0)]) * 1.64 # 1.64 = two-sided 0.95

for(i in 1:nrow(GARCH_vola)){
  if(GARCH_vola$Sigma[i] >= th){
    GARCH_vola$Trading_signal[i] <- 0
  }
  else{
    GARCH_vola$Trading_signal[i] <- 1
  }
}

# Trading strategy: Trading based on sign of mean forecast (based on ARMA(1,1))

GARCH_vola$Signum_Signal <- sign(GARCH_vola$Mu)

# Trading function

perf <- GARCH_vola$Trading_signal * dat_xts[(ind_0+1):ind_1]
sharpe<-sqrt(365)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))

# Signum trading

perf_sign <- GARCH_vola$Signum_Signal * dat_xts[(ind_0+1):ind_1]
sharpe_sign <- sqrt(365)*mean(perf_sign,na.rm=T)/sqrt(var(perf_sign,na.rm=T))

# Buy and hold
bh <- rep(1, length(perf)) * dat_xts[(ind_0+1):ind_1]
sharpe_bh <- sqrt(365)*mean(bh, na.rm = T) / sqrt(var(bh, na.rm = T))

# Save as .rda file

# save(GARCH_vola, file = "data/GARCH_vola_predictions/GARCH_vola_predictions.rda")
# save(GARCH_Signals)

# Plot performances
par(mfrow = c(1,1))
plot(cumsum(bh), main = "Cumulative daily returns")
lines(cumsum(perf_sign), col = 2)
lines(cumsum(perf), col = 3)
addLegend(legend.loc = "topleft", legend = c(paste("Buy-and-Hold, Sharpe:", round(sharpe_bh, 2)), 
                                                 paste("GARCH Signum Trading, Sharpe:", round(sharpe_sign,2)),
                                                 paste("GARCH Volatility Trading, Sharpe:", round(sharpe, 2))), 
          col = c(1,2,3), lty = 1)



# Save trading signals only

garch_out_signal=as.xts(GARCH_vola)$Trading_signal["2020-07-01::"]
save(garch_out_signal, file = "data/GARCH_vola_predictions/garch_out_signal.rda")




