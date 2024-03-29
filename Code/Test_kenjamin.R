# Load
source("add/libraries.R")
source("add/Functions.R")
load("data/log_ret_27_03_21.rda")
load("data/BTC_USD_27_03_21.rda")

##### Volatility prediction for trading signals: Method 1 using fGArch #####
# Using functions from fGarch package

# Data prep prices
BTC <- BTC_USD_27_03_21$`BTC-USD.Close`
BTC <- na.omit(BTC)

# Data prep log returns
dat <- log_ret_27_03_21
head(dat)

# Separation of data
sep <- "2020-05-01"
dat[as.Date(sep)]

# Set window size
window <- 365 # 365 days = 1 year

# Start prediction using package rugarch
# Set window range to 365 days and iterate daily

# Just to keep in mind: Window range for first iteration
as.Date(sep)-window # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep)-window)]
as.Date(sep) # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep))]

# Just keep in mind: Window range for last iteration
as.Date("2021-03-26")-window
as.Date("2021-03-26")

# Predictions with rolling window one-step-ahead forecasts
# Defining left border of rolling window in the beginning (date_0) and at the end (date_n)
date_0 <- as.Date(sep)-window
date_n <- as.Date("2021-03-26")-window
datum <- date_0

# Define prediction matrix
vola_mat <- dat[seq(as.Date(sep)+1, as.Date("2021-03-27"), by = "days")]
vola_mat$GARCH_Volatility <- rep(NA, nrow(vola_mat))
vola_mat$GJR_Volatility <- rep(NA, nrow(vola_mat))


#vola_mat <- data.frame(Date = seq((as.Date(sep)+1), as.Date("2021-03-27"), by = "days"),GARCH_Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1), GJR_Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1))


# One-step-ahead forecasts using for-loop

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)
dat <- na.omit(dat)

# One-step-ahead forecasts using while
j <- 1

while(datum <= date_n){
  # Estimation of GARCH model
  garch_fit <- garchFit(~ garch(1,1), include.mean = T, data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)])
                                                                                   :(index(dat)[which(row.names(dat) == datum + window)])],
                        trace = FALSE, cond.dist = "QMLE")
  
  # Estimation of GJR Garch model
  GJR_spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
  GJR_fit <- ugarchfit(data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)]):(index(dat)[which(row.names(dat) == datum + window)])], spec = GJR_spec, trace = FALSE)
  
  
  print(paste("Iteration: ", j, " Date: ", datum, "End: ", datum + window))
  datum <- datum + 1
  
  # Predictions
  vola_mat$GARCH_Volatility[j] <- predict(garch_fit, n.ahead = 1)$standardDeviation
  vola_mat$GJR_Volatility[j] <- sigma(ugarchforecast(GJR_fit, n.ahead = 1))
  j <- j + 1
}

# Plot of log prices, log returns and predicted volatilities
par(mfrow = c(2,1), mar = c(3,3,1,1))

# Plotting log prices (easier to compare relative changes)
t <- seq(as.Date(sep)+1, as.Date("2021-03-27"), by = "days")
plot(log(BTC[t]), main = "Bitcoin log prices", ylab = "Log prices")

# Plotting log returns
dat <- log_ret_27_03_21
plot(dat[t], type = "l", main = "Bitcoin log returns", ylab = "Log return")

# Plotting predicted volatility
vola_mat <- na.omit(vola_mat)
vola_mat$ci_garch_lower <- quantile(vola_mat$GARCH_Volatility, 0.05)
vola_mat$ci_garch_upper <- quantile(vola_mat$GARCH_Volatility, 0.95)
vola_mat$ci_gjr_lower <- quantile(vola_mat$GJR_Volatility, 0.05)
vola_mat$ci_gjr_upper <- quantile(vola_mat$GJR_Volatility, 0.95)

par(mfrow = c(1,1), c(1,1,1,1))
plot(vola_mat$GARCH_Volatility, ylim = c(0.00, 0.09))
lines(vola_mat$GJR_Volatility, col = 2)

lines(vola_mat$ci_garch_lower, lty = 2)
lines(vola_mat$ci_garch_upper, lty = 2)
lines(vola_mat$ci_gjr_lower, lty = 2, col = 2)
lines(vola_mat$ci_gjr_upper, lty = 2, col = 2)


addLegend("bottomleft", on=1, 
          legend.names = c("GARCH", "GJR-GARCH", "5%- and 95%-line (GARCH)", "5%- and 95%-line (GJR)"), 
          lty=c(1, 1, 2, 2), lwd=c(3, 3, 1, 1),
          col=c(1,2,1,2), cex = 0.8)

# Signum trading if volatility prediction significantly different 

# GARCH
upper_garch <- quantile(vola_mat$GARCH_Volatility, 0.95)
bottom_garch <- quantile(vola_mat$GARCH_Volatility, 0.05)

signals_sell <- (vola_mat$GARCH_Volatility > upper) * -1
signals_buy <- (vola_mat$GARCH_Volatility < bottom) * 1
signals_garch <- signals_sell + signals_buy

# GJR-GARCH
upper_gjr <- quantile(vola_mat$GJR_Volatility, 0.95)
bottom_gjr <- quantile(vola_mat$GJR_Volatility, 0.05)

signals_sell <- (vola_mat$GJR_Volatility > upper_gjr) * -1
signals_buy <- (vola_mat$GJR_Volatility < bottom_gjr) * 1
signals_gjr <- signals_sell + signals_buy











# Load
source("add/libraries.R")
source("add/Functions.R")
load("data/BTC_USD_27_03_21.rda")
load("data/log_ret_27_03_21.rda")

##### Volatility prediction for trading signals: Method 1 using fGArch #####
# Using functions from fGarch package

# Data prep prices
BTC <- BTC_USD_27_03_21$`BTC-USD.Close`
BTC <- BTC[-1,]
BTC <- na.omit(BTC)

# Data prep log returns
dat <- log_ret_27_03_21
colnames(dat) <- "BTC log returns"
head(dat)

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)
dat <- na.omit(dat)

# Define date from which we start to predict volatilities
# Define index for dat (class = data frame)

split <- as.Date("2020-05-01")

ind_1 <- index(dat)[which(rownames(dat) == split)]
dat[ind_1,]

# GARCH fit

GARCH_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))

# GARCH predictions
# Rolling window 

mod = ugarchroll(GARCH_spec, data = dat, n.ahead = 1, 
                 n.start = ind_1,  refit.every = 1, refit.window = "recursive", 
                  fit.control = list(),
                 calculate.VaR = TRUE, VaR.alpha = 0.05,
                 keep.coef = TRUE)

GARCH_vola <- mod@forecast$density

#save(GARCH_vola, file = "data/GARCH_vola_predictions/GARCH_vola_predictions.rda")








##### Trading performance of GARCH only #####

# Load data

# saving garch volas for the 9 split out of sample from 1.7.20- end
# garch_out_signal=as.xts(GARCH_vola)$Trading_signal["2020-07-01::"]
# save(garch_out_signal, file = "data/GARCH_vola_predictions/garch_out_signal.rda")
# 




load("data/GARCH_vola_predictions/GARCH_vola_predictions.rda")

# Check trading performance
split <- as.Date("2020-05-01")

ind_0 <- index(dat)[which(rownames(dat) == split)]
ind_1 <- nrow(dat)

perf <- GARCH_vola$Trading_signal * dat_xts[(ind_0+1):ind_1]
sharpe<-sqrt(365)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))

plot(cumsum(perf), main = "Trading performance GARCH(1,1)")

