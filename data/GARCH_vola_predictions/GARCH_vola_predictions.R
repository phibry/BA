##### Volatility prediction using GARCH(1,1) ####
# Windows size 365 days
# Rolling window with refit after every months

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
dat_xts <- dat <- log_ret_27_03_21
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

mod <- ugarchroll(GARCH_spec, data = dat, n.ahead = 1, 
                 n.start = ind_1,  refit.every = 30, refit.window = "moving", 
                 window.size = 365, 
                 fit.control = list(),
                 calculate.VaR = FALSE,
                 keep.coef = TRUE)

plot(mod)

# Save results in dataframe

GARCH_vola <- mod@forecast$density

# Adding trading signals with threshold upper CI = 0.95

th <- quantile(GARCH_vola$Sigma, p = 0.95)

for(i in 1:nrow(GARCH_vola)){
  if(GARCH_vola$Sigma[i] >= th){
    GARCH_vola$Trading_signal[i] <- 0
  }
  else{
    GARCH_vola$Trading_signal[i] <- 1
  }
}

# Save as .rda file

#save(GARCH_vola, file = "data/GARCH_vola_predictions/GARCH_vola_predictions.rda")

# Check trading performance

ind_0 <- index(dat)[which(rownames(dat) == split)]
ind_1 <- nrow(dat)

perf <- GARCH_vola$Trading_signal * dat_xts[(ind_0+1):ind_1]
sharpe<-sqrt(365)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))

plot(cumsum(perf), main = "Trading performance GARCH(1,1)")

