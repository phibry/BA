# Load
source("add/libraries.R")
source("add/Functions.R")
load("data/log_ret_27_03_21.rda")
load("data/BTC_USD_27_03_21.rda")

##### Volatility prediction for trading signals: Method 1 using fGArch #####
# Using functions from fGarch package

# Data prep prices
BTC <- BTC_USD_27_03_21$`BTC-USD.Close`
BTC <- as.data.frame(BTC)
BTC <- na.omit(BTC)

# Data prep log returns
dat <- log_ret_27_03_21
head(dat)

# Separation of data
sep <- "2020-05-01"
dat[as.Date(sep)]

# Set window size
window <- 365

# Start prediction using package rugarch
# Set window range to 365 days and iterate daily

# Just to inform: Window for first iteration
as.Date(sep)-window # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep)-window)]
as.Date(sep) # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep))]

# Just to inform: Window for last iteration
as.Date("2021-03-26")-window
as.Date("2021-03-26")

# Predictions with rolling window one-step-ahead forecasts
# Defining left border of rolling window in the beginning (date_0) and at the end (date_n)
date_0 <- as.Date(sep)-window
date_n <- as.Date("2021-03-26")-window
datum <- date_0

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)
dat <- na.omit(dat)

# Define prediction matrix
vola_mat <- data.frame(Date = seq((as.Date(sep)+1), as.Date("2021-03-27"), by = "days"), 
                       GARCH_Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1),
                       GJR_Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1))

# One-step-ahead forecasts
j <- 1

while(datum <= date_n){
  # Estimation of GARCH model
  garch_fit <- garchFit(~ garch(1,1), include.mean = T, data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)])
                                                                                   :(index(dat)[which(row.names(dat) == datum + window)])],
                        trace = FALSE, cond.dist = "QMLE")
  
  # Estimation of GJR Garch model
  GJR_spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
  GJR_fit <- ugarchfit(data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)])
                                                  :(index(dat)[which(row.names(dat) == datum + window)])], spec = GJR_spec, trace = FALSE)
  
  
  print(paste("Iteration: ", j, " Date: ", datum, "End: ", datum + window))
  datum <- datum + 1
  
  # Predictions
  vola_mat$GARCH_Volatility[j] <- predict(garch_fit, n.ahead = 1)$standardDeviation
  vola_mat$GJR_Volatility[j] <- sigma(ugarchforecast(GJR_fit, n.ahead = 1))
  j <- j + 1
}


# Plot of log prices, log returns and predicted volatilities
par(mfrow = c(3,1))

# Plotting log prices (easier to compare relative changes)
plot(log(BTC$`BTC-USD.Close`[(index(BTC)[which(row.names(BTC) == "2020-05-02")]
                              :(index(BTC)[which(row.names(BTC) == "2021-03-27")]))]), type = "l",
     main = "Bitcoin log prices", ylab = "Log prices")

# Plotting log returns
plot(dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == "2020-05-02")]
                          :(index(dat)[which(row.names(dat) == "2021-03-27")]))], type = "l",
     main = "Bitcoin log returns", ylab = "Log return")


# Plotting predicted volatility
vola_mat <- na.omit(vola_mat)
plot(vola_mat$GARCH_Volatility ~ vola_mat$Date, xaxt = "n", type = "l", main = "Predicted volatility measured by SD",
     xlab = "Time", ylab = "SD")
lines(vola_mat$GJR_Volatility ~ vola_mat$Date, type = "l", col = 2)
axis(1, vola_mat$Date, format(vola_mat$Date, "%Y-%m-%d"))
abline(h = median(vola_mat$GARCH_Volatility), col = 4)
legend("topleft", c("GARCH", "GJR-GARCH", "Median"), col = c(1,2,4), lty = 1, cex = 0.4)














##### Volatility prediction for trading signals: Method 2 using rugarch #####

# Data prep
dat <- log_ret_27_03_21
head(dat)

# Separation of data
sep <- "2020-05-01"
dat[as.Date(sep)]

# Set window size
window <- 365

# Start prediction using package rugarch
# Set window range to 365 days and iterate daily

# Just to inform: Window for first iteration
as.Date(sep)-window # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep)-window)]
as.Date(sep) # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep))]

# Just to inform: Window for last iteration
as.Date("2021-03-26")-window
as.Date("2021-03-26")

# Predictions with rolling window one-step-ahead forecasts
# Defining left border of rolling window in the beginning (date_0) and at the end (date_n)
date_0 <- as.Date(sep)-window
date_n <- as.Date("2021-03-26")-window
datum <- date_0

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)

# Define prediction matrix
vola_mat <- data.frame(Date = NA, Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1))
?data.frame
# Specify using ugarchspec() from rugarch package
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)))

# One-step-ahead forecasts
j <- 1

while(datum <= date_n){
  # Estimation of model
  garch_fit <- ugarchfit(spec = garch_spec, 
                         data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)])
                                                    :(index(dat)[which(row.names(dat) == datum + window)])
                                                    ]) # or datum + window?
  
  print(paste("Iteration: ", j, " Date: ", datum, "End: ", datum + window))
  datum <- datum + 1
  
  # Predictions
  vola_mat$Date[j] <- as.Date(datum + window + 1, format = "%Y-%m-%d")
  vola_mat$Volatility[j] <- sigma(ugarchforecast(fitORspec = garch_fit, n.ahead = 1))
  j <- j + 1
}

plot(vola_mat, type = "l")






