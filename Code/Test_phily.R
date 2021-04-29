# Imports####
source("add/libraries.r") 
source("add/Functions.r")


# Load Data####
start_in <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")
end_in <- c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28")
start_out <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01")
end_out <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-27")
dates_mat <- as.data.frame(cbind(start_in, end_in, start_out, end_out))


load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21

split1 <- logret["2020-01-01::2020-07-31"]
split1_in <- split1["2020-01-01::2020-06-30"]
split1_out <- split1["2020-07-01::2020-07-31"]

split2 <- logret["2020-02-01::2020-08-31"]
split2_in <- split2["2020-02-01::2020-07-31"]
split2_out <- split2["2020-08-01::2020-08-31"]

split3 <- logret["2020-03-01::2020-09-30"]
split3_in <- split3["2020-03-01::2020-08-31"]
split3_out <- split3["2020-09-01::2020-09-30"]

split4 <- logret["2020-04-01::2020-10-31"]
split4_in <- split4["2020-04-01::2020-09-30"]
split4_out <- split4["2020-10-01::2020-10-31"]

split5 <- logret["2020-05-01::2020-11-30"]
split5_in <- split5["2020-05-01::2020-10-31"]
split5_out <- split5["2020-11-01::2020-11-30"]

split6 <- logret["2020-06-01::2020-12-31"]
split6_in <- split6["2020-06-01::2020-11-30"]
split6_out <- split6["2020-12-01::2020-12-31"]

split7 <- logret["2020-07-01::2021-01-31"]
split7_in <- split7["2020-07-01::2020-12-31"]
split7_out <- split7["2021-01-01::2021-01-31"]

split8 <- logret["2020-08-01::2021-02-28"]
split8_in <- split8["2020-08-01::2021-01-31"]
split8_out <- split8["2021-02-01::2021-02-28"]

split9 <- logret["2020-09-01::2021-03-27"]
split9_in <- split9["2020-09-01::2021-02-28"]
split9_out <- split9["2021-03-01::2021-03-27"]

chart.ACF.phil(logret, maxlag = 15, main="All")
chart.ACF.phil(split1, maxlag = 15, main="Subset 1")
chart.ACF.phil(split2, maxlag = 15, main="Subset 2")
chart.ACF.phil(split3, maxlag = 15, main="Subset 3")
chart.ACF.phil(split4, maxlag = 15, main="Subset 4")
chart.ACF.phil(split5, maxlag = 15, main="Subset 5")
chart.ACF.phil(split6, maxlag = 15, main="Subset 6")
chart.ACF.phil(split7, maxlag = 15, main="Subset 7")
chart.ACF.phil(split8, maxlag = 15, main="Subset 8")
chart.ACF.phil(split9, maxlag = 15, main="Subset 9")


ind <- na.exclude(data[,1:4])
ind.lr <- na.exclude(diff(log(ind)))

ind.lr.out <- ind.lr[paste("2019-01-01","/",sep="")]

cumsum(ind.lr.out)

plot(cumsum(split1_out))

sharpe_bnh_1 <- as.double(sqrt(365) * mean(split1_out) / sqrt(var(split1_out)))
# AR-Optim####
performante_ar <- function(xin, xout, p=10) {
  # AR-Model Order p -> 1:8
  # Find best in-sample ar_obj
  # Compute out-of-of sample daily trading returns
  # Find best Sharpe
  sharpe_opt<- -9.e+99
  mse_opt <- 9.e+99
  
  res <- c()
  for (j in 1:p) {
    arma_obj <- arima(xin, order=c(j,0,0))
    mat_out <- cbind(xout)
    
    for (k in 1:(j)) {
      mat_out <- cbind(mat_out, lag(xout, k=k))
    }
    
    ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
      as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]
    
    # mse
    mse_ar <- mean(na.exclude((ar_pred - xout)^2))
    
    # sharpe
    ret_arima <- na.exclude(sign(ar_pred)*xout)
    sharpe_ar <- as.double(sqrt(365) * mean(ret_arima, na.rm=T) / sqrt(var(ret_arima, na.rm=T)))
    
    res <- rbind(res, c(sharpe_ar, mse_ar))
    
    if (sharpe_ar > sharpe_opt) {
      sharpe_opt <- as.data.frame(sharpe_ar)
      rownames(sharpe_opt) <- paste(j)
    }
    
    if (mse_ar < mse_opt) {
      mse_opt <- as.data.frame(mse_ar)
      rownames(mse_opt) <- paste(j)
    }
  }
  return(list(sharpe_ar=sharpe_opt,
              ar_sharpe_p=rownames(sharpe_opt),
              mse_ar=mse_opt,
              ar_mse_p=rownames(mse_opt),
              res=res))
}

ar_split_1 <- performante_ar(xin=split1_in, xout=split1_out, p=10)
ar_split_2 <- performante_ar(xin=split2_in, xout=split2_out, p=10)
ar_split_3 <- performante_ar(xin=split3_in, xout=split3_out, p=10)
ar_split_4 <- performante_ar(xin=split4_in, xout=split4_out, p=10)
ar_split_5 <- performante_ar(xin=split5_in, xout=split5_out, p=10)
ar_split_6 <- performante_ar(xin=split6_in, xout=split6_out, p=10)
ar_split_7 <- performante_ar(xin=split7_in, xout=split7_out, p=10)
ar_split_8 <- performante_ar(xin=split8_in, xout=split8_out, p=10)
ar_split_9 <- performante_ar(xin=split9_in, xout=split9_out, p=10)

rbind(
  c(ar_split_1$ar_sharpe_p, ar_split_1$ar_mse_p),
  c(ar_split_2$ar_sharpe_p, ar_split_2$ar_mse_p),
  c(ar_split_3$ar_sharpe_p, ar_split_3$ar_mse_p),
  c(ar_split_4$ar_sharpe_p, ar_split_4$ar_mse_p),
  c(ar_split_5$ar_sharpe_p, ar_split_5$ar_mse_p),
  c(ar_split_6$ar_sharpe_p, ar_split_6$ar_mse_p),
  c(ar_split_7$ar_sharpe_p, ar_split_7$ar_mse_p),
  c(ar_split_8$ar_sharpe_p, ar_split_8$ar_mse_p),
  c(ar_split_9$ar_sharpe_p, ar_split_9$ar_mse_p)
)


# Compare bnh to ar(4)
# split 9
sharpe_bnh_9 <- as.double(sqrt(365) * mean(split9_out[5:length(split9_out),]) / sqrt(var(split9_out[5:length(split9_out),])))



insamp <- logret["2020-01-01::2021-02-28"]
length(insamp)

chart.ACF.phil(insamp, maxlag = 15, main="All")
pacf(insamp)

arma_obj <- arima(insamp, order=c(1,0,0))
mat_out <- cbind(split9_out)

for (k in 1:1) {
  mat_out <- cbind(mat_out, lag(split9_out, k=k))
}

ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
  as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]

# sharpe
ret_arima <- na.exclude(sign(ar_pred)*split9_out)
sharpe_ar <- as.double(sqrt(365) * mean(ret_arima, na.rm=T) / sqrt(var(ret_arima, na.rm=T)))


# Plot
par(mfrow=c(2,1))
plot(cumsum(split9_out[5:length(split9_out),]), type="l", main=paste("Buy & Hold 9th Split, Sharpe:", round(sharpe_bnh_9, 3)))
plot(cumsum(ret_arima), type="l", main=paste("AR(4) 9th Split, Sharpe:", round(sharpe_ar, 3)))



as.double(sqrt(365) * mean(logret["2020-05-02::2021-03-27"]) / sqrt(var(logret["2020-05-02::2021-03-27"])))
