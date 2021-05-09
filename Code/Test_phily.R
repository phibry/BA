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

<<<<<<< HEAD
# Last Period####jkfj
# Test: Last Month
# Training: Last 6 Months
data_obj <- data_function(x=logret, lags=6, in_out_sep="2021-03-01", start="2020-09-01", end="2021-03-27")
nn_7_7_50_last <- nn_nl_comb_sharpe_mse(maxneuron=7,
                                    maxlayer=7,
                                    real=50,
                                    data_obj=data_obj)
# Don't do that
# 960799 nets * 50 Iterations = 48039950
# 9.2h Computation for 16482 of 960799 (2%)
=======
split3 <- logret["2020-03-01::2020-09-30"]
split3_in <- split3["2020-03-01::2020-08-31"]
split3_out <- split3["2020-09-01::2020-09-30"]
>>>>>>> main

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

para <- 4
arma_obj <- arima(insamp, order=c(para,0,0))
mat_out <- cbind(split9_out)

for (k in 1:para) {
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





# Test Pascal
data_function <- function(x, lags, in_out_sep, start="", end="",autoassign=F) {
  # Define startpoints
  x <- x[paste(start,"::", end, sep="")]
  data_mat <- x
  
  # Create lagged data
  for (j in 1:lags)
    data_mat <- cbind(data_mat, lag(x, k=j))
  
  # Remove NA's
  data_mat <- na.exclude(data_mat)
  
  
  # Specify in- and out-of-sample episodes
  # Target in-sample (current data)
  target_in <- data_mat[paste("/",in_out_sep,sep=""),1]
  # Remove last value
  target_in <- target_in[1:(length(target_in)-1),1]
  
  # Target out of sample (current data)
  target_out <- data_mat[paste(in_out_sep,"/",sep=""),1]
  
  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)
  
  train_set_xts <- scaled[paste("/",in_out_sep,sep=""),]
  test_set_xts <- scaled[paste(in_out_sep,"/",sep=""),]
  # Train-test split
  train_set <- scaled[paste("/",in_out_sep,sep=""),]
  # Remove last value
  train_set <- train_set[1:(dim(train_set)[1]-1),]
  
  test_set <- scaled[paste(in_out_sep,"/",sep=""),]
  
  train_set <- as.matrix(train_set)
  test_set <- as.matrix(test_set)
  
  # Formula
  colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
  n <- colnames(train_set)
  f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
  
  if(autoassign)
  {
    assign("data_mat",data_mat,.GlobalEnv)
    assign("target_in",target_in,.GlobalEnv)
    assign("target_out",target_out,.GlobalEnv)
    assign("train_set",train_set,.GlobalEnv)
    assign("test_set",test_set,.GlobalEnv)
    assign("train_set_xts",train_set_xts,.GlobalEnv)
    assign("test_set_xts",test_set_xts,.GlobalEnv)
    assign("f",f,.GlobalEnv)
  }
  
  
  return(list(data_mat=data_mat,
              target_in=target_in,
              target_out=target_out,
              train_set=train_set,
              test_set=test_set,
              f=f))
}


## Estimate Fun####
nn_estim <- function(data_obj, nl_comb) {
  
  # Prepare data
  train_set <- data_obj$train_set
  test_set <- data_obj$test_set
  data_mat <- data_obj$data_mat
  target_in <- data_obj$target_in
  target_out <- data_obj$target_out
  f <- as.formula(data_obj$f)
  
  
  # Train NeuralNet
  nn <- neuralnet(f, data=train_set, hidden=nl_comb, linear.output=T, stepmax = 1e+08)
  
  
  # In sample performance
  pred_in_scaled <- nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  pred_in <- pred_in_scaled*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # In-sample MSE
  train_rescaled <- train_set[,1]*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  mse_in <- mean((train_rescaled - pred_in)^2)
  
  # In-sample Sharpe
  perf_in <- (sign(pred_in))*target_in
  sharpe_in <- as.numeric(sqrt(365)*mean(perf_in)/sqrt(var(perf_in)))
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
  # pr.nn <- compute(nn, as.matrix(test_set[,2:ncol(test_set)]))
  pr.nn <- predict(nn, as.matrix(test_set[,2:ncol(test_set)]))
  
  predicted_scaled <- pr.nn
  # Results from NN are normalized (scaled)
  # Descaling for comparison
  pred_out <- predicted_scaled*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  test_rescaled <- test_set[,1]*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # Calculating MSE
  mse_out <- mean((test_rescaled - pred_out)^2)
  
  # Out of sample Sharpe
  perf_out <- (sign(pred_out))*target_out
  sharpe_out <- sqrt(365)*mean(perf_out)/sqrt(var(perf_out))
  
  # Compare in-sample and out-of-sample
  mse_nn <- c(mse_in, mse_out)
  sharpe_nn <- c(sharpe_in, sharpe_out)
  
  return(list(mse_nn=mse_nn, pred_out=pred_out, pred_in=pred_in, sharpe_nn=sharpe_nn))
}

logret["2020-01-01::2021-03-27"]
testi <- data_function(logret, 7, in_out_sep="2021-02-27", start="2020-01-01", end="2021-03-27")
testi$target_in
testi$target_out

resi1 <- nn_estim(testi, c(3,2))
resi2 <- nn_estim(testi, c(3,2))

par(mfrow=c(3,1))
plot(as.xts(rbind(resi1$pred_in, resi1$pred_out)), type="l")
plot(as.xts(rbind(resi2$pred_in, resi2$pred_out)), type="l")
plot(logret["2020-01-08::2021-03-27"], type="l")








# # von Wildi####
# transform_OLPD_back_original_data_func <- function(data_xts, data_mat, OLPD_scaled_mat, lm_obj, data) {
#   # Make xts-object (not trivial in this case because of monthly dates...)
#   OLPD_mat <- data_xts
#   for (i in 1:nrow(OLPD_scaled_mat))
#     OLPD_mat[i,] <- OLPD_scaled_mat[i,]
#   OLPD_scaled_mat <- OLPD_mat
#   is.xts(OLPD_mat)
#   colnames(OLPD_mat) <- c("intercept",colnames(data_xts)[2:ncol(data_xts)])
#   
#   # Transform back to original log-returns: the regression weights are not affected in this case because target and explanatory are scaled by the same constant: we nevertheless apply the (identity) scaling to be able to work in more general settings
#   for (j in 2:ncol(OLPD_mat))
#     OLPD_mat[,j] <- OLPD_scaled_mat[,j]*(max(data_mat[,1])-min(data_mat[,1]))/(max(data_mat[,j])-min(data_mat[,j]))
#   # The intercept is affected
#   #   -We center the intercept: variations about its mean value
#   #   -We scale these variations: divide by scale of transformed and multiply by scale of log-returns
#   #   -Add intercept from original regression
#   OLPD_mat[,1] <- (OLPD_scaled_mat[,1]-mean(OLPD_scaled_mat[,1],na.rm=T))*((max(data_mat[,1])-min(data_mat[,1]))/(max(data[,1])-min(data[,1]))) +lm_obj$coefficients[1]
#   
#   return(list(OLPD_mat=OLPD_mat,OLPD_scaled_mat=OLPD_scaled_mat))
# }
# OLPD_func <- function(x, delta, epsilon, nn) {
#   try_data_list <- try(out_original <- predict(nn,x), silent=T)
#   
#   if(class(try_data_list)[1]=="try-error") {
#     data_list <- vector(mode="list")
#     print("Neural net singular")
#     effect <- NULL
#     return(list(effect=effect))
#     
#   } else {
#     
#     
#     
#     # For each explanatory...
#     for (i in 1:ncol(x)) {
#       # y will be the original explanatory plus an infinitesimal perturbation of i-th explanatory
#       y <- x
#       y[,i]<-y[,i]+delta*x[,i]
#       
#       # Generate infinitesimally perturbated output
#       out_i <-predict(nn,y)
#       
#       if (i==1) {
#         effect<-(out_i-out_original)/(delta*x[,i])
#       } else {
#         effect<-c(effect,(out_i-out_original)/(delta*x[,i]))
#       }
#       # Collect for each explanatory the perturbated data and the corresponding nn-output
# 
#     }
#     # Virtual intercept: output of neural net minus linear regression part
#     virt_int <- out_original-as.double(x%*%effect)
#     effect <- c(virt_int,effect)
#     
#     
#     # Fit the regression to the noiseless perturbated data: as many observations as unknowns i.e. zero-residual
#     return(list(effect=effect))
#   }
# }
# 
# # in_out_sample_separator="2021-05-05"; neuron_vec=c(7,7)
# OLPDphil <- function(in_out_sample_separator,data_mat,use_in_samp=T,neuron_vec) {
# 
#   # Regression
#   reg_data <- data_mat
#   colnames(reg_data) <- paste("lag",0:(ncol(reg_data)-1),sep="")
#   lm_obj <- lm(lag0~., data=reg_data)
#   
#   # Scaling data for the NN
#   maxs <- apply(data_mat, 2, max)
#   mins <- apply(data_mat, 2, min)
#   
#   # Transform data into [0,1]
#   scaled <- scale(data_mat, center = mins, scale = maxs - mins)
#   
#   # Train-test split
#   train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
#   test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]
#   
#   # Transform to matrix
#   train_set <- as.matrix(train_set_xts)
#   test_set <- as.matrix(test_set_xts)
#   
#   # Change colnames
#   colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
#   
#   n <- colnames(train_set)
#   # Model: target is current GOOGLE, all other variables are explanatory
#   f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
#   
#   # Train neural net
#   nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=F)
#   
#   # Original linear parameter data
#   #   Generate new data from original data
#   #   New data: in each time point compute the parameters of the exact infinitesimal linear regression model
#   # Induce infinitesimal perturbations to data and fit regression to output
#   delta <- 1.e-5
#   epsilon <- 1.e-4
#   
#   
#   if (use_in_samp) {
#     # Smoother (in-sample data)
#     data <- train_set
#     data_xts <- train_set_xts
#   } else {
#     # Rougher (out-sample data)
#     data <- test_set
#     data_xts <- test_set_xts
#   }
# 
#   pb <- txtProgressBar(min = 1, max = (nrow(data)-1), style = 3)
#   for (i in 1:(nrow(data))) {
#     x <- matrix(data[i,2:ncol(data)], nrow=1)
#     colnames(x) <- colnames(data)[2:ncol(data)]
#     
#     # Wie OLPD-func
#     OLPD_scaled_obj <- OLPD_func(x, delta, epsilon, nn)
# 
#     if (i==1) {
#       OLPD_scaled_mat <- OLPD_scaled_obj$effect
#     } else {
#       OLPD_scaled_mat <- rbind(OLPD_scaled_mat,OLPD_scaled_obj$effect)
#     }
#     setTxtProgressBar(pb, i)
# 
#   }
#   close(pb)
# 
#   # Transform data back to its original form
#   OLPD_mat_obj <- transform_OLPD_back_original_data_func(data_xts, data_mat, OLPD_scaled_mat, lm_obj, data)
# 
#   OLPD_mat <- OLPD_mat_obj$OLPD_mat
#   
#   index(OLPD_mat) <- index(data_xts)
#   colnames(OLPD_mat)[1] <- "(Intercept)"
#   colnames(OLPD_mat)[2:ncol(OLPD_mat)] <- paste("lag",1:(ncol(train_set)-1),sep="")
#   
# 
#   return(list(OLPD_mat=OLPD_mat, lm_obj=lm_obj))
# }




# XAI-GOOGLE####
getSymbols("GOOGL")
tail(GOOGL)
save(GOOGL, file = "data/GOOGL.rda")
save("google_dat.Rdat")
g.adj <- GOOGL[,6]
par(mfrow=c(1,1))
plot(g.adj, main="Adjusted Prices ~ Google")
logi_googi <- diff(log(g.adj))
chart.ACF.phil(logi_googi)
chart.ACFplus.phil(logi_googi, ymax=0.1, main="GOOGLE price dependency structure")

par(mfrow=c(1,1))
chart.ACF.phil(logi_googi, ymax=0.1)
# lag1, 6, 7, 12

# XAI - BTC
load("data/log_ret_27_03_21.rda")
load("data/BTC_USD_27_03_21.rda")
btc <- BTC_USD_27_03_21$`BTC-USD.Adjusted`
logret <- log_ret_27_03_21

par(mfrow=c(1,1))
chart.ACF.phil(logret, ymax=0.06, maxlag = 20, main="BTC Price Dependency Structure")
chart.ACFplus.phil(logret, ymax=0.06, main="BTC price dependency structure")
# lag 6 und lag 10

## Data Prep####
par(mfrow=c(2,1))
plot(logret, main="Bitcoin")
plot(btc, main="Bitcoin")

x <- logret

data_mat <- cbind(x,
                  lag(x),
                  lag(x,k=2),
                  lag(x,k=3),
                  lag(x,k=4),
                  lag(x,k=5),
                  lag(x,k=6),
                  lag(x,k=7),
                  lag(x,k=8),
                  lag(x,k=9),
                  lag(x,k=10))

# Check length of time series before na.exclude
dim(data_mat)
data_mat <- na.exclude(data_mat)
# Check length of time series after removal of NAs
dim(data_mat)
head(data_mat)
tail(data_mat)


# 100x####
# res_mat <- 0
# for (i in 1:100) {
#   res <- na.exclude(OLPDphil("2021-03-27", data_mat, use_in_samp=TRUE, c(7,7))$OLPD_mat)
#   res_mat <- res_mat + res
# }

# 1:
# res_mat1 <- res_mat

resi <- OLPDphil("2021-03-27", data_mat, use_in_samp=TRUE, c(7,7))
# par(mfrow=c(1,1))
# plot(res_mat, main="")
# 
# n <- n + 1
# # res <- res_mat
# res <- res + res_mat
# # res <- res/n
# 
# # res10 <- res

reg_data <- data_mat
colnames(reg_data) <- paste("lag",0:(ncol(reg_data)-1),sep="")
lm_obj <- lm(lag0~., data=reg_data)
xai_lm <- lm_obj

par(mfrow=c(2,1))
# colorino <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#f6004a", "#ff1208", "#ef4e3d", "#ffa600", "#ff7c43")
colorino <- c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#f6004a", "#004c6d", "#0075b6", "#665191", "#ff7c43")
plot(xai_data, main="XAI ~ Bitcoin", col=colorino)
for (i in c(7,8,9,10,11))
  mtext(colnames(xai_data)[i], col=colorino[i], line=-i)
plot(logret, main="LogReturn ~ Bitcoin")

apply(na.omit(xai_data), 2, mean)[c(7,8,9,10,11)]
coef(xai_lm)[c(7,8,9,10,11)]

# Test
# test1 <- na.exclude(OLPDphil("2021-03-27", data_mat, use_in_samp=TRUE, c(7,7))$OLPD_mat)


# xai_data <- res
# save(xai_data, file = "data/xai_data.rda")
# save(xai_lm, file = "data/xai_lm.rda")
load("data/xai_data.rda")
load("data/xai_lm.rda")
xai_lm$coefficients

# xai_mat <- xai_data$OLPD_mat
# xai_lm <- xai_data$lm_obj
# par(mfrow=c(1,1))
# # colorino <- c("#003f5c", "#2f4b7c", "#520065", "#860064", "#a05195", "#665191", "#f95d6a", "#d45087", "#ff7c43", "#d6204f", "#ef4e3d", "#ffa600")
# colorino <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#f6004a", "#ff1208", "#ef4e3d", "#ffa600", "#ff7c43")
# par(mfrow=c(1,1))
# plot(xai_mat, main="XAI ~ Bitcoin", col=colorino)
# for (i in 1:ncol(xai_mat))
#   mtext(colnames(xai_mat)[i], col=colorino[i], line=-i)
# 
# 
# options(scipen = 999)
# coef(xai_lm)
# apply(na.omit(xai_mat), 2, mean)
# coef(xai_lm)[c(7, 11)]
# apply(na.omit(xai_mat), 2, mean)[c(7, 11)]
# options(scipen = 0)
# 
# 
# par(mfrow=c(2,1))
# colors <- colorino[c(1,6)]
# select_acf <- test2$OLPD_mat[, c(7, 11)]
# plot(select_acf, main="XAI ~ Bitcoin", col=colors)
# for (i in 1:ncol(select_acf))
#   mtext(colnames(select_acf)[i], col=colors[i], line=-i)
# plot(logret, main="Adjusted log(Prices) ~ Bitcoin")
# 
# summary(na.exclude(test2$OLPD_mat[, 7]))
# summary(na.exclude(test2$OLPD_mat[, 11]))
# 
# 
# par(mfrow=c(2,1))
# plot(test2$OLPD_mat, main="XAI ~ Bitcoin", col=colorino)
# for (i in 1:ncol(test2$OLPD_mat))
#   mtext(colnames(test2$OLPD_mat)[i], col=colorino[i], line=-i)
# plot(logret, main="Adjusted log(Prices) ~ Bitcoin")


#003f5c
#2f4b7c
#665191
#a05195
#d45087
#f95d6a
#ff7c43
#f6004a
#ff1208
#ffa600

par(mfrow=c(1,1))
# colorino <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#f6004a", "#ff1208", "#ef4e3d", "#ffa600", "#ff7c43")
colorino <- c("#ff1208", "#003f5c", "#2f4b7c", "#a05195", "#d45087", "#f95d6a", "#f6004a", "#004c6d", "#0075b6", "#665191", "#ff7c43")
# colorino <- c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#f6004a", "#004c6d", "#0075b6", "#665191", "#ff7c43")
plot(xai_data, main="XAI ~ Bitcoin", col=colorino)
for (i in 1:ncol(xai_data))
  mtext(colnames(xai_data)[i], col=colorino[i], line=-i)

class(xai_data)

xai_dat <- as.data.frame(xai_data)
class(xai_dat)

plot(xai_dat[,1], type="l", ylim=c(min(na.exclude(xai_dat)), na.exclude(max(xai_dat))))
c(min(na.exclude(xai_dat)), na.exclude(max(xai_dat)))

# Table####
as.numeric(round(xai_lm$coefficients,4))

name <- c("Intercept", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5",
          "Lag 6", "Lag 7", "Lag 8", "Lag 9", "Lag 10")  
z <- as.numeric(round(xai_lm$coefficients,4))
df <- as.data.frame(rbind(name,z))
colnames(df) <- NULL
rownames(df) <- c("Coefficient", "Value")
df
