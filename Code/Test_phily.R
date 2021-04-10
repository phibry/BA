# Imports####
source("add/libraries.r") 
# source("add/Functions.r")


# Load Data####
source("add/libraries.r") 
load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21

tail(logret)

# Data Function
data_function<-function(x, lags, in_out_sep, start="", end="") {
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

  # Target out of sample (current data)
  target_out <- data_mat[paste(in_out_sep,"/",sep=""),1]
  
  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)
  

  # Train-test split
  train_set <- scaled[paste("/",in_out_sep,sep=""),]
  test_set <- scaled[paste(in_out_sep,"/",sep=""),]
  
  train_set <- as.matrix(train_set)
  test_set <- as.matrix(test_set)
  
  # Formula
  colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
  n <- colnames(train_set)
  f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
  
  return(list(data_mat=data_mat,
              target_in=target_in,
              target_out=target_out,
              train_set=train_set,
              test_set=test_set,
              f=f))
}

data_obj <- data_function(x=logret, lags=6, in_out_sep="2020-05-01", start="2018-01-01", end="")

# Estimate Fun####
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

# Input Grid Function####
input_grid <- function(n=3, l=3) {
  anz <- n^(1:l)
  mat <- matrix(0, nrow=sum(anz), ncol=l)
  
  
  i_end <- cumsum(anz)
  i_start <- anz-1
  i_start <- i_end - i_start
  
  
  for(j in 0:(length(anz)-1)) {
    for (i in (1+j):l) {
      mat[i_start[i]:i_end[i], i-j] <- rep(1:n, rep(n^(j), n))
    }
  }
  return(as.data.frame(mat))
}

# MSE Fun####
nn_nl_comb_sharpe_mse <- function(maxneuron=3, maxlayer=3, real=10, data_obj) {
  starttime=Sys.time()
  # Define Input Grid
  # needs input grid function
  combmat <- input_grid(maxneuron,maxlayer)
  
  
  # Naming the  grid with combinations
  ind <- rep(NA,dim(combmat)[1])
  for(k in 1:dim(combmat)[1])
  {
    x <- as.vector(combmat[k,])
    ind[k] <- toString(as.character(x[x!=0]))
  }
  
  # Define result matrix
  mati <- matrix(nrow=dim(combmat)[1], ncol=real*4, 0)
  mati <- as.data.frame(mati)
  rownames(mati) <- ind
  
  
  #creating , testing , neural net
  for( i in 1: dim(combmat)[1]) {
    pb <- txtProgressBar(min = 1, max = dim(combmat)[1], style = 3)
    
    x=as.vector(combmat[i,])
    x= x[x!=0]
    
    for(k in seq(1,real*4,4)) {

      net <- nn_estim(data_obj, nl_comb=x)
      
      mati[i, k:(k+3)] <- c(net$mse_nn, net$sharpe_nn)
    }
    
    print(paste("Elapsed Time: " ,Sys.time()-starttime))
    print(paste("Iteration: ", i, "of", dim(combmat)[1]))
    setTxtProgressBar(pb, i)
    
  }
  
  close(pb)
  print(paste("Overall Time: " ,Sys.time()-starttime))
  return(mati)
}

# All in One:####
nn_12_2_50 <- nn_nl_comb_sharpe_mse(maxneuron=12,
                              maxlayer=2,
                              real=50,
                              data_obj=data_obj)

# save(nn_12_2_50, file = "data/nn_12_2_50.rda") # 1.64h
# save(nn_5_5_50, file = "data/nn_5_5_50.rda") # 1.75h
# save(nn_10_3_40, file = "data/nn_10_3_40.rda") # 2.56h
# save(nn_5_5_20, file = "data/nn_5_5_20.rda") # 40min
# save(nn_5_2_20, file = "data/nn_5_2_20.rda") # 10min

#.####
# Plot Sharpe~MSE####
# nn_5_2_20####
## Sharpe####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_5_2_20[,seq(3, 80, 4)])
maxi <- max(nn_5_2_20[,seq(3, 80, 4)])
color <- 1
plot(nn_5_2_20[,3], type="l", main="Sharpe-In", col=color, ylim=c(mini,maxi), ylab="Sharpe")
color <- color + 1
for (i in seq(7, 80, 4)) {
  lines(nn_5_2_20[,i], col=color)
  color <- color + 1
}

# Out-of-Sample
mini <- min(nn_5_2_20[,seq(4, 80, 4)])
maxi <- max(nn_5_2_20[,seq(4, 80, 4)])
color <- 1
plot(nn_5_2_20[,4], type="l", main="Sharpe-Out", col=color, ylim=c(mini,maxi), ylab="Sharpe")
color <- color + 1
for (i in seq(8, 80, 4)) {
  lines(nn_5_2_20[,i], col=color)
  color <- color + 1
}

## MSE####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_5_2_20[,seq(1, 80, 4)])
maxi <- max(nn_5_2_20[,seq(1, 80, 4)])
color <- 1
plot(nn_5_2_20[,1], type="l", main="MSE-In", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(5, 80, 4)) {
  lines(nn_5_2_20[,i], col=color)
  color <- color + 1
}

# Out-of-Sample
mini <- min(nn_5_2_20[,seq(2, 80, 4)])
maxi <- max(nn_5_2_20[,seq(2, 80, 4)])
color <- 1
plot(nn_5_2_20[,2], type="l", main="MSE-Out", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(6, 80, 4)) {
  lines(nn_5_2_20[,i], col=color)
  color <- color + 1
}

#.####
# nn_5_5_20####
## Sharpe####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_5_5_20[,seq(3, 80, 4)])
maxi <- max(nn_5_5_20[,seq(3, 80, 4)])
color <- 1
plot(nn_5_5_20[,3], type="l", main="Sharpe-In", col=color, ylim=c(mini,maxi), ylab="Sharpe")
color <- color + 1
for (i in seq(7, 80, 4)) {
  lines(nn_5_5_20[,i], col=color)
  color <- color + 1
}

# Out-of-Sample
mini <- min(nn_5_5_20[,seq(4, 80, 4)])
maxi <- max(nn_5_5_20[,seq(4, 80, 4)])
color <- 1
plot(nn_5_5_20[,4], type="l", main="Sharpe-Out", col=color, ylim=c(mini,maxi), ylab="Sharpe")
color <- color + 1
for (i in seq(8, 80, 4)) {
  lines(nn_5_5_20[,i], col=color)
  color <- color + 1
}

## MSE####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_5_5_20[,seq(1, 80, 4)])
maxi <- max(nn_5_5_20[,seq(1, 80, 4)])
color <- 1
plot(nn_5_5_20[,1], type="l", main="MSE-In", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(5, 80, 4)) {
  lines(nn_5_5_20[,i], col=color)
  color <- color + 1
}

# Out-of-Sample
mini <- min(nn_5_5_20[,seq(2, 80, 4)])
maxi <- max(nn_5_5_20[,seq(2, 80, 4)])
color <- 1
plot(nn_5_5_20[,2], type="l", main="MSE-Out", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(6, 80, 4)) {
  lines(nn_5_5_20[,i], col=color)
  color <- color + 1
}


#.####
# nn_10_3_40####
## Sharpe####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_10_3_40[,seq(3, 160, 4)])
maxi <- max(nn_10_3_40[,seq(3, 160, 4)])
color <- 1
plot(nn_10_3_40[,3], type="l", main="Sharpe-In", col=color, ylim=c(mini,maxi), ylab="Sharpe", lwd=0.5)
color <- color + 1
for (i in seq(7, 160, 4)) {
  lines(nn_10_3_40[,i], col=color, lwd=0.5)
  color <- color + 1
}
grid()

# Out-of-Sample
mini <- min(nn_10_3_40[,seq(4, 160, 4)])
maxi <- max(nn_10_3_40[,seq(4, 160, 4)])
color <- 1
plot(nn_10_3_40[,4], type="l", main="Sharpe-Out", col=color, ylim=c(mini,maxi), ylab="Sharpe", lwd=0.5)
color <- color + 1
for (i in seq(8, 160, 4)) {
  lines(nn_10_3_40[,i], col=color, lwd=0.5)
  color <- color + 1
}

## MSE####
par(mfrow=c(2,1))
# In-Sample
mini <- min(nn_10_3_40[,seq(1, 80, 4)])
maxi <- max(nn_10_3_40[,seq(1, 80, 4)])
color <- 1
plot(nn_10_3_40[,1], type="l", main="MSE-In", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(5, 80, 4)) {
  lines(nn_10_3_40[,i], col=color)
  color <- color + 1
}

# Out-of-Sample
mini <- min(nn_10_3_40[,seq(2, 80, 4)])
maxi <- max(nn_10_3_40[,seq(2, 80, 4)])
color <- 1
plot(nn_10_3_40[,2], type="l", main="MSE-Out", col=color, ylim=c(mini,maxi), ylab="MSE")
color <- color + 1
for (i in seq(6, 80, 4)) {
  lines(nn_10_3_40[,i], col=color)
  color <- color + 1
}




# save(optim_10_2_10, file = "data/optim_10_2_10.rda") # 27.2min
# save(optim_6_2_40, file = "data/optim_6_2_40.rda") # 12.6min
# save(optim_5_3_30, file = "data/optim_5_3_30.rda") # 13.32min
# save(optim_5_5_20, file = "data/optim_5_5_20.rda") # 1.117h
# save(optim_5_5_10, file="data/optim_5_5_10.rda")



# 3645
# nn <- neuralnet(f, data=train_set, hidden=c(3,3), linear.output=T, stepmax = 1e+08)
# 
# # In sample performance
# predicted_scaled_in_sample<-nn$net.result[[1]]
# # Scale back from interval [0,1] to original log-returns
# predicted_nn_in_sample<-predicted_scaled_in_sample*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
# # In-sample MSE
# MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max(data_mat[,1])-min(data_mat[,1])))^2)
# 
# # Out-of-sample performance
# # Compute out-of-sample forecasts
# pr.nn1 <- compute(nn, as.matrix(test_set[,2:ncol(test_set)]))
# pr.nn2 <- predict(nn, as.matrix(test_set[,2:ncol(test_set)]))
# pr.nn1$net.result
# pr.nn2[,1]
