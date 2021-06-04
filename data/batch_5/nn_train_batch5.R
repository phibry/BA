source("add/libraries.r") 
source("add/Functions.r")
# Batch 4####

load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21

start_dates <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")
seperator <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01")
end_dates <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-27")

dates_mat <- as.data.frame(cbind(start_dates, seperator, end_dates))
dates_mat[1,][1]
dates_mat[1,][2]
dates_mat[1,][3]



chart.ACF.phil(logret["2020-01-01::2021-03-27"],  ymax = 0.12, main="ACF of time period: 2020-01-01/2021-03-27")


x_fit <- logret["2020-01-01::2021-03-27"]
y.garch_11 <- garchFit(~garch(1,1), data=x_fit, delta=2, include.delta=F, 
                       include.mean=F, trace=F)
summary(y.garch_11)

par(mfrow=c(1,1))
ts.plot(x_fit)
lines(y.garch_11@sigma.t,col="red")

# Standardisierten Residuen -> u
standard_residuals <- y.garch_11@residuals/y.garch_11@sigma.t
ts.plot(standard_residuals)

par(mfrow=c(2,1))
chart.ACF.phil(logret["2020-01-01::2021-03-27"],  ymax = 0.12, main="ACF of time period: 2020-01-01/2021-03-27")
chart.ACF.phil(standard_residuals,  ymax = 0.12, main="Acf standardized residuals GARCH(1,1)")


data_obj$f

#.####
# Training/Prediction####
## 1####
iter <- 1
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_1 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_1, file = "data/batch_5/nn_10_3_50_1.rda") # 33.62min

## 2####
iter <- 2
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_2 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_2, file = "data/batch_5/nn_10_3_50_2.rda") # 27.65min

## 3####
iter <- 3
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_3 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_3, file = "data/batch_5/nn_10_3_50_3.rda") # 30.82min

## 4####
iter <- 4
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_4 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_4, file = "data/batch_5/nn_10_3_50_4.rda") # 2.34h

## 5####
iter <- 5
data_obj$target_out
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_5 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_5, file = "data/batch_5/nn_10_3_50_5.rda") # 1.65h
data_obj_5 <- data_obj
save(data_obj_5, file = "data/data_obj_5.rda")
head(data_obj_5$target_in)
head(data_obj_5$data_mat, 5)
test <- nn_nl_comb_sharpe_mse(maxneuron=7,
                                      maxlayer=2,
                                      real=1,
                                      data_obj=data_obj)


logistic_fun <- function (x) {
  return(1/(1 + exp(-x)))
}


par(mfrow=c(1,2))
plot(data_obj_5$train_set[,1], type="l", main="")
grid()
plot(as.matrix(data_obj_5$target_in), type="l")
grid()

data_obj_5$train_set
nnt <- neuralnet(data_obj_5$f, data=data_obj_5$train_set, hidden=c(7,7), linear.output=T, stepmax = 1e+08)
nnf <- neuralnet(data_obj_5$f, data=data_obj_5$train_set, hidden=c(7,7), linear.output=F, stepmax = 1e+08)

predict(nn, as.matrix(test_set[,2:ncol(test_set)]))

as.matrix(data_obj_5$test_set[,2:ncol(data_obj_5$test_set)])

par(mfrow=c(1,4))

plot(nnt$net.result[[1]], type="l", col="blue", ylim=c(0.2,0.8))
lines(nnf$net.result[[1]], col="red")

nnf_logistic <- logistic_fun(nnf$net.result[[1]])

plot(nnf$net.result[[1]], type="l", col="red", ylim=c(0,1))
lines(nnf_logistic, col="red", lty=3)
lines(nnt$net.result[[1]], col="blue")
lines(data_obj_5$train_set[,1])
legend("topleft", legend = c("linear", "logistic(linear)", "non-linear", "original"),
       lty=c(1,3,1,1), col=c("red", "red", "blue", "black"))
nnt
nnf
nn$act.fct

nn$linear.output
nn$net.result

plot(nn$net.result[[1]], type="l")
lines(data_obj_5$train_set[,1], col="red")
lines(nn$net.result[[1]], col="blue")

## 6####
iter <- 6
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_6 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_6, file = "data/batch_5/nn_10_3_50_6.rda") # 2.15h

## 7####
iter <- 7
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_7 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_7, file = "data/batch_5/nn_10_3_50_7.rda") # 1.03h

## 8####
iter <- 8
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_8 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_8, file = "data/batch_5/nn_10_3_50_8.rda") # 1.55h

## 9####
iter <- 9
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_9 <- nn_nl_comb_sharpe_mse(maxneuron=3,
                                      maxlayer=3,
                                      real=10,
                                      data_obj=data_obj)
# save(nn_10_3_50_9, file = "data/batch_5/nn_10_3_50_9.rda") # # 3.56h



#.####
# neuralnet####

data_obj
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nl_comb <- c(3,2)

# Prepare data
train_set <- data_obj$train_set
test_set <- data_obj$test_set
data_mat <- data_obj$data_mat
target_in <- data_obj$target_in
target_out <- data_obj$target_out
f <- as.formula(data_obj$f)


# Train NeuralNet
nn <- neuralnet(f, data=train_set, hidden=nl_comb, linear.output=T, stepmax = 1e+08)
nn$net.result

# Prediction
pr.nn <- predict(nn, as.matrix(test_set[,2:ncol(test_set)]))
pr.nn



# rnn####
train_set<-data_obj$train_set
test_set<-data_obj$test_set
target_out<-data_obj$target_out
target_in<-data_obj$target_in
data_mat<-data_obj$data_mat


# This is a particular formatting of the data for rnn recurrent (it differs from keras package or mxnet)  

y_train_rnn <- as.matrix(train_set[,1])
x_train_rnn <- array(as.matrix(train_set[,2:ncol(train_set)]),dim=c(dim(as.matrix(train_set[,2:ncol(train_set)]))[1],1,dim(as.matrix(train_set[,2:ncol(train_set)]))[2]))

y_test_rnn <- as.matrix(test_set[,1])
x_test_rnn <- array(as.matrix(test_set[,2:ncol(test_set)]),dim=c(dim(as.matrix(test_set[,2:ncol(test_set)]))[1],1,dim(as.matrix(test_set[,2:ncol(test_set)]))[2]))


# specific rnn parameters
batch_size <- nrow(train_set)
network_type<-"rnn"
network_type<-"lstm"
epochs<-50
number_neurons <- c(10,10,10)

library(rnn)
fit_rnn.obj <- estimate_rnn_func(x_train_rnn, y_train_rnn, x_test_rnn, y_test_rnn,
                                 number_neurons, data_mat, batch_size, epochs, network_type)

predicted_rnn <- fit_rnn.obj$predicted_rnn

par(mfrow=c(2,1))
plot(predicted_rnn, type="l")
plot(pr.nn[,1], type="l")
pr.nn[,1]














rnn_nl_comb_sharpe_mse <- function(maxneuron=3, maxlayer=3, real=10, data_obj, epochs=10, nn_type="rnn", learningrate=0.05) {
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
      
      net <- rnn_estim(data_obj, nl_comb=x, epochs, nn_type, learningrate)
      
      
      mati[i, k:(k+3)] <- c(net$mse_nn, net$sharpe_nn)
    }
    
    print(paste("Elapsed Time: " ,Sys.time()-starttime))
    print(paste("Iteration: ", i, "of", dim(combmat)[1]))
    setTxtProgressBar(pb, i)
    
  }
  
  # close(pb)
  print(paste("Overall Time: " ,Sys.time()-starttime))
  return(mati)
}
rnn_estim <- function(data_obj, nl_comb, epochs, nn_type, learningrate) {
  
  # Prepare data
  train_set <- data_obj$train_set
  test_set <- data_obj$test_set
  target_out <- data_obj$target_out
  target_in <- data_obj$target_in
  data_mat <- data_obj$data_mat
  
  
  # This is a particular formatting of the data for rnn recurrent (it differs from keras package or mxnet)  
  
  y_train_rnn <- as.matrix(train_set[,1])
  x_train_rnn <- array(as.matrix(train_set[,2:ncol(train_set)]),dim=c(dim(as.matrix(train_set[,2:ncol(train_set)]))[1],1,dim(as.matrix(train_set[,2:ncol(train_set)]))[2]))
  
  y_test_rnn <- as.matrix(test_set[,1])
  x_test_rnn <- array(as.matrix(test_set[,2:ncol(test_set)]),dim=c(dim(as.matrix(test_set[,2:ncol(test_set)]))[1],1,dim(as.matrix(test_set[,2:ncol(test_set)]))[2]))
  
  batch_size <- nrow(train_set)
  
  # Train NeuralNet
  model <- trainr(Y = y_train_rnn,
                  X = x_train_rnn,
                  learningrate = learningrate,
                  hidden_dim = nl_comb,
                  numepochs = epochs,
                  nn_type=nn_type)
  
  # In sample performance
  pred_in_scaled <- predictr(model, x_train_rnn)
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
  pr.nn <- predictr(model, x_test_rnn)
  
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



# Test FFN####
test_ffn <- nn_nl_comb_sharpe_mse(maxneuron=3,
                                      maxlayer=3,
                                      real=10,
                                      data_obj=data_obj)
# save(test_ffn, file = "data/exploration/test_ffn.rda")
plot_all_rect(test_ffn, 10, title="FFN")

# Test RNN####
test_rnn <- rnn_nl_comb_sharpe_mse(maxneuron=3,
                                   maxlayer=3,
                                   real=10,
                                   data_obj=data_obj,
                                   epochs=10,
                                   nn_type="rnn")
# save(test_rnn, file = "data/exploration/test_rnn.rda")
plot_all_rect(test_rnn, 10, title="RNN")

# Test LSTM####
test_lstm <- rnn_nl_comb_sharpe_mse(maxneuron=3,
                                   maxlayer=3,
                                   real=10,
                                   data_obj=data_obj,
                                   epochs=10,
                                   nn_type="lstm")
# save(test_lstm, file = "data/exploration/test_lstm.rda")
plot_all_rect(test_lstm, 10, title="LSTM")

# Test GRU####
test_gru <- rnn_nl_comb_sharpe_mse(maxneuron=3,
                                    maxlayer=3,
                                    real=10,
                                    data_obj=data_obj,
                                    epochs=10,
                                    nn_type="gru")
# save(test_gru, file = "data/exploration/test_gru.rda")
plot_all_rect(test_gru, 10, title="GRU")



# Plots####
par_default <- par(no.readonly = TRUE)
par(mfrow=c(2,1), mar=c(3,5,3,2))
iter <- 1
plot(test_ffn[,iter],
     type="l",
     ylab="MSE In-Sample",
     xlab="",
     ylim=c(min(test_ffn[,iter], test_rnn[,iter], test_lstm[,iter], test_gru[,iter]),
            max(test_ffn[,iter], test_rnn[,iter], test_lstm[,iter], test_gru[,iter])))
lines(test_rnn[,iter], col=2)
lines(test_lstm[,iter], col=3)
lines(test_gru[,iter], col=4)

iter <- 2
plot(test_ffn[,iter],
     type="l",
     ylab="MSE Out-of-Sample",
     xlab="",
     ylim=c(min(test_ffn[,iter], test_rnn[,iter], test_lstm[,iter], test_gru[,iter]),
            max(test_ffn[,iter], test_rnn[,iter], test_lstm[,iter], test_gru[,iter])))
lines(test_rnn[,iter], col=2)
lines(test_lstm[,iter], col=3)
lines(test_gru[,iter], col=4)
par(par_default)
legend("center", legend = c("FFN", "RNN", "LSTM", "GRU"), lty=1, pt.cex=2, cex=0.8, bty='n',
       col = 1:4, horiz=TRUE)




# Mean####
mse_ffn_in <- apply(X=test_ffn[, seq(1, 40, 4)], MARGIN=1, FUN=mean)
mse_rnn_in <- apply(X=test_rnn[, seq(1, 40, 4)], MARGIN=1, FUN=mean)
mse_lstm_in <- apply(X=test_lstm[, seq(1, 40, 4)], MARGIN=1, FUN=mean)
mse_gru_in <- apply(X=test_gru[, seq(1, 40, 4)], MARGIN=1, FUN=mean)

mse_ffn_out <- apply(X=test_ffn[, seq(2, 40, 4)], MARGIN=1, FUN=mean)
mse_rnn_out <- apply(X=test_rnn[, seq(2, 40, 4)], MARGIN=1, FUN=mean)
mse_lstm_out <- apply(X=test_lstm[, seq(2, 40, 4)], MARGIN=1, FUN=mean)
mse_gru_out <- apply(X=test_gru[, seq(2, 40, 4)], MARGIN=1, FUN=mean)

sharpe_ffn_in <- apply(X=test_ffn[, seq(3, 40, 4)], MARGIN=1, FUN=mean)
sharpe_rnn_in <- apply(X=test_rnn[, seq(3, 40, 4)], MARGIN=1, FUN=mean)
sharpe_lstm_in <- apply(X=test_lstm[, seq(3, 40, 4)], MARGIN=1, FUN=mean)
sharpe_gru_in <- apply(X=test_gru[, seq(3, 40, 4)], MARGIN=1, FUN=mean)

sharpe_ffn_out <- apply(X=test_ffn[, seq(4, 40, 4)], MARGIN=1, FUN=mean)
sharpe_rnn_out <- apply(X=test_rnn[, seq(4, 40, 4)], MARGIN=1, FUN=mean)
sharpe_lstm_out <- apply(X=test_lstm[, seq(4, 40, 4)], MARGIN=1, FUN=mean)
sharpe_gru_out <- apply(X=test_gru[, seq(4, 40, 4)], MARGIN=1, FUN=mean)


# MSE-Plots####
par_default <- par(no.readonly = TRUE)
par(mfrow=c(2,1), mar=c(3,5,3,2))
plot(mse_ffn_in,
     type="l",
     ylab="MSE In-Sample",
     xlab="",
     main="Neuron-Layer Combination: Bitcoin",
     ylim=c(min(mse_ffn_in, mse_rnn_in, mse_lstm_in, mse_gru_in),
            max(mse_ffn_in, mse_rnn_in, mse_lstm_in, mse_gru_in)))
lines(mse_rnn_in, col=2)
lines(mse_lstm_in, col=3)
lines(mse_gru_in, col=4)

plot(mse_ffn_out,
     type="l",
     ylab="MSE Out-of-Sample",
     xlab="",
     ylim=c(min(mse_ffn_out, mse_rnn_out, mse_lstm_out, mse_gru_out),
            max(mse_ffn_out, mse_rnn_out, mse_lstm_out, mse_gru_out)))
lines(mse_rnn_out, col=2)
lines(mse_lstm_out, col=3)
lines(mse_gru_out, col=4)

par(par_default)
legend("center", legend = c("FFN", "RNN", "LSTM", "GRU"), lty=1, pt.cex=2, cex=0.8, bty='n',
       col = 1:4, horiz=TRUE)


perf_in <- as.numeric(data_obj$target_in)
perf_out <- as.numeric(data_obj$target_out)
sharpe_bnh_in <- as.numeric(sqrt(365)*mean(perf_in)/sqrt(var(perf_in)))
sharpe_bnh_out <- as.numeric(sqrt(365)*mean(perf_out)/sqrt(var(perf_out)))

# Sharpe-Plots####
par_default <- par(no.readonly = TRUE)
par(mfrow=c(2,1), mar=c(3,5,3,2))
plot(sharpe_ffn_in,
     type="l",
     ylab="Sharpe In-Sample",
     xlab="",
     main="Neuron-Layer Combination: Bitcoin",
     ylim=c(min(sharpe_ffn_in, sharpe_rnn_in, sharpe_lstm_out, sharpe_gru_in),
            max(sharpe_ffn_in, sharpe_rnn_in, sharpe_lstm_out, sharpe_gru_in)))
lines(sharpe_rnn_in, col=2)
lines(sharpe_lstm_out, col=3)
lines(sharpe_gru_in, col=4)
abline(h=sharpe_bnh_in, lty=3, col=8)

plot(sharpe_ffn_out,
     type="l",
     ylab="Sharpe Out-of-Sample",
     xlab="",
     ylim=c(min(sharpe_ffn_out, sharpe_rnn_out, sharpe_lstm_out, sharpe_gru_out),
            max(sharpe_ffn_out, sharpe_rnn_out, sharpe_lstm_out, sharpe_gru_out)))
lines(sharpe_rnn_out, col=2)
lines(sharpe_lstm_out, col=3)
lines(sharpe_gru_out, col=4)
abline(h=sharpe_bnh_out, lty=3, col=8)

par(par_default)
legend("center", legend = c("FFN", "RNN", "LSTM", "GRU", "BnH-Sharpe"), lty=1, pt.cex=2, cex=0.8, bty='n',
       col = c(1,2,3,4, 8), horiz=TRUE)



plot(rnorm(50), type="l", col=8)
?lty
