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
data_obj <- data_function(x=logret, lags=7, in_out_sep=dates_mat[iter,][2], start=dates_mat[iter,][1], end=dates_mat[iter,][3])
c(as.character(time(head(data_obj$target_in, 1))), as.character(time(tail(data_obj$target_in, 1))), as.character(length(data_obj$target_in)))
c(as.character(time(head(data_obj$target_out, 1))), as.character(time(tail(data_obj$target_out, 1))), as.character(length(data_obj$target_out)))
nn_10_3_50_5 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_5, file = "data/batch_5/nn_10_3_50_5.rda") # 1.65h

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
nn_10_3_50_9 <- nn_nl_comb_sharpe_mse(maxneuron=10,
                                      maxlayer=3,
                                      real=50,
                                      data_obj=data_obj)
# save(nn_10_3_50_9, file = "data/batch_5/nn_10_3_50_9.rda") # # 3.56h
