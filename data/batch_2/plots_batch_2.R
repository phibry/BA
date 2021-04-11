source("add/libraries.r") 
source("add/Functions.r")
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






# Start: 2018-01-01
# End: 2021-03-27
# Seperator: 2020-05-01

#.####
# Plots with function####
## 5N 2L 20R####
load("data/batch_2/nn_5_2_20.rda")
plot_all_rect_mse(nn_5_2_20, 20, title="5N2L20R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_all_rect_sharpe(nn_5_2_20, 20, title="5N2L20R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_mean(nn_5_2_20, 20, "5N2L20R")

## 5N 5L 20R####
load("data/batch_2/nn_5_5_20.rda")
plot_all_rect_mse(nn_5_5_20, 20, title="5N5L20R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_all_rect_sharpe(nn_5_5_20, 20, title="5N5L20R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_mean(nn_5_5_20, 20, "5N5L20R")

plot_mean(nn_5_5_20, 20, "5N5L20R")


## 5N 5L 50R####
load("data/batch_2/nn_5_5_50.rda")
plot_all_rect_mse(nn_5_5_50, 50, title="5N5L50R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_all_rect_sharpe(nn_5_5_50, 50, title="5N5L50R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_mean(nn_5_5_50, 50, "5N5L50R")


## 10N 3L 40R####
load("data/batch_2/nn_10_3_40.rda")
plot_all_rect_mse(nn_10_3_40, 40, title="10N3L40R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_all_rect_sharpe(nn_10_3_40, 40, title="10N3L40R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_mean(nn_10_3_40, 40, "10N3L40R")

## 12N 2L 50R####
### Batch 2####
load("data/batch_2/nn_12_2_50.rda")
plot_all_rect_mse(nn_12_2_50, 50, title="12N2L50R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_all_rect_sharpe(nn_12_2_50, 50, title="12N2L50R, Start:2018-01-01, End:2021-03-27, Sep:2020-05-01")
plot_mean(nn_12_2_50, 50, "12N2L50R")

### Batch 3####
load("data/batch_3/nn_12_2_50_last.rda")
plot_all_rect_mse(nn_12_2_50_last, 50, title="12N2L50R, Start:2020-09-01, End:2021-03-27, Sep:2021-03-01")
plot_all_rect_sharpe(nn_12_2_50_last, 50, title="12N2L50R, Start:2020-09-01, End:2021-03-27, Sep:2021-03-01")
plot_mean(nn_12_2_50_last, 50, "12N2L50R")
