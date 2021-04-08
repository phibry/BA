source("add/libraries.r") 
source("add/Functions.r")

# Data####
load("data/log_ret_27_03_21.rda")  # loading logreturns closing! data xts
load("data/BTC_USD_27_03_21.rda")

lr <- log_ret_27_03_21
cl <- BTC_USD_27_03_21$`BTC-USD.Close`

par(mfrow=c(2,2))
plot(cl)
chart.ACF(cl)
plot(lr)
chart.ACF(lr)


# l=3, n=3####
neuron <- 3
layer <- 3
anz <- neuron^(1:layer)
mat <- matrix(0, nrow=sum(anz), ncol=layer)

# Erster Schritt
mat[1:3,1] <- rep(1:3)

# Zweiter Schritt
mat[4:12, 1] <- rep(1:3, rep(3,3))
mat[4:12, 2] <- rep(1:3, 3)

# Dritter Schritt
mat[13:39, 1] <- rep(1:3, rep(9,3))
mat[13:39, 2] <- rep(rep(1:3, c(3,3,3)), 3)
mat[13:39, 3] <- rep(1:3, 9)
mat


# l=2, n=2####
n <- 2
l <- 2
anz <- n^(1:l)
mat <- matrix(0, nrow=sum(anz), ncol=l)

# Erster Schritt
mat[1:2, 1] <- rep(1:n)

# Zweiter Schritt
mat[3:6, 1] <- rep(1:n, rep(n, n))
mat[3:6, 2] <- rep(1:n, 2)


# l=3, n=2####
n <- 2
l <- 3
anz <- n^(1:l)
mat <- matrix(0, nrow=sum(anz), ncol=l)

# Erster Schritt:
mat[1:2, 1] <- rep(1:2)

# Zweiter Schritt:
mat[3:6, 1] <- rep(1:n, rep(n, n))
mat[3:6, 2] <- rep(1:n, rep(n))

# Dritter Schritt:
mat[7:14, 1] <- rep(1:n, rep(n*n, n))
mat[7:14, 2] <- rep(rep(1:n, c(n,n)), n)
mat[7:14, 3] <- rep(1:n, n*n)




# 

# Plots for single Realisation
# Full Plot
# plot(mati[,2],type="l", ylim=c(min(mati[,3]), max(mati[,2])))
# lines(mati[,3],col="red")
# for (i in head(layers, -1)) {
#   abline(v=(1+i), lty=2)
# }
# 
# iter <- 1
# prev_it <- 1
# for(i in layers) {
#   plot(mati[prev_it:i,2],type="l", ylim=c(min(mati[prev_it:i,3]), max(mati[prev_it:i,2])), main=paste("Layer: ", iter))
#   lines(mati[prev_it:i,3],col="red")
#   
#   prev_it <- i+1
#   iter <- iter + 1
# }




# Fullplots in one
# par(mfrow=c(1,1))
# 
# in_samp_seq <- seq(1, real*2, 2)
# out_of_samp_seq <- seq(2, real*2, 2)
# 
# for(i in 1:real) {
#   if (i == 1) {
#     plot(mati[,in_samp_seq[i]],
#          main="Full",
#          type="l",
#          ylim=c(min(mati[,out_of_samp_seq]) ,max(mati[,in_samp_seq])),
#          col=i,
#          ylab="MSE")
#     
#     lines(mati[,out_of_samp_seq[i]], col=i)
# 
#   } else {
#     lines(mati[,in_samp_seq[i]], col=i)
#     lines(mati[,out_of_samp_seq[i]], col=i)
#   }
# }
# for (i in head(layers, -1)) {
#   abline(v=(1+i), lty=2)
# }
