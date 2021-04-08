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