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



data_function<-function(x, lags, in_out_sep, start="") {
  # Define startpoints
  x <- x[paste(start,"::", sep="")]
  data_mat <- x
  
  # Create lagged data
  for (j in 1:lags)
    data_mat <- cbind(data_mat, lag(x, k=j))
  
  # Remove NA's
  data_mat <- na.exclude(data_mat)
  
  
  # Specify in- and out-of-sample episodes
  # Target in-sample (current data)
  target_in <- data_mat[paste("/",in_out_sep,sep=""),1]
  # Explanatory in-sample (lagged)
  explanatory_in <- data_mat[paste("/",in_out_sep,sep=""),2:ncol(data_mat)]
  
  # Target out of sample (current data)
  target_out <- data_mat[paste(in_out_sep,"/",sep=""),1]
  # Explanatory out of sample (lagged)
  explanatory_out <- data_mat[paste(in_out_sep,"/",sep=""),2:ncol(data_mat)]
  
  # Train / Test Split
  train <- cbind(target_in, explanatory_in)
  test <- cbind(target_out, explanatory_out)
  
  
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
  
  # formula
  colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
  n <- colnames(train_set)
  f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
  
  return(list(data_mat=data_mat,
              target_in=target_in,
              target_out=target_out,
              explanatory_in=explanatory_in,
              explanatory_out=explanatory_out,
              train_set=train_set,
              test_set=test_set,
              f=f))
}






# Load and prepare date####
load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21
x <- logret["2018-01-01::"]
colnames(x) <- "BTC-LogReturns"
chart.ACF(x)

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

# remove NA's
data_mat <- na.exclude(data_mat)

# Maxs and Mins per lag
maxs <- apply(data_mat, 2, max)
mins <- apply(data_mat, 2, min)

# Scaling returns between 0 and 1
scaled_log_ret <- scale(data_mat, center = mins, scale = maxs - mins)
plot(scaled_log_ret[,1], main="Scaled")
plot(data_mat[,1], main="Original")

# Define train/test split
in_out_sample_separator="2020-05-01"
train_set <- scaled_log_ret[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled_log_ret[paste(in_out_sample_separator,"/",sep=""),]

# Define target in/out
target_in <- data_mat[paste("/",in_out_sample_separator,sep=""),]
target_out <- data_mat[paste(in_out_sample_separator,"/",sep=""),]

# Transform to matrices
train_set <- as.matrix(train_set)
test_set <- as.matrix(test_set)

# Define formula for the neuralnet
colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)
f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))




# Input-Grid-Function####
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

input_grid(10,1)

# Define Input Grid
combmat <- input_grid(2,2)

# Naming
ind <- rep(NA,dim(combmat)[1])
for(k in 1:dim(combmat)[1]) {
  x <- as.vector(combmat[k,])
  ind[k] <- toString(as.character(x[x!=0]))
}

# Define result matrix
mati <- matrix(nrow=dim(combmat)[1], ncol=20, 0)
mati <- as.data.frame(mati)
rownames(mati) <- ind

real <- 10


# starttime <- Sys.time()
# for (j in 1:dim(combmat)[1]) {
#   for (i in seq(from=1, to=2*real, by=2)) {
#     mati[j,i:(i+1)] <- c(i, i+1) # Time difference of 5.376341 secs
#     
#     # mati[j,i] <- c(i)
#     # mati[j,(i+1)] <- c(i+1)
#     
#   }
# }
# print(Sys.time() - starttime)


# starttime <- Sys.time()
# for(i in 1:dim(combmat)[1]) {
#   for (i in seq(from=1, to=2*real, by=2)) {
#     x <- as.vector(combmat[i,])       # in vektor  umwandeln
#     x <- x[x!=0]
#     # ohne null
#     net <- estimate_nn(train_set, number_neurons=x, data_mat, test_set,f) # netz erstellen
#     mati[j, i:(i+1)] <- c(net$MSE_nn[1], net$MSE_nn[2])
#     # namen der spalte
#     print('yolo')
#   }
# }
# print(Sys.time() - starttime)

starttime=Sys.time()

for( i in 1: dim(combmat)[1])
{
  x=as.vector(combmat[i,])
  x= x[x!=0]
  for(k in seq(1,real*2,2))
  {
    net=estimate_nn(train_set,number_neurons=x,data_mat,test_set,f) # netz erstellen
    mati[i,k]=net$MSE_nn[1] # insample error
    mati[i,k+1]=net$MSE_nn[2]
    # out of sample error
  }
  print(mati[i,])
}
print(paste("duration: " ,Sys.time()-starttime))


load("data/optim_5_5_10.rda")
head(optim_5_5_10)
mati <- optim_5_5_10
real <- 10
# Layer Breakpoints
breakpoints <- unique(nchar(rownames(mati)))

layer_breakpoints_vec <- c()

for (i in breakpoints) {
  layer_breakpoints_vec <- cbind(layer_breakpoints_vec, sum(nchar(rownames(mati)) == i))
}
layers <- cumsum(layer_breakpoints_vec)


# Full Plots####
par(mfrow=c(2,1))
# In-Sample
color <- 1

in_samp_seq <- seq(1, real*2, 2)
for(i in in_samp_seq) {
  if (i == 1) {
    plot(mati[,i],
         main="In-Sample",
         type="l",
         ylim=c(min(mati[,in_samp_seq]) ,max(mati[,in_samp_seq])),
         xlim=c(1, dim(mati)[1]),
         col=color,
         ylab="MSE")
    
    color = color + 1
  } else {
    lines(mati[,i], col=color)
    color = color + 1
  }
}
for (i in head(layers, -1)) {
  abline(v=(1+i), lty=2)
}


# Out-of-Sample
color <- 1
out_of_samp_seq <- seq(2, real*2, 2)
for(i in out_of_samp_seq) {
  if (i == 2) {
    plot(mati[,i],
         main="Out-of-Sample",
         type="l",
         ylim=c(min(mati[,out_of_samp_seq]), max(mati[,out_of_samp_seq])),
         col=color,
         ylab="MSE")
    color = color + 1
  } else {
    lines(mati[,i], col=color)
    color = color + 1
  }
}
for (i in head(layers, -1)) {
  abline(v=(1+i), lty=2)
}



# Plots by Layer####
par(mfrow=c(2,1))
iter <- 1
prev_it <- 1
mini_in <- min(mati[,in_samp_seq])
maxi_in <- max(mati[,in_samp_seq])

mini_out <- min(mati[,out_of_samp_seq])
maxi_out <- max(mati[,out_of_samp_seq])

for(i in layers) {
  color <- 1
  
  for(j in in_samp_seq) {
    if (j == 1) {
      plot(mati[prev_it:i, j],
           ylim=c(mini_in, maxi_in),
           main=paste("Layer: ", iter),
           type="l",
           col=color,
           ylab="MSE")
      
      color = color + 1
    } else {
      lines(mati[prev_it:i, j], col=color)
      color = color + 1
    }
  }
  
  
  color <- 1
  for(k in out_of_samp_seq) {
    if (k == 2) {
      plot(mati[prev_it:i, k],
           ylim=c(mini_out, maxi_out),
           main=paste("Layer: ", iter),
           type="l",
           col=color,
           ylab="MSE")
      color = color + 1 
    } else {
      lines(mati[prev_it:i, k], col=color)
      color = color + 1
    }
  }
  
  prev_it <- i+1
  iter <- iter + 1
}


# Plots mit Rect####
## All in One####
par(mfrow=c(2,1), mar=c(3,5,3,2))
### In-Sample####
# color indizes for plots
color <- 1

# color codes for the rect
colorcodes <- c("#FF00001A", # red
                "#0000FF1A", # blue
                "#80FF001A", # green
                "#FF80001A", # orange
                "#00FFFF1A", # teal
                "#8000FF1A") # purple

in_samp_seq <- seq(1, real*2, 2)
for(i in in_samp_seq) {
  if (i == 1) {
    plot(mati[,i],
         main="In-Sample",
         type="l",
         ylim=c(min(mati[,in_samp_seq]) ,max(mati[,in_samp_seq])),
         xlim=c(1, dim(mati)[1]),
         col=color,
         ylab="MSE",
         frame.plot = FALSE,
         xaxt="n",
         xlab="")
    color = color + 1
  } else {
    lines(mati[,i], col=color)
    color = color + 1
  }
}
for (i in head(layers, -1)) {
  abline(v=(1+i), lty=2)
}

startl <- c(1, head(layers, -1)+1)
endl <- layers
for (i in 1:length(layers)) {
  rect(xleft = startl[i],
       xright = endl[i],
       ybottom = 0,
       ytop = 1,
       col=colorcodes[i],
       border = "transparent")
  text(startl[i]+(endl[i]-startl[i])/2, min(mati[,in_samp_seq]), i)
}


### Out-of-Sample####
color <- 1
out_of_samp_seq <- seq(2, real*2, 2)
for(i in out_of_samp_seq) {
  if (i == 2) {
    plot(mati[,i],
         main="Out-of-Sample",
         type="l",
         ylim=c(min(mati[,out_of_samp_seq]), max(mati[,out_of_samp_seq])),
         col=color,
         ylab="MSE",
         frame.plot = FALSE,
         xaxt="n",
         xlab="")
    color = color + 1
  } else {
    lines(mati[,i], col=color)
    color = color + 1
  }
}
for (i in head(layers, -1)) {
  abline(v=(1+i), lty=2)
}

for (i in 1:length(layers)) {
  rect(xleft = startl[i],
       xright = endl[i],
       ybottom = 0,
       ytop = 1,
       col=colorcodes[i],
       border = "transparent")
  text(startl[i]+(endl[i]-startl[i])/2, max(mati[,out_of_samp_seq]), i)
}



## Per Layer####
par(mfrow=c(2,1))
iter <- 1
prev_it <- 1
mini_in <- min(mati[,in_samp_seq])
maxi_in <- max(mati[,in_samp_seq])

mini_out <- min(mati[,out_of_samp_seq])
maxi_out <- max(mati[,out_of_samp_seq])

for(i in layers) {
  color <- 1
  # i <- 155
  # prev_it <- 30+1
  length(rownames(mati[prev_it:i,])) / 5
  
  
  
  for(j in in_samp_seq) {
    if (j == 1) {
      plot(mati[prev_it:i, j],
           ylim=c(mini_in, maxi_in),
           main=paste("Layer: ", iter),
           type="l",
           col=color,
           ylab="MSE",
           xaxt="n",
           ann=FALSE)
      
      par("xaxp")
      
      end <- par("xaxp")[2]
      splitter <- par("xaxp")[3]
      multi <- round(end / splitter)
      
      at <- c(1, cumsum(rep(multi, splitter)))
      
      # rownames(mati)[at]
      labels <- rownames(mati[prev_it:i,])[at]
      prev_it:i
      rownames(mati[130,])
      
      axis(1, at=at, labels=labels)
      
      
      color = color + 1
    } else {
      lines(mati[prev_it:i, j], col=color)
      color = color + 1
    }
  }
  
  
  color <- 1
  for(k in out_of_samp_seq) {
    if (k == 2) {
      plot(mati[prev_it:i, k],
           ylim=c(mini_out, maxi_out),
           main=paste("Layer: ", iter),
           type="l",
           col=color,
           ylab="MSE")
      color = color + 1 
    } else {
      lines(mati[prev_it:i, k], col=color)
      color = color + 1
    }
  }
  
  prev_it <- i+1
  iter <- iter + 1
}
