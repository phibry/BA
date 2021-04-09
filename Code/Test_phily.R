source("add/libraries.r") 
source("add/Functions.r")




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

# Define train/test split
in_out_sample_separator="2020-05-01"
train_set <- scaled_log_ret[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled_log_ret[paste(in_out_sample_separator,"/",sep=""),]

# Transform to matrices
train_set <- as.matrix(train_set)
test_set <- as.matrix(test_set)

# Define formula for the neuralnet
colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
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



#.####
# Estimate Fun####
estimate_nn<-function(train_set,number_neurons,data_mat,test_set,f)
{
  # nn <- neuralnet(f,data=train_set,hidden=number_neurons,linear.output=T, stepmax = 1e+08)
  nn <- neuralnet(f,data=train_set,hidden=number_neurons,linear.output=T, stepmax = 1e+08)
  
  # In sample performance
  predicted_scaled_in_sample<-nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  predicted_nn_in_sample<-predicted_scaled_in_sample*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # In-sample MSE
  MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max(data_mat[,1])-min(data_mat[,1])))^2)
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
  # pr.nn <- compute(nn, as.matrix(test_set[,2:ncol(test_set)]))
  pr.nn <- predict(nn, as.matrix(test_set[,2:ncol(test_set)]))
  
  predicted_scaled <- pr.nn
  # Results from NN are normalized (scaled)
  # Descaling for comparison
  predicted_nn <- predicted_scaled*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  test.r <- test_set[,1]*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # Calculating MSE
  MSE.out.nn <- mean((test.r - predicted_nn)^2)
  
  # Compare in-sample and out-of-sample
  MSE_nn<-c(MSE.in.nn,MSE.out.nn)
  
  return(list(MSE_nn=MSE_nn,predicted_nn=predicted_nn,predicted_nn_in_sample=predicted_nn_in_sample))
  
}

# MSE Fun####
combination_in_out_MSE=function(maxneuron=3,maxlayer=3,real=10,train_set,data_mat,test_set,f,plot=F)
{
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
  mati <- matrix(nrow=dim(combmat)[1], ncol=real*2, 0)
  mati <- as.data.frame(mati)
  rownames(mati) <- ind
  
  
  #creating , testing , neural net
  for( i in 1: dim(combmat)[1])
  {
    pb <- txtProgressBar(min = 1, max = dim(combmat)[1], style = 3)
    
    # i<-3
    x=as.vector(combmat[i,])
    x= x[x!=0]
    for(k in seq(1,real*2,2))
    {
      net=estimate_nn(train_set,
                      number_neurons=x,
                      data_mat,
                      test_set,
                      f) # netz erstellen
      mati[i,k]=net$MSE_nn[1] # insample error
      mati[i,k+1]=net$MSE_nn[2]
      # out of sample error
    }
    print(paste("Elapsed Time: " ,Sys.time()-starttime))
    print(paste("Iteration: ", i, "of", dim(combmat)[1]))
    setTxtProgressBar(pb, i)
    
  }
  close(pb)
  print(paste("Overall Time: " ,Sys.time()-starttime))
  
  if( plot == T)
  {
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
        plot(mati[,i],main="In-Sample", type="l", ylim=c(min(mati[,in_samp_seq]),max(mati[,in_samp_seq])), col=color)
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
        plot(mati[,i],main="Out-of-Sample", type="l", ylim=c(min(mati[,out_of_samp_seq]),max(mati[,out_of_samp_seq])), col=color)
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
    par(mfrow=c(1,1))
    iter <- 1
    prev_it <- 1
    mini <- min(mati[,in_samp_seq])
    maxi <- max(mati[,in_samp_seq])
    
    for(i in layers) {
      for(j in in_samp_seq) {
        if (j == 1) {
          plot(mati[prev_it:i, j], ylim=c(mini,maxi), main=paste("Layer: ", iter), type="l")
        } else {
          lines(mati[prev_it:i, j])
        }
      }
      prev_it <- i+1
      iter <- iter + 1
    }
  }
  
  return(mati)
}




# All in One:####
optim_10_2_20 <- combination_in_out_MSE(maxneuron=10,
                              maxlayer=2,
                              real=20,
                              train_set,
                              as.matrix(data_mat),
                              test_set,
                              f,
                              plot=T)
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
