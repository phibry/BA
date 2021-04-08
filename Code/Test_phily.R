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


load("data/optim_5_5.rda")
head(optim_5_5)
mati <- optim_5_5
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
  print(i)
  
  for(j in in_samp_seq) {
    print(j)
    if (j == 1) {
      plot(mati[prev_it:i, j], ylim=c(mini,maxi), main=paste("Layer: ", iter), type="l")
    } else {
      lines(mati[prev_it:i, j])
    }
  }
  prev_it <- i+1
  iter <- iter + 1
}


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

