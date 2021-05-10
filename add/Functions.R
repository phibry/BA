# feed forward nets ####
# feedforward estimation

# this function takes the input :
# trainset = scaled set for training ( insample )
# test_set = scaled set for training ( out of sample )
# f       = formula -- nn takes input like regression LM 
# data_mat = first row are output data 2: n rows are input data
# number of neurons = vector with neurons 


#this function generates a neural net on the training data and computes output with the trained model on out of sample data
# it compares the in sample performance of the net with mse to the out of sample performance mse
# output delivers mse in vs out of sample , predicted values insample and predicted values out of sample

estimate_nn <- function(train_set,number_neurons,data_mat,test_set,f,newnet=T,nn=NA)
{
  
  if(newnet){nn <- neuralnet(f,data=train_set,hidden=number_neurons,linear.output=T, stepmax = 1e+08)}
  else{nn=nn}

  
  # In sample performance
  predicted_scaled_in_sample<-nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  predicted_nn_in_sample<-predicted_scaled_in_sample*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # In-sample MSE
  MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max(data_mat[,1])-min(data_mat[,1])))^2)
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
  
  pr.nn <- compute(nn, as.matrix(test_set[,2:ncol(test_set)]))
  
  
  pr.nn <- retry(compute(nn,as.matrix(test_set[,2:ncol(test_set)])), when = "Fehler in cbind(1, pred) %*% weights[[num_hidden_layers + 1]] : 
  verlangt numerische/komplexe Matrix/Vektor-Argumente ")

  predicted_scaled<-pr.nn$net.result
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



# this function creates an input grid for all posiible combatioons of neurons and eliminates invalid 0
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


# optimizing with all combinations
# this function requires estimate_nn, grid_function
combination_in_out_MSE <- function(maxneuron=3,maxlayer=3,real=10,train_set,data_mat,test_set,f,plot=F)
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
    x=as.vector(combmat[i,])
    x= x[x!=0]
    for(k in seq(1,real*2,2))
    {
      net=estimate_nn(train_set,number_neurons=x,data_mat,test_set,f) # netz erstellen
      mati[i,k]=net$MSE_nn[1] # insample error
      mati[i,k+1]=net$MSE_nn[2]
      # out of sample error
    }
    print(paste("Elapsed Time: " ,Sys.time()-starttime))
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
  }
  
  return(mati)
}



#.####
# NN####
# Train/Predict various neural nets
# - Define first the data_obj with the funtion: data_function
#   x: data
#   lags: amount of lags (e.g. acf(x) -> significant = 6 -> lags=6)
#   start: Startdate (e.g. 2020-01-01)
#   end: Enddate (eg. 2020-07-31)
#   in_out_sep: Seperator (eg. 2020-07-01)
#   data_function will return a few data objects which are needed for
#   the computation of the MSE and Sharpe

# - use nn_nl_comb_sharpe_mse
#   define maxneurons/layers and the amount of realizations

## MSE Sharpe Function####
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
  
  # close(pb)
  print(paste("Overall Time: " ,Sys.time()-starttime))
  return(mati)
}

## Data Function####
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

## Input Grid Function####
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




#.####
# MSE Plots####
plot_all_rect <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
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
           main=paste(title, " In-Sample", sep=""),
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
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,in_samp_seq]),
         ytop = max(mati[,in_samp_seq]),
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  
  ## Out-of-Sample
  color <- 1
  out_of_samp_seq <- seq(2, real*2, 2)
  for(i in out_of_samp_seq) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, " Out-of-Sample", sep=""),
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
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,out_of_samp_seq]),
         ytop = max(mati[,out_of_samp_seq]),
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
    # text(startl[i]+(endl[i]-startl[i])/2, max(mati[,out_of_samp_seq])*0.98, i)
  }
  
  par(par_default)
}

plot_all_rect_scale <- function(mati, real, title="", scale_fac = 3) {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
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
           main=paste(title, ": In-Sample", sep=""),
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
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mati[,in_samp_seq]),
         # ytop = max(mati[,in_samp_seq]),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  
  ## Out-of-Sample
  color <- 1
  out_of_samp_seq <- seq(2, real*2, 2)
  ylower=min(mati[out_of_samp_seq])
  yupper=min(mati[out_of_samp_seq])*scale_fac
  
  for(i in out_of_samp_seq) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(ylower, yupper),
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
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mati[,in_samp_seq]),
         # ytop = max(mati[,in_samp_seq]),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
    # text(startl[i]+(endl[i]-startl[i])/2, max(mati[,out_of_samp_seq])*0.98, i)
  }
  
  par(par_default)
}


plot_by_layer_rect <- function(mati, real, title="") {
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Plots by Layer
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  in_samp_seq <- seq(1, real*2, 2)
  out_of_samp_seq <- seq(2, real*2, 2)
  iter <- 1
  prev_it <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  for(i in layers) {
    color <- 1
    
    for(j in in_samp_seq) {
      mini_in <- min(mati[prev_it:i, in_samp_seq])
      maxi_in <- max(mati[prev_it:i, in_samp_seq])
      if (j == 1) {
        plot(mati[prev_it:i, j],
             ylim=c(mini_in, maxi_in),
             main=paste(title, " Layer: ", iter, " In-Sample",sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")
        color = color + 1

        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()
        
        
        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }
        
        axis(1, at=at, labels=labels)
        
      } else {
        lines(mati[prev_it:i, j], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    color <- 1
    for(k in out_of_samp_seq) {
      mini_out <- min(mati[prev_it:i, out_of_samp_seq])
      maxi_out <- max(mati[prev_it:i, out_of_samp_seq])
      if (k == 2) {
        plot(mati[prev_it:i, k],
             ylim=c(mini_out, maxi_out),
             main=paste(title, " Layer: ", iter, " Out-of-Sample", sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")
        color = color + 1 
        
        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()
        
        
        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }
        
        axis(1, at=at, labels=labels)
      } else {
        lines(mati[prev_it:i, k], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    prev_it <- i+1
    iter <- iter + 1
  }
  par(par_default)
}

plot_by_layer_rect_scale <- function(mati, real, title="", scale_vec) {
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Plots by Layer
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  in_samp_seq <- seq(1, real*2, 2)
  out_of_samp_seq <- seq(2, real*2, 2)
  iter <- 1
  prev_it <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  for(i in layers) {
    color <- 1
    
    for(j in in_samp_seq) {
      mini_in <- min(mati[prev_it:i, in_samp_seq])
      maxi_in <- max(mati[prev_it:i, in_samp_seq])
      if (j == 1) {
        plot(mati[prev_it:i, j],
             ylim=c(mini_in, maxi_in),
             main=paste(title, " Layer: ", iter, " In-Sample",sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")
        color = color + 1
        
        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()
        
        
        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }
        
        axis(1, at=at, labels=labels)
        
      } else {
        lines(mati[prev_it:i, j], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    color <- 1
    for(k in out_of_samp_seq) {
      mini_out <- min(mati[prev_it:i, out_of_samp_seq])
      maxi_out <- min(mati[prev_it:i, out_of_samp_seq]) * scale_vec[iter]
      # maxi_out <- max(mati[prev_it:i, out_of_samp_seq])
      if (k == 2) {
        plot(mati[prev_it:i, k],
             ylim=c(mini_out, maxi_out),
             main=paste(title, " Layer: ", iter, " Out-of-Sample", sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")
        color = color + 1 
        
        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()
        
        
        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }
        
        axis(1, at=at, labels=labels)
      } else {
        lines(mati[prev_it:i, k], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    prev_it <- i+1
    iter <- iter + 1
  }
  par(par_default)
}

plot_all <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }

  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)


  # Full Plots
  par(mfrow=c(2,1))
  # In-Sample
  color <- 1

  in_samp_seq <- seq(1, real*2, 2)
  for(i in in_samp_seq) {
    if (i == 1) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
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
           main=paste(title, ": Out-of-Sample", sep=""),
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

}
plot_by_layer <- function(mati, real, title="") {
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }

  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Plots by Layer
  par(mfrow=c(2,1))
  in_samp_seq <- seq(1, real*2, 2)
  out_of_samp_seq <- seq(2, real*2, 2)
  iter <- 1
  prev_it <- 1
  # mini_in <- min(mati[,in_samp_seq])
  # maxi_in <- max(mati[,in_samp_seq])

  # mini_out <- min(mati[,out_of_samp_seq])
  # maxi_out <- max(mati[,out_of_samp_seq])

  for(i in layers) {
    color <- 1

    for(j in in_samp_seq) {
      mini_in <- min(mati[prev_it:i, in_samp_seq])
      maxi_in <- max(mati[prev_it:i, in_samp_seq])
      if (j == 1) {
        plot(mati[prev_it:i, j],
             ylim=c(mini_in, maxi_in),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")

        color = color + 1

        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()


        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }

        axis(1, at=at, labels=labels)
      } else {
        lines(mati[prev_it:i, j], col=color)
        color = color + 1
      }
    }


    color <- 1
    for(k in out_of_samp_seq) {
      mini_out <- min(mati[prev_it:i, out_of_samp_seq])
      maxi_out <- max(mati[prev_it:i, out_of_samp_seq])
      if (k == 2) {
        plot(mati[prev_it:i, k],
             ylim=c(mini_out, maxi_out),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE",
             xaxt="n")

        end <- length(rownames(mati[prev_it:i, ]))
        multi <- round((end-1)/3)
        at <- c(1, 1+multi, 1+multi*2, end)
        labels <- c()


        for (g in at) {
          nlcomb <- gsub("[[:blank:]]", "", rownames(mati[prev_it:i, ])[g])
          labels <- c(labels, paste('(',nlcomb,')', sep=""))
        }

        axis(1, at=at, labels=labels)
        color = color + 1
      } else {
        lines(mati[prev_it:i, k], col=color)
        color = color + 1
      }
    }

    prev_it <- i+1
    iter <- iter + 1
  }
}


#.####
# Sharpe-MSE Plots####
plot_all_rect_mse <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  color <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  seq_mse_in <- seq(1, real*4, 4)
  seq_mse_out <- seq(2, real*4, 4)
  seq_sharpe_in <- seq(3, real*4, 4)
  seq_sharpe_out <- seq(4, real*4, 4)
  
  
  # MSE: In-sample
  for(i in seq_mse_in) {
    if (i == 1) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_mse_in]) ,max(mati[,seq_mse_in])),
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
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.04)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # MSE: Out-of-sample
  color <- 1
  for(i in seq_mse_out) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_mse_out]), max(mati[,seq_mse_out])),
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
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.96)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}


plot_all_rect_sharpe <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  color <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  seq_mse_in <- seq(1, real*4, 4)
  seq_mse_out <- seq(2, real*4, 4)
  seq_sharpe_in <- seq(3, real*4, 4)
  seq_sharpe_out <- seq(4, real*4, 4)
  
  
  # Sharpe: In-sample
  for(i in seq_sharpe_in) {
    if (i == 3) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_sharpe_in]) ,max(mati[,seq_sharpe_in])),
           xlim=c(1, dim(mati)[1]),
           col=color,
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.04)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
    
    # text(startl[i]+(endl[i]-startl[i])/2, min(mati[,seq_sharpe_in])*0.9, i)
  }
  
  # MSE: Out-of-sample
  color <- 1
  for(i in seq_sharpe_out) {
    if (i == 4) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_sharpe_out]), max(mati[,seq_sharpe_out])),
           col=color,
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.96)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
    
    # text(startl[i]+(endl[i]-startl[i])/2, max(mati[,seq_sharpe_out])*0.9, i)
  }
  
  par(par_default)
}


plot_mean <- function(mati, real, title="") {
  # Mean over all Realisations
  mse_in_mean <- apply(mati[,seq(1, real*4, 4)], MARGIN=1, FUN=mean)
  mse_out_mean <- apply(mati[,seq(2, real*4, 4)], MARGIN=1, FUN=mean)
  sharpe_in_mean <- apply(mati[,seq(3, real*4, 4)], MARGIN=1, FUN=mean)
  sharpe_out_mean <- apply(mati[,seq(4, real*4, 4)], MARGIN=1, FUN=mean)
  
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  mini <- min(min(mse_in_mean), min(mse_out_mean))
  maxi <- max(max(mse_in_mean), max(mse_out_mean))
  plot(mse_in_mean,
       type="l",
       col="#2f4b7c",
       ylim=c(mini, maxi),
       ylab="MSE",
       main=paste(title," MSE mean over ", 50, " realizations", sep=""),
       frame.plot = FALSE,
       xaxt="n",
       xlab="")
  lines(mse_out_mean, col="#ff7c43")
  legend("top", legend=c("MSE In", "MSE Out"), lty=c(1,1), col=c("#2f4b7c","#ff7c43"), cex=0.6, bty = "n")
  
  # rect
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.04)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  mini <- min(min(sharpe_in_mean), min(sharpe_out_mean))
  maxi <- max(max(sharpe_in_mean), max(sharpe_out_mean))
  plot(sharpe_in_mean,
       type="l",
       col="#2f4b7c",
       ylim=c(mini, maxi),
       ylab="Sharpe",
       main=paste(title, " Sharpe mean over ", 50, " realizations", sep=""),
       frame.plot = FALSE,
       xaxt="n",
       xlab="")
  lines(sharpe_out_mean, type="l", col="#ff7c43")
  legend("top", legend=c("Sharpe In", "Sharpe Out"), lty=c(1,1), col=c("#2f4b7c","#ff7c43"), cex=0.6, bty = "n")
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = par('usr')[3],
         ytop = par('usr')[4],
         col=colorcodes[i])
    
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.96)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}


#.####
# Mean Plots####
plot_mse_mean <- function(mse_in, mse_out, title="",scale_fac=3) {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mse_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  split_colors <- c("#59C7EB",
                    "#E0607E",
                    "#0A9086",
                    "#FEA090",
                    "#3E5496",
                    "#EFDC60",
                    "#8E2043",
                    "#9AA0A7",
                    "#AC9A8C")
  # MSE in
  for(i in 1:dim(mse_in)[2]) {
    if (i == 1) {
      plot(mse_in[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mse_in) ,max(mse_in)),
           xlim=c(1, dim(mse_in)[1]),
           col=split_colors[i],
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(mse_in[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mse_in),
         ytop = max(mse_in),
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # MSE out
  for(i in 1:dim(mse_out)[2]) {
    if (i == 1) {
      plot(mse_out[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mse_out) ,min(mse_out)*scale_fac),
           xlim=c(1, dim(mse_out)[1]),
           col=split_colors[i],
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(mse_out[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_out),
         # ytop = max(mse_out),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}

plot_sharpe_mean <- function(sharpe_in, sharpe_out, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(sharpe_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  split_colors <- c("#59C7EB",
                    "#E0607E",
                    "#0A9086",
                    "#FEA090",
                    "#3E5496",
                    "#EFDC60",
                    "#8E2043",
                    "#9AA0A7",
                    "#AC9A8C")
  # Sharpe in
  for(i in 1:dim(sharpe_in)[2]) {
    if (i == 1) {
      plot(sharpe_in[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(sharpe_in) ,max(sharpe_in)),
           xlim=c(1, dim(sharpe_in)[1]),
           col=split_colors[i],
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(sharpe_in[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(sharpe_in),
         ytop = max(sharpe_in),
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # Sharpe out
  for(i in 1:dim(sharpe_out)[2]) {
    if (i == 1) {
      plot(sharpe_out[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(sharpe_out) ,max(sharpe_out)),
           xlim=c(1, dim(sharpe_out)[1]),
           col=split_colors[i],
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(sharpe_out[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_out),
         # ytop = max(mse_out),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}

# XAI ####
OLPD_func<-function(x,delta,epsilon,nn)
{
  try_data_list<-try(out_original<-predict(nn,x),silent=T)
  
  if(class(try_data_list)[1]=="try-error")
  {
    data_list<-vector(mode="list")
    print("Neural net singular")
    effect<-NULL
    return(list(effect=effect))
    
  } else
  {
    
    
    
    # For each explanatory...
    for (i in 1:ncol(x))#i<-1
    {
      # y will be the original explanatory plus an infinitesimal perturbation of i-th explanatory
      y<-x
      y[,i]<-y[,i]+delta*x[,i]
      
      # Generate infinitesimally perturbated output
      out_i <-predict(nn,y)
      
      if (i==1)
      {
        effect<-(out_i-out_original)/(delta*x[,i])
      } else
      {
        effect<-c(effect,(out_i-out_original)/(delta*x[,i]))
      }
      # Collect for each explanatory the perturbated data and the corresponding nn-output
      #    }
    }
    # Virtual intercept: output of neural net minus linear regression part
    virt_int<-out_original-as.double(x%*%effect)
    effect<-c(virt_int,effect)
    
    
    # Fit the regression to the noiseless perturbated data: as many observations as unknowns i.e. zero-residual
    return(list(effect=effect))
  }
}
transform_OLPD_back_original_data_func<-function(data_xts,data_mat,OLPD_scaled_mat,lm_obj,data)
{
  # Make xts-object (not trivial in this case because of monthly dates...)
  OLPD_mat<-data_xts
  for (i in 1:nrow(OLPD_scaled_mat))
    OLPD_mat[i,]<-OLPD_scaled_mat[i,]
  OLPD_scaled_mat<-OLPD_mat
  is.xts(OLPD_mat)
  colnames(OLPD_mat)<-c("intercept",colnames(data_xts)[2:ncol(data_xts)])
  
  # Transform back to original log-returns: the regression weights are not affected in this case because target and explanatory are scaled by the same constant: we nevertheless apply the (identity) scaling to be able to work in more general settings
  for (j in 2:ncol(OLPD_mat))
    OLPD_mat[,j]<- OLPD_scaled_mat[,j]*(max(data_mat[,1])-min(data_mat[,1]))/(max(data_mat[,j])-min(data_mat[,j]))
  # The intercept is affected
  #   -We center the intercept: variations about its mean value
  #   -We scale these variations: divide by scale of transformed and multiply by scale of log-returns
  #   -Add intercept from original regression
  OLPD_mat[,1]<-(OLPD_scaled_mat[,1]-mean(OLPD_scaled_mat[,1],na.rm=T))*((max(data_mat[,1])-min(data_mat[,1]))/(max(data[,1])-min(data[,1]))) +lm_obj$coefficients[1]
  
  return(list(OLPD_mat=OLPD_mat,OLPD_scaled_mat=OLPD_scaled_mat))
}

OLPDphil <- function(in_out_sample_separator,data_mat,use_in_samp=T,neuron_vec) {
  
  # Regression
  reg_data <- data_mat
  colnames(reg_data) <- paste("lag",0:(ncol(reg_data)-1),sep="")
  lm_obj <- lm(lag0~., data=reg_data)
  
  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)
  
  # Train-test split
  train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
  test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]
  
  # Transform to matrix
  train_set <- as.matrix(train_set_xts)
  test_set <- as.matrix(test_set_xts)
  
  # Change colnames
  colnames(train_set) <- paste("lag",0:(ncol(train_set)-1),sep="")
  
  n <- colnames(train_set)
  # Model: target is current GOOGLE, all other variables are explanatory
  f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
  
  # Train neural net
  nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=F)
  
  # Original linear parameter data
  #   Generate new data from original data
  #   New data: in each time point compute the parameters of the exact infinitesimal linear regression model
  # Induce infinitesimal perturbations to data and fit regression to output
  delta <- 1.e-5
  epsilon <- 1.e-4
  
  
  if (use_in_samp) {
    # Smoother (in-sample data)
    data <- train_set
    data_xts <- train_set_xts
  } else {
    # Rougher (out-sample data)
    data <- test_set
    data_xts <- test_set_xts
  }
  
  pb <- txtProgressBar(min = 1, max = (nrow(data)-1), style = 3)
  for (i in 1:(nrow(data))) {
    x <- matrix(data[i,2:ncol(data)], nrow=1)
    colnames(x) <- colnames(data)[2:ncol(data)]
    
    # Wie OLPD-func
    OLPD_scaled_obj <- OLPD_func(x, delta, epsilon, nn)
    
    if (i==1) {
      OLPD_scaled_mat <- OLPD_scaled_obj$effect
    } else {
      OLPD_scaled_mat <- rbind(OLPD_scaled_mat,OLPD_scaled_obj$effect)
    }
    setTxtProgressBar(pb, i)
    
  }
  close(pb)
  
  # Transform data back to its original form
  OLPD_mat_obj <- transform_OLPD_back_original_data_func(data_xts, data_mat, OLPD_scaled_mat, lm_obj, data)
  
  OLPD_mat <- OLPD_mat_obj$OLPD_mat
  
  index(OLPD_mat) <- index(data_xts)
  colnames(OLPD_mat)[1] <- "(Intercept)"
  colnames(OLPD_mat)[2:ncol(OLPD_mat)] <- paste("lag",1:(ncol(train_set)-1),sep="")
  
  
  return(list(OLPD_mat=OLPD_mat, lm_obj=lm_obj))
}

xai_outp<-function(x,lags,in_out_sep,neuron_vec,intercept=F,anz=1000,percentage=NULL,devi=1,plot=F,outtarget,use_between=F,lower=2,upper=3)
{
  
  data_function(x, lags, in_out_sep, autoassign = T)
  
  target_in<-data_mat[paste("/",in_out_sep,sep=""),1]
  target_out<-data_mat[paste(in_out_sep,"/",sep=""),1]
  
  
  # lm only used, for coefficients
  explanatory_in<-data_mat[paste("/",in_out_sep,sep=""),2:ncol(data_mat)]
  explanatory_out<-data_mat[paste(in_out_sep,"/",sep=""),2:ncol(data_mat)]
  
  lm_obj<-lm(target_in~explanatory_in)
  #summary(lm_obj)
  
  
  
  
  
  # Induce infinitesimal perturbations to data and fit regression to output
  
  delta<-1.e-5
  epsilon<-1.e-4
  
  data_in<-train_set
  data_xts_in<-train_set_xts
  
  
  data_out<-test_set
  data_xts_out<-test_set_xts
  
  
  for (l in 1:anz)  
  {  
    #Neural net fitting for btc with sigmoid
    nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=T)
    net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f,newnet = F,nn=nn)
    
    ###olpd insample 
    for (i in 1:(nrow(data_in)))
    {
      x_in<-matrix(data_in[i,2:ncol(data_in)],nrow=1)
      colnames(x_in)<-colnames(data_in)[2:ncol(data_in)]
      OLPD_scaled_obj<-OLPD_func(x_in,delta,epsilon,nn)
      
      if (i==1)
      {
        OLPD_scaled_mat<-OLPD_scaled_obj$effect
      } else
      {
        OLPD_scaled_mat<-rbind(OLPD_scaled_mat,OLPD_scaled_obj$effect)
      }
    }
    OLPD_mat_in<-transform_OLPD_back_original_data_func(data_xts_in,data_mat,OLPD_scaled_mat,lm_obj,data_in)$OLPD_mat
    
    #olpd out of sample
    for (i in 1:(nrow(data_out)))
    {
      x_out<-matrix(data_out[i,2:ncol(data_out)],nrow=1)
      colnames(x_out)<-colnames(data_out)[2:ncol(data_out)]
      OLPD_scaled_obj<-OLPD_func(x_out,delta,epsilon,nn)
      
      if (i==1)
      {
        OLPD_scaled_mat<-OLPD_scaled_obj$effect
      } else
      {
        OLPD_scaled_mat<-rbind(OLPD_scaled_mat,OLPD_scaled_obj$effect)
      }
    }
    OLPD_mat_out<-transform_OLPD_back_original_data_func(data_xts_out,data_mat,OLPD_scaled_mat,lm_obj,data_out)$OLPD_mat    
    
    
    
    
    #### calclating mean 
    if (l==1)
    {
      #olpd in and out
      start_in=OLPD_mat_in
      start_out=OLPD_mat_out
      #nn
      startsignal_in=sign(net$predicted_nn_in_sample)
      startsignal_out=sign(net$predicted_nn)  
    }
    else
    {
      #olpd in and out
      
      start_in=start_in+OLPD_mat_in
      start_out=start_out+OLPD_mat_out
      #nn
      startsignal_in=startsignal_in+sign(net$predicted_nn_in_sample)
      startsignal_out=startsignal_out+sign(net$predicted_nn) 
    }
    cat("\014")   
    print(l)
  }  
  
  
  
  #renaming
  
  index(OLPD_mat_in)<-index(data_xts_in)  
  index(OLPD_mat_out)<-index(data_xts_out) 
  
  #mean of all olpd over realisations
  
  OLPD_mat_in=na.exclude(start_in/anz)
  OLPD_mat_out=na.exclude(start_out/anz)
  
  
  mean_in=apply(OLPD_mat_in[-nrow(OLPD_mat_in),],2,mean)
  sd_in=apply(OLPD_mat_in[-nrow(OLPD_mat_in),],2,sd)
  
  apply(OLPD_mat_out,2,sd)
  apply(OLPD_mat_out,2,mean)
  
  
  if(intercept==F){OLPD_mat_out=OLPD_mat_out[,-1];mean_in=mean_in[-1];sd_in=sd_in[-1]}
  
  OLPD_mat_out_deviator=OLPD_mat_out
  OLPD_mat_out_deviator[]<-0
  
  for (k in 1:dim(OLPD_mat_out_deviator)[2])
    
    
    OLPD_mat_out_deviator[which(OLPD_mat_out[,k]   > mean_in[k] + devi*sd_in[k] | OLPD_mat_out[,k]   < mean_in[k] - devi*sd_in[k]),k]<-1
  
  
  
  
  
  
  sum_explana=as.xts(apply(OLPD_mat_out_deviator,1,sum))
  signal_olpd<-sum_explana
  signal_olpd[]=1
  signal_olpd[which(sum_explana>=lower & sum_explana < upper )]<- 0.5
  signal_olpd[which(sum_explana >= upper )]<-0
  
  
  
  
  
  
  
  
  
  
  #signals ##########################################################--------------------------------------------
  
  # neuralnet signals
  
  #signum of the most votet 50% negative + 50 % positive get a zero , >50% negative & <50%positive get a -1  
  
  signal_in=sign(startsignal_in)
  signal_out=sign(startsignal_out)
  majority=signal_out # return all signals from nets
  #other rule 2  
  if( !is.null(percentage)){
    signal_in[which(abs(startsignal_in)<=percentage*anz)]<-0
    signal_out[which(abs(startsignal_out)<=percentage*anz)]<-0
  }
  
  target_out<-outtarget
  
  
  #perf_nn_in<-signal_in*target_in
  perf_nn_out<-signal_out*target_out
  perf_nn_in<-signal_in*target_in[paste("::",as.Date(in_out_sep)-1,sep="")]
  
  
  
  
  sum(signal_out==sign(target_out))/length(signal_out) # out of sample accuracy
  sum(signal_in==sign(target_in[paste("::",as.Date(in_out_sep)-1,sep="")]))/length(signal_in) # in sample accuracy
  
  
  
  signal_nn_and_olpd=signal_out;signal_nn_and_olpd[which(signal_olpd==0)]<-0;#signal_nn_and_olpd[which(signal_olpd==0.5)]<-0.5
  perf_nn_out_with_olpd=signal_nn_and_olpd*target_out
  
  
  sharpe_bh=round(sqrt(365)*SharpeRatio(target_out,FUN="StdDev"),3)
  sharpe_net=round(sqrt(365)*SharpeRatio(perf_nn_out,FUN="StdDev"),3)
  sharpe_net_olpd=round(sqrt(365)*SharpeRatio(perf_nn_out_with_olpd,FUN="StdDev"),3)
  
  #plots####--------------------------------------------------------------------------------------------------------------------------
  plot.new()  
  if(plot)
  {
    
    #par(mfrow=c(4,2))
    #plot of decision
    
    
    plot(OLPD_mat_out,col=rainbow(ncol(OLPD_mat_out)))
    for (i in 1:ncol(OLPD_mat_out))
      mtext(colnames(OLPD_mat_out)[i],col=rainbow(ncol(OLPD_mat_out))[i],line=-i)
    
    abline(h=as.numeric(mean_in[-1]))
    abline(h=as.numeric(mean_in[-1]+devi*sd_in[-1]),col="red")
    abline(h=as.numeric(mean_in[-1]-devi*sd_in[-1]),col="green")
    
    
    
    
    
    plot(OLPD_mat_in,col=rainbow(ncol(OLPD_mat_in)))
    for (i in 1:ncol(OLPD_mat_in))
      mtext(colnames(OLPD_mat_in)[i],col=rainbow(ncol(OLPD_mat_in))[i],line=-i)
    
    
    
    
    plot(signal_olpd,main="final signal")
    
    
    plot(sum_explana,type="b",main=paste("lags deviateing:",as.character(devi),"from mean"))
    
    
    
    
    plot(target_out)
    
    
    plot(cumsum(target_out),main=paste("buy and hold, sharpe:",as.character(sharpe_bh)) )
    
    
    plot(cumsum(perf_nn_out),main=paste(as.character(anz),"n nets",as.character(neuron_vec),", sharpe:",as.character(sharpe_net)))
    
    
    plot(cumsum(perf_nn_out_with_olpd),main=paste("Net Olpd, sharpe:",as.character(sharpe_net_olpd)))
  }
  
  
  
  return(list(sharpe_net_olpd=sharpe_net_olpd,sharpe_net=sharpe_net,sharpe_bh=sharpe_bh
              ,perf_nn_out_with_olpd=perf_nn_out_with_olpd,perf_nn_out=perf_nn_out,signal_out=signal_out,signal_olpd=signal_olpd,majority=majority)) 
  
}
s

#.####


# Mean of Mean####
plot_mse_mean_mean <- function(mse_in, mse_out, title="",scale_fac=3) {
  mse_in <- mean_mean_in
  mse_out <- mean_mean_out
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  layers <- sapply(X=names(mse_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Only the first two
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  # MSE in
  plot(mse_in,
       main=paste(title, ": In-Sample", sep=""),
       type="l",
       ylim=c(min(mse_in) ,max(mse_in)),
       xlim=c(1, length(mse_in)),
       col="#303030",
       ylab="MSE",
       frame.plot = FALSE,
       xaxt="n",
       xlab="")
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_in),
         # ytop = max(mse_in),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i],
         lty=3)
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # MSE out
  plot(mse_out,
       main=paste(title, ": Out-of-Sample", sep=""),
       type="l",
       ylim=c(min(mse_out) ,min(mse_out)*scale_fac),
       xlim=c(1, length(mse_out)),
       col="#303030",
       ylab="MSE",
       frame.plot = FALSE,
       xaxt="n",
       xlab="")
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_out),
         # ytop = max(mse_out),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i],
         lty=3)
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}



# ACF####
chart.ACF.phil <- function (R, maxlag = NULL, elementcolor = "gray", main = NULL, ymax = 0.3,
                            ...) 
{
  R = checkData(R)
  data = checkData(R[, 1], method = "vector", na.rm = TRUE)
  columns = ncol(R)
  rows = nrow(R)
  columnnames = colnames(R)
  if (is.null(main)) 
    main = columnnames[1]
  num = length(data)
  if (is.null(maxlag)) 
    maxlag = ceiling(10 + sqrt(num))
  ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
  Lag = 1:length(ACF)/frequency(data)
  minA = min(ACF)
  U = 2/sqrt(num)
  L = -U
  minu = min(minA, L) - 0.01
  plot(Lag, ACF, type = "h", ylim = c(minu, ymax), main = main, 
       axes = FALSE, ...)
  box(col = elementcolor)
  axis(2, col = elementcolor, cex.axis = 0.8)
  axis(1, col = elementcolor, cex.axis = 0.8)
  abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
}

chart.ACFplus.phil <- function (R, maxlag = NULL, elementcolor = "gray", main = NULL, ymax = 1,
                                ...) 
{
  R = checkData(R)
  data = checkData(R[, 1, drop = FALSE], rm.na = TRUE, method = "vector")
  columns = ncol(R)
  rows = nrow(R)
  columnnames = colnames(R)
  if (is.null(main)) 
    main = columnnames[1]
  num = length(data)
  if (is.null(maxlag)) 
    maxlag = ceiling(10 + sqrt(num))
  ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
  PACF = t(as.matrix(pacf(data, maxlag, plot = FALSE)$acf))
  Lag = 1:length(ACF)/frequency(data)
  minA = min(ACF)
  minP = min(PACF)
  U = 2/sqrt(num)
  L = -U
  minu = min(minA, minP, L) - 0.01
  op <- par(no.readonly = TRUE)
  layout(rbind(1, 2))
  par(mar = c(0.5, 4, 4, 2) + 0.1)
  barplot(ACF, ylim = c(minu, ymax), main = main, axes = FALSE, 
          ylab = "ACF", ...)
  box(col = elementcolor)
  axis(2, col = elementcolor, cex.axis = 0.8)
  abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
  par(mar = c(4, 4, 0.5, 2) + 0.1)
  barplot(PACF, ylim = c(minu, ymax), axes = FALSE, ylab = "PACF", 
          ...)
  box(col = elementcolor)
  axis(1, col = elementcolor, cex.axis = 0.8, xlab = "lag")
  axis(2, col = elementcolor, cex.axis = 0.8)
  abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
  par(op)
}

