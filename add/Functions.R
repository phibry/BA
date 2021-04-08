# # feed forward nets ####
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

estimate_nn<-function(train_set,number_neurons,data_mat,test_set,f)
{
  nn <- neuralnet(f,data=train_set,hidden=number_neurons,linear.output=T)
  
  
  # In sample performance
  predicted_scaled_in_sample<-nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  predicted_nn_in_sample<-predicted_scaled_in_sample*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # In-sample MSE
  MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max(data_mat[,1])-min(data_mat[,1])))^2)
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
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





