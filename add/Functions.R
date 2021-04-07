# # feed forward nets ####
library(neuralnet)
# neuralnet(f,data=train_set,hidden=layer,linear.output=F) # neurnalnet function
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
  pr.nn <- compute(nn,as.matrix(test_set[,2:ncol(test_set)]))
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




##
#looping loiue by Pb####

# this function generates MLPs between 1 and max layer and 1 and maxneuron,  it 
# showes graphically the optimization via repetition  and plots the mean and minimum of the repetitions

# note that thsi function needs the inputs from wildi train set test set  formula f usw from stimate func()
insampleres=function(maxlayer=2,maxneuron=6,rep=10)
{
  # function
  resmat=matrix(nrow = maxlayer,ncol = maxneuron,0)
  allresult=t(rep(1,maxlayer*maxneuron +2))
  minlayer=1;minneuron=1
  for (l in 1:rep)
    
  { 
    pb <- txtProgressBar(min = 1, max = rep, style = 3)
    for( layer_i in minlayer:maxlayer)
    {
      for (neuron_k in minneuron:maxneuron)
      {
        number_neurons=c(rep(neuron_k,layer_i))
        net=estimate_nn(train_set,number_neurons=number_neurons,data_mat,test_set,f)
        resmat[layer_i,neuron_k]=net$MSE_nn[1]
        print(number_neurons)
      }
      
    }
    
    print(paste(as.character(rep-l)," Iterations left"))
    vec= c(as.vector(t(resmat)),mean(resmat),min(resmat)) 
    allresult=rbind(allresult,vec)
    setTxtProgressBar(pb, l)
  }
  close(pb)
  
  par(mfrow=c(2,1))
  allresult=allresult[-1,] # row = layers times neurons note last 2 are mean and min of try, 
  plot(-allresult[1,1:(maxlayer*maxneuron)],type = "l",xlab=" neurons per layer",ylab="negativ insample error", main= paste("insample error optimzation, Maxneuron:",as.character(maxneuron),"Maxlayer:",as.character(maxlayer)),ylim=c(-max(allresult),-min(allresult)) )
  abline(v=seq(maxneuron,maxlayer*maxneuron,maxneuron), col = "red",lty= 2)
  for(o in 2:dim(allresult)[1])
    {lines(-allresult[o,1:(maxlayer*maxneuron)],col = o)}
  legend("topright",lty=2,col = "red",legend = "layer batch")  
  
  plot(allresult[,maxlayer*maxneuron+1],main="insample error : mean over optimization",ylab= "insample error",ylim=c(min(allresult[,maxlayer*maxneuron+2]),max(allresult[,maxlayer*maxneuron+1])))
  abline(h=mean(allresult[,maxlayer*maxneuron+1]),)
  points(allresult[,maxlayer*maxneuron+2],main=paste("mean and min of all " ,as.character(rep)," iterations"),col = "red")
  abline(h=mean(allresult[,maxlayer*maxneuron+2]),col="red")
  legend("bottom",lty=c(2,2),col = c("black","red"),legend = c("mean","minimum"))
  
  return(allresult)
  #which( resmat==min(resmat),arr.ind =T) #  index of value
}

