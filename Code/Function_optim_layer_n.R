source("add/libraries.r") 
# load libraries file
source("add/Functions.r") 

# input #### 

load("data/log_ret_27_03_21.rda")  
logret=log_ret_27_03_21           
x=logret["2018-01-01::"] 
acf(x)   
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6),lag(x,k=7),lag(x,k=8),lag(x,k=9),lag(x,k=10)) 
data_mat<-na.exclude(data_mat)
maxs <- apply(data_mat, 2, max)   #    creating a vector with the maximum per lag 
mins <- apply(data_mat, 2, min)   #    creating a vector with the minimum per lag
scaled_log_ret <- scale(data_mat, center = mins, scale = maxs - mins)  # scaling the returns   # all values betwen  0 1
in_out_sample_separator="2020-05-01"             
train_set <- scaled_log_ret[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled_log_ret[paste(in_out_sample_separator,"/",sep=""),]
train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)
colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)
f <- as.formula(paste   ("lag0 ~"   ,  paste(n[!n %in% "lag0"], collapse = " + ")  ))



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

vec= c(as.vector(t(resmat)),mean(resmat),min(resmat)) 
allresult=rbind(allresult,vec)
setTxtProgressBar(pb, l)
}
close(pb)

par(mfrow=c(2,1))
allresult=allresult[-1,] # row = layers times neurons note last 2 are mean and min of try, 
plot(-allresult[1,1:(maxlayer*maxneuron)],type = "l",xlab=" neurons per layer",ylab="negativ insample error", main= paste("insample error optimzation, Maxneuron:",as.character(maxneuron),"Maxlayer:",as.character(maxlayer)),ylim=c(-0.0018,-0.0013) )
abline(v=seq(maxneuron,maxlayer*maxneuron,maxneuron), col = "red",lty= 2)
for(o in 2:dim(allresult)[2])
  lines(-allresult[o,1:(maxlayer*maxneuron)],col = o)
legend("topleft",lty=2,col = "red",legend = "layer batch")

plot(allresult[,maxlayer*maxneuron+1],main="insample error : mean over optimization",ylim=c(0.0013,0.00172))
abline(h=mean(allresult[,maxlayer*maxneuron+1]),)
points(allresult[,maxlayer*maxneuron+2],main="insample error : min over optimization",col = "red")
abline(h=mean(allresult[,maxlayer*maxneuron+2]),col="red")
legend("bottom",lty=c(2,2),col = c("black","red"),legend = c("mean","minimum"))

return(allresult)
#which( resmat==min(resmat),arr.ind =T) #  index of value
}