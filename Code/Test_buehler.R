# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

rm(list=ls())

source("add/libraries.r") 
# load libraries file
source("add/Functions.r") #load functions

#----------!!dont do this unless you want to load fresh data!!!-------------------------------#
# Downloading Data ####

# Acessing data via coinmarket cap https://coinmarketcap.com/
# from yahoofinance ticker BTC-USD
# *Close price adjusted for splits.
#**Adjusted close price adjusted for both dividends and splits.

#saving original data------------------------------------#
#getSymbols("BTC-USD") # loads the newest data from quandl
#BTC_USD_27_03_21=na.omit(`BTC-USD`)
#save(BTC_USD_27_03_21, file = "data/BTC_USD_27_03_21.rda")

#create an save log returns-------------------------------#  
#log_ret_27_03_21  = na.exclude(diff(log(Cl(BTC_USD_27_03_21)))) # saveing the logretursn of closing data Cl()
#save(log_ret_27_03_21, file = "data/log_ret_27_03_21.rda")  
# 
#---------------------------------------------------------------------------------------------#

load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")


# tryin a sma on logrets ####

#sma
btc=`BTC_USD_27_03_21`
logret=diff(log(SMA(Cl(btc),3)))["2020-01-01::"]
x=logret

#logret normal

#logret=log_ret_27_03_21["2020-01-01::"]
x_level=log(BTC_USD_27_03_21$`BTC-USD.Close`)["2020-01-01::"]
logret <- log_ret_27_03_21["2020-01-01::"]
x=logret



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
    assign("f",f,.GlobalEnv)
    }
  
  
  return(list(data_mat=data_mat,
              target_in=target_in,
              target_out=target_out,
              train_set=train_set,
              test_set=test_set,
              f=f))
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

#number_neurons<-neuron_vec
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




x=logret

#°source(paste(getwd(),"/OLPD_generic_calculate_oeko3.R",sep=""))
#----------------------
# Classic linear regression: applied to Bid-prices
# Specify target and explanatory data: we use first six lags based on above data analysis
#x<-ret<-na.omit(diff(log(dat$Bid)))
#x_level<-log(dat$Bid)
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6),lag(x,k=7))
# Check length of time series before na.exclude
data_mat<-na.exclude(data_mat)

#--------------------------------------------------------------------
# Specify in- and out-of-sample episodes
#in_out_sample_separator<-index(x_level)[nrow(x_level)]
in_out_sample_separator="2021-02-27"














target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
target_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),1]


# lm only used after for coefficients

explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
explanatory_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),2:ncol(data_mat)]
train<-cbind(target_in,explanatory_in)
test<-cbind(target_out,explanatory_out)
lm_obj<-lm(target_in~explanatory_in)
summary(lm_obj)
#-------------------------------------------------------------------------------
#Neural net fitting

maxs <- apply(data_mat, 2, max)
mins <- apply(data_mat, 2, min)
# Transform data into [0,1]
scaled <- scale(data_mat, center = mins, scale = maxs - mins)
apply(scaled,2,min)
apply(scaled,2,max)

train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]

train_set<-as.matrix(train_set_xts)
test_set<-as.matrix(test_set_xts)

colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")

n <- colnames(train_set)
# Model: target is current bitcoin, all other variables are explanatory
f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))





#set.seed(30)
neuron_vec=c(3,2)
use_in_samp=F
nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=F)

#plot(nn)


# Induce infinitesimal perturbations to data and fit regression to output

delta<-1.e-5
epsilon<-1.e-4


if (use_in_samp)
{
  # Smoother (in-sample data)
  data<-train_set
  data_xts<-train_set_xts
} else
{
  # Rougher (out-sample data)
  data<-test_set
  data_xts<-test_set_xts
}

for (i in 1:(nrow(data)))
{
  x<-matrix(data[i,2:ncol(data)],nrow=1)
  colnames(x)<-colnames(data)[2:ncol(data)]
  OLPD_scaled_obj<-OLPD_func(x,delta,epsilon,nn)
  
  if (i==1)
  {
    OLPD_scaled_mat<-OLPD_scaled_obj$effect
  } else
  {
    OLPD_scaled_mat<-rbind(OLPD_scaled_mat,OLPD_scaled_obj$effect)
  }

  
}


OLPD_mat_obj<-transform_OLPD_back_original_data_func(data_xts,data_mat,OLPD_scaled_mat,lm_obj,data)

OLPD_mat<-OLPD_mat_obj$OLPD_mat
is.xts(OLPD_mat)
index(OLPD_mat)<-index(data_xts)


par(mfrow=c(2,1))
plot(OLPD_mat,col=rainbow(ncol(OLPD_mat)))
for (i in 1:ncol(OLPD_mat))
  mtext(colnames(OLPD_mat)[i],col=rainbow(ncol(OLPD_mat))[i],line=-i)
plot(x_level[index(OLPD_mat)])







#####################p##################
deviate= function(x,deviationscaling=1)
  #this function checks if value is > mean + scaling * std and returns 1 if true 0 if not
{
  if(sum(is.na(x))!=0){print("nas detected")}
  x=abs(x)
  deviatevalind=which(x>mean(x)+deviationscaling*stdev(x))
  x[]=FALSE
  x[deviatevalind]=TRUE
  return(x)
}

?apply
OLPD_mat=na.exclude(OLPD_mat)
deviatmat=apply(OLPD_mat,2,deviate)


sum_explana=as.xts(apply(deviatmat,1,sum))
plot(sum_explana,type="b")




decision<-sum_explana
decision[]=1
decision[which(sum_explana>=2 & sum_explana < 3 )]<- 0.5
decision[which(sum_explana >= 3 )]<-0
plot(decision)

#################################################
par(mfrow=c(2,1))
net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f,newnet = F,nn=nn)

signal_in=sign(net$predicted_nn_in_sample)
signal_out=sign(net$predicted_nn)  


#perf_nn_in<-signal_in*target_in
perf_nn_out<-signal_out*target_out
perf_nn_in<-signal_in*target_in


plot(cumsum(target_out))
charts.PerformanceSummary(perf_nn_out, main="performance via SMA10")
sqrt(365)*SharpeRatio(perf_nn_out,FUN="StdDev")

plot(target_out)
plot(net$predicted_nn)


charts.PerformanceSummary(target_out, main="Buy and hold")
sqrt(365)*SharpeRatio(target_out,FUN="StdDev")





sum(signal_out==sign(target_out))/length(signal_out)
sum(signal_in==sign(target_in))/length(signal_in)






   ###########################################################
#####  set seed 30

max=300
resmat=matrix(nrow=max,ncol = 2,data=0)

for( i in 1:max)
{
  set.seed(i)
  net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f)
  signal_out=sign(net$predicted_nn)
  perf_nn<-signal_out* target_out
  resmat[i,2]=sum(signal_out==sign(target_out))/length(signal_out)
  
  resmat[i,1]=  sqrt(365)*SharpeRatio(perf_nn,FUN="StdDev")
  print(i)
}


par(mfrow=c(2,1))

plot(resmat[,1],type="l",main="")
abline(h=sqrt(365)*SharpeRatio(target_out,FUN="StdDev"),col= "red")
abline(h=0,col = "green")
mean(resmat[,1]) 

length(which(resmat[,1]> as.numeric(sqrt(365)*SharpeRatio(target_out,FUN="StdDev")) ))/max
length(which(resmat[,1]>0))/max

plot(resmat[,2],type="l")

mean(resmat[,2])
