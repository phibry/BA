# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

rm(list=ls())
# load libraries file
source("add/libraries.r") 
#load functions
source("add/Functions.r") 



#loading data
load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")




# 
x_level=log(BTC_USD_27_03_21$`BTC-USD.Close`)["2020-01-01::"]
logret <- log_ret_27_03_21["2020-01-01::"]


# input area #####----------------------------------------------------

##data
x=logret
# how many lags considered
lags=7
#test train split
in_out_sep="2021-02-27"
# seed
# #set.seed(30)

#neurons 
neuron_vec=c(3,2)

# insample or out of sample of net ai 
use_in_samp=F# how many standart devioatons considered for telling is stable or not
devi=1
#--------------------------------------------------------------------------


#preparing data for nn with autoassign =T results directly in global env
data_function(x, lags, in_out_sep, autoassign = T)

target_in<-data_mat[paste("/",in_out_sep,sep=""),1]
target_out<-data_mat[paste(in_out_sep,"/",sep=""),1]


# lm only used, for coefficients
explanatory_in<-data_mat[paste("/",in_out_sep,sep=""),2:ncol(data_mat)]
explanatory_out<-data_mat[paste(in_out_sep,"/",sep=""),2:ncol(data_mat)]

lm_obj<-lm(target_in~explanatory_in)
#summary(lm_obj)



#Neural net fitting for btc with sigmoid
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


par(mfrow=c(3,1))
plot(OLPD_mat,col=rainbow(ncol(OLPD_mat)))
for (i in 1:ncol(OLPD_mat))
  mtext(colnames(OLPD_mat)[i],col=rainbow(ncol(OLPD_mat))[i],line=-i)
plot(x_level[index(OLPD_mat)])








#####################p##################
deviate= function(x,deviationscaling=devi) # devi is assigned before 
  #this function checks if value is > mean + scaling * std and returns 1 if true 0 if not
{
  if(sum(is.na(x))!=0){print("nas detected")}
  x=abs(x)
  deviatevalind=which(x>mean(x)+deviationscaling*stdev(x))
  x[]=FALSE
  x[deviatevalind]=TRUE
  return(x)
}

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
  #set.seed(i)
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
