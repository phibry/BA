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
# seedsadsaD
# #set.seed(30)
devi=1

#neurons 
neuron_vec=c(3,2)

# insample or out of sample of net ai 
use_in_samp=T# how many standart devioatons considered for telling is stable or not
anz=1000 # ANZAHL REEALSIATIONEN OLPD MAT
#--------------------------------------------------------------------------
#xai_outp>-function(x,lags,in_out_sep,neuron_vec,use_in_samp,anz){

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

for (l in 1:anz)  
{  
  #Neural net fitting for btc with sigmoid
  nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=T)
  net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f,newnet = F,nn=nn)
  
  
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
  OLPD_mat<-transform_OLPD_back_original_data_func(data_xts,data_mat,OLPD_scaled_mat,lm_obj,data)$OLPD_mat
  
  
 if (l==1)
  {
  start=OLPD_mat
  startsignal_in=sign(net$predicted_nn_in_sample)
  startsignal_out=sign(net$predicted_nn)  
  }
 else
  {
  start=start+OLPD_mat 
  startsignal_in=startsignal_in+sign(net$predicted_nn_in_sample)
  startsignal_out=startsignal_out+sign(net$predicted_nn) 
  }
cat("\014")   
print(l)
}  
  
  

    
index(OLPD_mat)<-index(data_xts)  
#mean of all olpd over realisations
OLPD_mat=na.exclude(start/anz)

#signals ########################################################## 
#the most votet gets the signal 
signal_in=sign(startsignal_in)
signal_out=sign(startsignal_out)
  
  

#perf_nn_in<-signal_in*target_in
perf_nn_out<-signal_out*target_out
perf_nn_in<-signal_in*target_in[paste("::",as.Date(in_out_sep)-1,sep="")]



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


deviatmat=apply(OLPD_mat[,-1],2,deviate)
sum_explana=as.xts(apply(deviatmat,1,sum))
signal_olpd<-sum_explana
signal_olpd[]=1
signal_olpd[which(sum_explana>=2 & sum_explana < 3 )]<- 0.5
signal_olpd[which(sum_explana >= 3 )]<-0


sum(signal_out==sign(target_out))/length(signal_out) # out of sample accuracy
sum(signal_in==sign(target_in[paste("::",as.Date(in_out_sep)-1,sep="")]))/length(signal_in) # in sample accuracy



signal_nn_and_olpd=signal_out;signal_nn_and_olpd[which(signal_olpd==0)]<-0;#signal_nn_and_olpd[which(signal_olpd==0.5)]<-0.5
perf_nn_out_with_olpd=signal_nn_and_olpd*target_out

#plots####
par(mfrow=c(3,1))
#plot olpd
plot(OLPD_mat,col=rainbow(ncol(OLPD_mat)))
  for (i in 1:ncol(OLPD_mat))
  mtext(colnames(OLPD_mat)[i],col=rainbow(ncol(OLPD_mat))[i],line=-i)


#plot of decision
plot(signal_olpd)

#how many deviate
plot(sum_explana,type="b")

sharpe_bh=round(sqrt(365)*SharpeRatio(target_out,FUN="StdDev"),3)

sharpe_net=round(sqrt(365)*SharpeRatio(perf_nn_out,FUN="StdDev"),3)
sharpe_net_olpd=round(sqrt(365)*SharpeRatio(perf_nn_out_with_olpd,FUN="StdDev"),3)


plot(cumsum(target_out),main=paste("bh, sharpe:",as.character(sharpe_bh)) )
plot(logret <- log_ret_27_03_21["2020-01-08::2021-02-27"])
plot(cumsum(perf_nn_out),main=paste("nn, sharpe:",as.character(sharpe_net)))
plot(cumsum(perf_nn_out_with_olpd),main=paste("Net Olpd, sharpe:",as.character(sharpe_net_olpd)))


# testing code
par(mfrow=c(2,1))
plot(rbind(net$predicted_nn_in_sample,net$predicted_nn),type="l")
plot(log_ret_27_03_21["2020-01-01::"])
