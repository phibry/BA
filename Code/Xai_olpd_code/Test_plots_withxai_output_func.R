
rm(list=ls())
# load libraries file
source("add/libraries.r") 
#load functions
source("add/Functions.r") 

getwd()
#loading data
load("data/BTC_USD_27_03_21.rda")
load("data/log_ret_27_03_21.rda")




# 
x_level=log(BTC_USD_27_03_21$`BTC-USD.Close`)["2020-01-01::"]
logret <- log_ret_27_03_21["2020-01-01::"]




set.seed(30)


##data
x=logret
# how many lags considered
lags=7
#test train split
in_out_sep="2021-02-27"

#how many standart deviatons for olpd threshold
devi=1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.1

#neurons 
neuron_vec=c(7,7)

# insample or out of sample of net ai 
use_in_samp=T# how many standart deviations considered for telling is stable or not
anz=100
# ANZAHL REEALSIATIONEN OLPD MAT

outtarget=log_ret_27_03_21["2021-02-27::"]

plot=T


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
  
  
  
  
  
  
  
  
  
  
  
  
  
  index(OLPD_mat_in)<-index(data_xts_in)  
  index(OLPD_mat_out)<-index(data_xts_out) 
  #mean of all olpd over realisations
  
  OLPD_mat_in=na.exclude(start_in/anz)
  OLPD_mat_out=na.exclude(start_out/anz)
  
plot(OLPD_mat_in)  
plot(OLPD_mat_out)   
  

mean_in=apply(OLPD_mat_in,2,mean)
sd_in=apply(OLPD_mat_in,2,sd)




deviate= function(x,deviationscaling=devi,sd_in,) # devi is assigned before 
  #this function checks if value is > mean + scaling * std and returns 1 if true 0 if not
{
  if(sum(is.na(x))!=0){print("nas detected")}
  x=abs(x)
  deviatevalind=which(x>mean(x)+deviationscaling*stdev(x))
  x[]=FALSE
  x[deviatevalind]=TRUE
  return(x)
}


deviatmat=apply(OLPD_mat_out[,-1],2,deviate)
sum_explana=as.xts(apply(deviatmat,1,sum))
signal_olpd<-sum_explana
signal_olpd[]=1
signal_olpd[which(sum_explana>=2 & sum_explana < 3 )]<- 0.5
signal_olpd[which(sum_explana >= 3 )]<-0










  #signals ##########################################################--------------------------------------------
  
  # neuralnet signals
  
  #signum of the most votet 50% negative + 50 % positive get a zero , >50% negative & <50%positive get a -1  
  
  signal_in=sign(startsignal_in)
  signal_out=sign(startsignal_out)
  
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
    
    
    plot(OLPD_mat,col=rainbow(ncol(OLPD_mat)))
    for (i in 1:ncol(OLPD_mat))
      mtext(colnames(OLPD_mat)[i],col=rainbow(ncol(OLPD_mat))[i],line=-i)
    
    
    plot(signal_olpd,main="final signal")
    
    
    plot(sum_explana,type="b",main=paste("lags deviateing:",as.character(devi),"from mean"))
    
    
    
    
    plot(target_out)
    
    
    plot(cumsum(target_out),main=paste("buy and hold, sharpe:",as.character(sharpe_bh)) )
    
    
    plot(cumsum(perf_nn_out),main=paste(as.character(anz),"n nets",as.character(neuron_vec),", sharpe:",as.character(sharpe_net)))
    
    
    plot(cumsum(perf_nn_out_with_olpd),main=paste("Net Olpd, sharpe:",as.character(sharpe_net_olpd)))
  }
  

