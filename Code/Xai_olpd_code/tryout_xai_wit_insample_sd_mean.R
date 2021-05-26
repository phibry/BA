
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




#set.seed(30)


##data
x=logret
# how many lags considered
lags=7
#test train split
in_out_sep="2021-01-01"

#how many standart deviatons for olpd threshold
devi=1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.2

#neurons 
neuron_vec=c(7,7)

# insample or out of sample of net ai 
intercept=F# use the intercept for evaluation?
anz=100
# ANZAHL REEALSIATIONEN OLPD MAT

outtarget=log_ret_27_03_21[paste(in_out_sep,"::",sep="")]

plot=T

lower=2
upper=3





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
  class (OLPD_mat_out)
    
    plot.xts(OLPD_mat_out["::2021-02-08."],col=rainbow(ncol(OLPD_mat_out)),main="LPD Out of Sample")
    for (i in 1:ncol(OLPD_mat_out))
       mtext(colnames(OLPD_mat_out)[i],col=rainbow(ncol(OLPD_mat_out))[i],line=-i)

    # save(OLPD_mat_out, file = "data/xai/7_7_withsignal_xai_in/OLPD_mat_out_plot.rda")
    # save(mean_in, file = "data/xai/7_7_withsignal_xai_in/OLPD_mat_in_mean.rda")
    # save(sd_in, file = "data/xai/7_7_withsignal_xai_in/OLPD_mat_in_sd.rda")
    # 
    

    abline(h=mean_in[-1],lwd=2)
    
    
    meanline=rep(as.numeric(mean_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))
    meanup=rep(as.numeric(mean_in[4]+devi*sd_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))
    meandown=rep(as.numeric(mean_in[4]-devi*sd_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))
    
    meanline=reclass(meanline,OLPD_mat_out["::2021-02-08"])
    meanup=reclass(meanup,OLPD_mat_out["::2021-02-08"])
    meandown=reclass(meandown,OLPD_mat_out["::2021-02-08"])
    
    
    
    lines(meanline,lwd=2,col="black")
    lines(meanup,lwd=2,col="red",lty=2)
    lines(meandown,lwd=2,col="red",lty=2)
    
    name=c("arithmetic mean ffrom insample lag_j","standard deviation from insample lag_j * λ")
    addLegend("bottomleft", 
              legend.names=name,
              col=c("black","red"),
              lty=c(1,2),
              lwd=c(1,1),
              ncol=1,
              bg="white")
    
    
    
    
    
    
    text(x = as.POSIXct("2021-02-01"), y =0, labels = "RSquared = 0.5", cex=2)
    
    
    devi=2
    
    
    abline(h=as.numeric(mean_in[4]+devi*sd_in[4]),col="red",lwd=2,lty=2)
    abline(h=as.numeric(mean_in[4]-devi*sd_in[4]),col="red",lwd=2,lty=2)
    
    
    
    
    
    plot(OLPD_mat_in,col=rainbow(ncol(OLPD_mat_in)))
    for (i in 1:ncol(OLPD_mat_in))
      mtext(colnames(OLPD_mat_in)[i],col=rainbow(ncol(OLPD_mat_in))[i],line=-i)
    
    
        
    
    plot(signal_olpd,main="Final signal LPD")
    
    
    
    
    
    # save(sum_explana, file = "data/xai/7_7_withsignal_xai_in/sum_explana_out_plot.rda")
    
    plot(sum_explana["::2021-02-08"],type=c("b"),main="Sum of lags deviateing λ*sdY from Mean(Y)")
   
    
    sum_with_lines=na.exclude(cbind(as.xts(sum_explana),lineup,linedown))
    
    lineup=rep(4.0,nrow(sum_explana["::2021-02-08"]))
    linedown=rep(2.0,nrow(sum_explana["::2021-02-08"]))
    
  
    lineup=reclass(lineup,sum_explana["::2021-02-08"])
    linedown=reclass(linedown,sum_explana["::2021-02-08"])
    

     
    lines(lineup,lwd=2,col="red",lty=1)
    lines(linedown,lwd=2,col="green",lty=1)
    
    

    name=c("η upper","η lower")
    addLegend("topright", 
              legend.names=name,
              col=c("red","green"),
              lty=c(1,1),
              lwd=c(2,2),
              ncol=1,
              bg="white")
    
    
    
    
    
    
    
    
    
    
        
    
    
    plot(target_out)
    
    
    plot(cumsum(target_out),main=paste("buy and hold, sharpe:",as.character(sharpe_bh)) )
    
    
    plot(cumsum(perf_nn_out),main=paste(as.character(anz),"n nets",as.character(neuron_vec),", sharpe:",as.character(sharpe_net)))
    
    
    plot(cumsum(perf_nn_out_with_olpd),main=paste("Net Olpd, sharpe:",as.character(sharpe_net_olpd)))
  }
  

