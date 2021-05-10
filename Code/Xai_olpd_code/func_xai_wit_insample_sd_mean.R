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
