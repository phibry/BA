# testfile Buehl

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






start_in <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")
end_in <- c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28")
start_out <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01")
end_out <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-27")

dates_mat <- as.data.frame(cbind(start_in, end_in, start_out, end_out))

sharpmat=matrix(ncol=3,nrow = 9,data=0)
colnames(sharpmat)<-c("sharpe_olpd","sharpe_net","sharpe_bh")
#set.seed(30)



##data

# how many lags considered
lags=7


#how many standart deviatons for olpd threshold
devi=1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.3

#neurons 
neuron_vec=c(3,2)

# insample or out of sample of net ai 
use_in_samp=F# how many standart deviations considered for telling is stable or not
anz=1000
# ANZAHL REEALSIATIONEN OLPD MAT




for (batch in 1:9)
  
{  
  
  
  # 
  
  #test train split
  in_out_sep=dates_mat$start_out[batch]
  
  start=dates_mat$start_in[batch]
  end=dates_mat$end_out[batch]
  
  
  x_level=log(BTC_USD_27_03_21$`BTC-USD.Close`)[paste(start,"::",end,sep="")]
  logret <- log_ret_27_03_21[paste(start,"::",end,sep="")]
  x=logret
  outtarget=log_ret_27_03_21[paste(in_out_sep,"::",end,sep="")]
  
  
  
  
  if (batch==1)
  {
    first=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,use_in_samp=use_in_samp,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
    alloverperf_olpd=first$perf_nn_out_with_olpd
    alloverperf_nn=first$perf_nn_out
    sharpmat[1,1:3]<-as.numeric(first[1:3])
  }
  
  second=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,use_in_samp=use_in_samp,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
  
  
  
  #alloverperformnace olpd
  alloverperf_olpd=rbind(alloverperf_olpd,second$perf_nn_out_with_olpd)
  alloverperf_olpd <- alloverperf_olpd[ ! duplicated( index(alloverperf_olpd) ),  ]
  #alloverperformance net
  alloverperf_nn=rbind(alloverperf_nn,second$perf_nn_out)
  alloverperf_nn <- alloverperf_nn[ ! duplicated( index(alloverperf_nn) ),  ]
  
  
  sharpmat[batch,1:3]<-as.numeric(second[1:3])
  
  
  
  if (batch == 9){
    
    
    
    olpd_string=  paste("alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
    nn_string=  paste("alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
    sharpmat_string=  paste("sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
    
    assign(olpd_string,alloverperf_olpd) 
    assign(nn_string,alloverperf_nn) 
    assign(sharpmat_string,sharpmat) 
    
    
    
    save(list=olpd_string, file = paste("data/xai/3_2/9",olpd_string,".rda",sep="") ) 
    save(list=nn_string, file = paste("data/xai/3_2/9",nn_string,".rda",sep="") )  
    save(list=sharpmat_string, file = paste("data/xai/3_2/9",sharpmat_string,".rda",sep=""))  
    
  }
  
}

