# testfile Buehl



rm(list=ls())

# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

source("add/libraries.r") 
# load libraries file
source("add/Functions.r") 
#load functions

#----------!!dont do this unless you want to load fresh data!!!-------------------------------#
# Downloading Data ####

# Acessing data via coinmarket cap https://coinmarketcap.com/
# from yahoofinance ticker BTC-USD
# *Close price adjusted for splits.
#**Adjusted close price adjusted for both dividends and splits.

#saving original data------------------------------------#
# getSymbols("BTC-USD") # loads the newest data from quandl
# BTC_USD_27_03_21=na.omit(`BTC-USD`)
# save(BTC_USD_27_03_21, file = "data/BTC_USD_03_06_21.rda")

# create an save log returns-------------------------------#
# log_ret_27_03_21  = na.exclude(diff(log(Cl(BTC_USD_27_03_21)))) # saveing the logretursn of closing data Cl()
# save(log_ret_27_03_21, file = "data/log_ret_03_06_21.rda")



#loading data
load("data/BTC_USD_03_06_21.rda")
load("data/log_ret_03_06_21.rda")






start_in <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01","2020-11-01")
end_in <- c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28","2021-03-31","2021-04-30")
start_out <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01","2021-04-01","2021-05-01")
end_out <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-31","2021-04-30","2021-05-31")




dates_mat <- as.data.frame(cbind(start_in, end_in, start_out, end_out))

sharpmat=matrix(ncol=4,nrow = 11,data=0)
colnames(sharpmat)<-c("sharpe_nn_olpd","sharpe_net","sharpe_bh","sharpe_lpd")
#set.seed(30)



##data

# how many lags considered
lags=7


#how many standart deviatons for olpd threshold
devi=1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.2

#neurons 
neuron_vec=c(7,7)

# insample or out of sample of net ai 
use_in_samp=F# how many standart deviations considered for telling is stable or not
anz=1000
# ANZAHL REEALSIATIONEN OLPD MAT

save_separate=F

    
    
    
    
    

    
    for (batch in 1:11)
      
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
        first=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,intercept=F,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
        alloverperf_olpd=first$perf_nn_out_with_olpd
        alloverperf_nn=first$perf_nn_out
        alloverperf_lpd=first$perf_lpd_out
        
        
        
        sharpmat[1,1:4]<-as.numeric(first[1:4])
        nn_signal=first$signal_out
        olpd_signal=first$signal_olpd
      }
      else{
        second=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,intercept=F,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
        
        
        
        #alloverperformnace olpd
        alloverperf_olpd=rbind(alloverperf_olpd,second$perf_nn_out_with_olpd)
        ### removing of duplicate not reewuired anymore  #alloverperf_olpd <- alloverperf_olpd[ ! duplicated( index(alloverperf_olpd) ),  ]
        #alloverperformance net
        alloverperf_nn=rbind(alloverperf_nn,second$perf_nn_out)
        #alloverperf_nn <- alloverperf_nn[ ! duplicated( index(alloverperf_nn) ),  ]
        alloverperf_lpd=rbind(alloverperf_lpd,second$perf_lpd_out)
        #alloverperf_nn <- alloverperf_nn[ ! duplicated( index(alloverperf_nn) ),  ]
        
        #sharpe
        sharpmat[batch,1:4]<-as.numeric(second[1:4])
        
        #
        nn_signal=rbind(nn_signal,second$signal_out)
        olpd_signal=rbind(olpd_signal,second$signal_olpd)
      }
      
      if (batch == 11){
        
        data_string= paste("obj","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        
        
        olpd_string=  paste("alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        nn_string=  paste("alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        lpd_string=  paste("alloverperf_lpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        sharpmat_string=  paste("sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        olpd_signal_string=  paste("olpd_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        nn_signal_string=  paste("nn_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
        
        
        
        assign(olpd_string,alloverperf_olpd) 
        assign(nn_string,alloverperf_nn) 
        assign(lpd_string,alloverperf_lpd) 
        assign(sharpmat_string,sharpmat)
        assign(nn_signal_string,nn_signal) 
        assign(olpd_signal_string,olpd_signal)     
        
        
        
        if(save_separate)
        {
          save(list=olpd_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",olpd_string,".rda",sep="") ) 
          save(list=nn_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",nn_string,".rda",sep="") )  
          save(list=sharpmat_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",sharpmat_string,".rda",sep=""))
          save(list=nn_signal_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",nn_signal_string,".rda",sep="") )  
          save(list=olpd_signal_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",olpd_signal_string,".rda",sep=""))
          save(list=lpd_signal_string, file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",lpd_signal_string,".rda",sep=""))
          
        }
        
        
        save(list=c(olpd_string, nn_string,lpd_string, sharpmat_string,nn_signal_string,olpd_signal_string),file = paste("data/xai/7_7_withsignal_xai_in_outlook/9",data_string,".rda",sep="") ) 
        
        
        
        
      }
      
    }
    
#end of loop










