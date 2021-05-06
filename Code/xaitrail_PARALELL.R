# testfile Buehl

# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

rm(list=ls())

# load libraries file
source("add/libraries.r") 
#load functions
source("add/Functions.r") 

source("Code/xai_outp_fun.r")

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
percentage= 0.2

#neurons 
neuron_vec=c(7,7)

# insample or out of sample of net ai 
use_in_samp=F# how many standart deviations considered for telling is stable or not
anz=100
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
  save(alloverperf_olpd, file = paste("data/xai/7_7/9","alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),".rda",sep="_") ) 
  save(alloverperf_nn, file = paste("data/xai/7_7/9","alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),".rda",sep="_") )  
  save(sharpmat, file = paste("data/xai/7_7/9","sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),".rda",sep="_") )  
   
}

}




outtarget=log_ret_27_03_21["2020-07-01::"]
     
nnsharpe=sqrt(365)*SharpeRatio(alloverperf_nn,FUN="StdDev")
olpdsharpe=sqrt(365)*SharpeRatio(alloverperf_olpd,FUN="StdDev")
bhsharpe=sqrt(365)*SharpeRatio(outtarget,FUN="StdDev")
     

x=cbind(cumsum(alloverperf_olpd),cumsum(alloverperf_nn), cumsum(outtarget))

par(mfrow=c(1,1))


plot(x,col=c("black","red","blue"))


     
plot(sharpmat[,1],type="l",main="Sharpe out of sample splits",ylab= "sharpe",xlab="split nr")
lines(sharpmat[,2],col="red")
lines(sharpmat[,3],col ="blue")
legend("topleft",legend= c("with xai deviation signal ","only nn signals","buy and hold"),col = c("black","red","blue"),pch = c(1,1,1))
legend("bottomleft",legend= legvec,col = c("black","red","blue"),pch = c(1,1,1))

     

legvec= c(    paste( "overallsharpe=",as.character(round(olpdsharpe,2)) ),paste( "overallsharpe=", as.character(round(nnsharpe,2))),paste("overallsharpe=",as.character(round(bhsharpe,2))))
     
     
     
plot(cumsum(alloverperf_olpd))
plot(cumsum(alloverperf_nn))
plot(cumsum( outtarget))

#return overall performance bh nn and olpd, matrix with sharpe of all 3 per batch, sharpe of all together





wwmax=100

resmat=matrix(nrow=max,ncol = 2, data=0)

for(k in 1:max){
  
  f=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,use_in_samp=use_in_samp,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
  resmat[k,1:2]=f[1:2]
  print(k)
  cat("\014")
}




par(mfrow=c(1,1))
plot(resmat[,1],col="blue",type="l")  #sharpenet olpd
lines(resmat[,2],col="green",type="l") #sharpe net

abline(h=sqrt(365)*SharpeRatio(outtarget,FUN="StdDev"),col= "red")
abline(h=0,col= "green")

mean(resmat[,1])
mean(resmat[,2])

sum(resmat[,1] > resmat[,2])
