# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

rm(list=ls())

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




   ###########################################################
#####  set seed 30

max=300
resmat=matrix(nrow=max,ncol = 2,data=0)

for( i in 1:max)
{
  #set.seed(i)
  #net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f)
  
  nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=T,stepmax = 1e+08)
  net=estimate_nn(train_set,number_neurons=neuron_vec,data_mat,test_set,f,newnet = F,nn=nn)
  
  signal_out=sign(net$predicted_nn)
  perf_nn<-signal_out* target_out
  resmat[i,2]=sum(signal_out==sign(target_out))/length(signal_out)
  
  resmat[i,1]=  sqrt(365)*SharpeRatio(perf_nn,FUN="StdDev")
  cat("\014")
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

##########################################################################################################
#test for plotiing ouitput works only for r base plots




lit=function(){plot(1:10) 
p <- recordPlot()

plot(1:20,col="blue")

l <- recordPlot()

p1 <-recordPlot()
return(list(x11(),p,l) )
       
}


par(mfrow=c(3,1))

lit()


#########################################################################################################



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
devi=1.3
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.1

#neurons 
neuron_vec=c(7,7)

# insample or out of sample of net ai 
use_in_samp=F# how many standart deviations considered for telling is stable or not
anz=100
# ANZAHL REEALSIATIONEN OLPD MAT

outtarget=log_ret_27_03_21["2021-02-27::"]


f=xai_outp(x=x,lags=lags,in_out_sep=in_out_sep,neuron_vec=neuron_vec,use_in_samp=use_in_samp,anz=anz,percentage=percentage,devi=devi,outtarget=outtarget)
f[1]

max=100

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




















##buy and hold
load("data/xai/9alloverperf_bh_ 7_7_10000")


plot(sharpmat[,1],type="l",main="sharpe over 9 splits",ylab= "sharpe",xlab="split nr")
lines(sharpmat[,2],col="red")
lines(sharpmat[,3],col ="blue")
legend("topleft", inset=.02,legend= c("with xai signal","only nn signals","buy and hold"),col = c("black","red","blue"),pch = c(1,1,1))



sqrt(365)*SharpeRatio(alloverperf_nn,FUN="StdDev")
sqrt(365)*SharpeRatio(alloverperf_olpd,FUN="StdDev")
sqrt(365)*SharpeRatio(outi,FUN="StdDev")



plot(cumsum(alloverperf_olpd))
addSeries(cumsum(alloverperf_nn), main = "", on = NA, type = "l", col = "red", lty = 1,lwd = 1, pch = 0)

plot(cumsum(alloverperf_nn))
plot(cumsum( outi))

outtarget=log_ret_27_03_21["2020-07-01::"]

x=cbind(cumsum(alloverperf_nn),cumsum(alloverperf_olpd), cumsum(outtarget))

plot(x,col=c("red","blue","green"))


