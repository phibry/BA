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
