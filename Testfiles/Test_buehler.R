# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

# acessing data via coinmarket cap https://coinmarketcap.com/
# from yahoofinance ticker BTC-USD
# *Close price adjusted for splits.
#**Adjusted close price adjusted for both dividends and splits.


#getSymbols("BTC-USD")
#BTC_USD_13_03_21=na.exclude(`BTC-USD`)
#save(BTC_USD_13_03_21, file = "BTC_USD_13_03_21.rda")
load()

#

head(BTC_USD,n=5)
tail(BTC_USD,n=3)      #  

x_level = Cl(BTC_USD)
plot(log(Cl(BTC_USD)))# 
log_ret_13_03_21  = na.exclude(diff(log(Cl(BTC_USD)))) # 




plot(log_ret)
# Line Chart
chartSeries(BTC_USD,type="line", theme=chartTheme("white"))


# Bar Chart
chartSeries(BTC_USD, type="bar",theme=chartTheme("white"))

# Candle Stick Chart
chartSeries(BTC_USD, type="auto", theme=chartTheme("white"))

# 



# log returns btc
lo
# Train-test split




#save to rda file+

#log_ret_13_03_21=na.exclude(log_ret)

getwd()
setwd("C:/Users/buehl/Desktop/BA/BA/data")

save(log_ret_13_03_21, file = "log_ret_13_03_21.rda")


class(log_ret)
str(log_ret)



acf(log_ret_13_03_21)



y.garch<-garchFit(~arma(0,10)+garch(1,1),data=log_ret_13_03_21,delta=2,include.delta=F,include.mean=F,trace=F)
summary(y.garch)



ts.plot(log_ret_13_03_21)

lines(y.garch@sigma.t,col="red")
lines(y.garch@residuals,col="blue")


log_ret_13_03_21_sd<-y.garch@residuals/y.garch@sigma.t

ts.plot(log_ret_12_03_21_sd)
acf(log_ret_12_03_21_sd)

save(log_ret_13_03_21_sd, file = "log_ret_13_03_21_sd.rda")





train_set <- scaled[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled[paste(in_out_sample_separator,"/",sep=""),]

train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)



estimate_nn<-function(train_set,number_neurons,data_mat,test_set,f)
{
  nn <- neuralnet(f,data=train_set,hidden=number_neurons,linear.output=T)
  
  
  # In sample performance
  predicted_scaled_in_sample<-nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  predicted_nn_in_sample<-predicted_scaled_in_sample*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # In-sample MSE
  MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max(data_mat[,1])-min(data_mat[,1])))^2)
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
  pr.nn <- compute(nn,as.matrix(test_set[,2:ncol(test_set)]))
  predicted_scaled<-pr.nn$net.result
  # Results from NN are normalized (scaled)
  # Descaling for comparison
  predicted_nn <- predicted_scaled*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  test.r <- test_set[,1]*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
  # Calculating MSE
  MSE.out.nn <- mean((test.r - predicted_nn)^2)
  
  # Compare in-sample and out-of-sample
  MSE_nn<-c(MSE.in.nn,MSE.out.nn)
  return(list(MSE_nn=MSE_nn,predicted_nn=predicted_nn,predicted_nn_in_sample=predicted_nn_in_sample))
  
}








