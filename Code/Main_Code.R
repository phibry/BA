source("add/libraries.r") # load libraries file
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


#Loading data ####

load("data/BTC_USD_27_03_21.rda")  # loading logreturns closing! data xts
logret=log_ret_27_03_21            # shorter variable name





# Feedforwardnets ####
##preparing data ####
x=logret["2018-01-01::"] # only the data from 2017 until now 

acf(x)   # check the dependency structure 
# creating a data matrix with every row is x today and the lags before n
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6)) # only the  6 lags
# other lags 
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6),lag(x,k=7),lag(x,k=8),lag(x,k=9),lag(x,k=10)) 
#data_mat<-cbind(x,lag(x),lag(x,k=2))    

# exclude nas
data_mat<-na.exclude(data_mat)

# because of the perceptron functions are sigmoid ad the classification is betwen 0 and we need to scale the returns
maxs <- apply(data_mat, 2, max)   #    creating a vector with the maximum per lag 
mins <- apply(data_mat, 2, min)   #    creating a vector with the minimum per lag


scaled_log_ret <- scale(data_mat, center = mins, scale = maxs - mins)  # scaling the returns   # all values betwen  0 1
## insample out of sample split ####
# separating in sample / out of sample
in_out_sample_separator="2020-05-01"                  ### note this can be changed and should be fixeed by grouo
train_set <- scaled_log_ret[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled_log_ret[paste(in_out_sample_separator,"/",sep=""),]

# neuralnet package need the data in matrix format, cannot handle xts
train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)


# describing the matrix with the lags ( automatic when we change the lag size)
colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)


## generating a net ####
# neuralnet needs th notation in formuma style similar to regression
f <- as.formula(paste   ("lag0 ~"   ,  paste(n[!n %in% "lag0"], collapse = " + ")  ))

# defining hidden layers in a vector could be anything
layer=hidden=c(5,10,5)

#generating neural net

nn <- neuralnet(f,data=train_set,hidden=layer,linear.output=F)
plot(nn)

net=estimate_nn(train_set,number_neurons=layer,data_mat,test_set,f)

net$MSE_nn




#







