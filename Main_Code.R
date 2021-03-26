#Loading data ####
load("data/log_ret_13_03_21.rda")  # loading logreturns closing! data xts
logret=log_ret_13_03_21            # shorter variable name


# Feedforwardnets ####
##preparing data ####
x=logret

data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6)) # only the first 6 lags
# other lags 
data_mat<-cbind(x,lag(x),lag(x,k=2))    

# exclude nas
data_mat<-na.exclude(data_mat)

# because of the perceptron functions are sigmoid ad the classification is betwen 0 and we need to scale the returns
maxs <- apply(data_mat, 2, max)   #    creating a vector with the maximum per lag 
mins <- apply(data_mat, 2, min)   #    creating a vector with the minimum per lag


scaled_log_ret <- scale(data_mat, center = mins, scale = maxs - mins)  # scaling the returns   # all values betwen  0 1
## insample out of sample split ####
# separating in sample / out of sample
in_out_sample_separator="2019-12-31"                  ### note this can be changed and should be fixeed by grouo
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
layer=hidden=c(100,100,1000)

#generating neural net
set
nn <- neuralnet(f,data=train_set,hidden=layer,linear.output=F)


nn
plot(nn)

