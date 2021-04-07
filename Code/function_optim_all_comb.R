# optimizing with all combinations


load("data/log_ret_27_03_21.rda")  
logret=log_ret_27_03_21           
x=logret["2018-01-01::"] 
acf(x)   
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6),lag(x,k=7),lag(x,k=8),lag(x,k=9),lag(x,k=10)) 
data_mat<-na.exclude(data_mat)
maxs <- apply(data_mat, 2, max)   #    creating a vector with the maximum per lag 
mins <- apply(data_mat, 2, min)   #    creating a vector with the minimum per lag
scaled_log_ret <- scale(data_mat, center = mins, scale = maxs - mins)  # scaling the returns   # all values betwen  0 1
in_out_sample_separator="2020-05-01"             
train_set <- scaled_log_ret[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled_log_ret[paste(in_out_sample_separator,"/",sep=""),]
train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)
colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)
f <- as.formula(paste   ("lag0 ~"   ,  paste(n[!n %in% "lag0"], collapse = " + ")  ))


source("Code/combination_input_grid.R") # calling funcktin
maxlayer=7
maxneuron=5
combmat=combination_input_grid(maxlayer,maxneuron)

##generating names of rows ###----------------------
ind=rep(NA,dim(combmat)[1])
for(k in 1:dim(combmat)[1])
{
  x=as.vector(combmat[k,])
  ind[k]=toString(as.character(x[x!=0]))
}

# generating empty result matrix--------------------
mati=matrix(nrow=dim(combmat)[1],ncol=3,0)
mati=as.data.frame(mati)
rownames(mati)=ind


for( i in 1: dim(combmat)[1])
{
  x=as.vector(combmat[i,])       # in vektor  umwandeln
  x= x[x!=0]
  # ohne null
  net=estimate_nn(train_set,number_neurons=x,data_mat,test_set,f) # netz erstellen
  mati[i,2]=net$MSE_nn[1]   # insample error
  mati[i,3]=net$MSE_nn[2]   # out of sample error    
  print(mati[i,])
          # namen der spalte 
}


plot(mati[,2],type="l")
lines(mati[,3],col="red")
