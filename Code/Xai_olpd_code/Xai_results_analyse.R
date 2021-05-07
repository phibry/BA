rm(list=ls())

#loading area ####
source("add/libraries.r") 

load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")
outtarget=log_ret_27_03_21["2020-07-01::"]




#-------------------------------------------------------------------------------
#load dasets #####
#choosing wich dataset should be loaded in envirmonet


#how many standart deviatons for olpd threshold
devi=1.1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.1
#anzahl
anz=1000
#-------------------------------------------------------------------------------


olpd_string=  paste("alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
nn_string=  paste("alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
sharpmat_string=  paste("sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")


load(paste("data/xai/7_7/9",olpd_string,".rda",sep=""))
load(paste("data/xai/7_7/9",nn_string,".rda",sep=""))
load(paste("data/xai/7_7/9",sharpmat_string,".rda",sep=""))


#------------------------------------------------------------------------------

# 
# assign("olpd_1",get(olpd_string))
# assign("nn_1",get(nn_string))
# assign("sharpmat_1",get(sharpmat_string))

# 
# assign("olpd_2",get(olpd_string))
# assign("nn_2",get(nn_string))
# assign("sharpmat_2",get(sharpmat_string))

# assign("olpd_3",get(olpd_string))
# assign("nn_3",get(nn_string))
# assign("sharpmat_3",get(sharpmat_string))




#-------------------------------------------------
#sharperatio over all


sharpe_olpd_1=sqrt(365)*SharpeRatio(olpd_1,FUN="StdDev")
sharpe_nn_1=sqrt(365)*SharpeRatio(nn_1,FUN="StdDev")

sharpe_olpd_2=sqrt(365)*SharpeRatio(olpd_2,FUN="StdDev")
sharpe_nn_2=sqrt(365)*SharpeRatio(nn_2,FUN="StdDev")

sharpe_olpd_3=sqrt(365)*SharpeRatio(olpd_3,FUN="StdDev")
sharpe_nn_3=sqrt(365)*SharpeRatio(nn_3,FUN="StdDev")

sharpe_bh=sqrt(365)*SharpeRatio(outtarget,FUN="StdDev")

#all sharpmats togheter
allsharp=cbind(sharpmat_1[,-3],sharpmat_2[,-3],sharpmat_3[,-3])

#plots####
#-----------------------------------------------------------------------------

#performance cumulated
par(mfrow=c(3,1))

main=paste("Performance cumulated,  2L ,7N, ",as.character(anz)," Reps , different Majority shares")

compare_perf=cbind(cumsum(olpd_1),cumsum(nn_1),cumsum(olpd_2),cumsum(nn_2),cumsum(olpd_3),cumsum(nn_3), cumsum(outtarget))

name=c("olpd 10%","nn 10%","olpd 20%","nn 20%","olpd 30%","nn 30%","Buy and Hold")
colnames(compare_perf)=name
  
colors= c("red","pink","blue","lightblue","black","grey","green")
  
  
#plot.xts(compare_perf,main=main,col=colors)
addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=rep(1,7),
          lwd=rep(2,7),
          ncol=1,
          bg="white")


#sharpe ratios over all

plot(rbind(sharpe_olpd_1,sharpe_nn_1,sharpe_olpd_2,sharpe_nn_2,sharpe_olpd_3,sharpe_nn_3,sharpe_bh),main="sharpe cumulated",xaxt="n",ylab="sharpe"
     ,col=colors,pch=19)
axis(1, at=1:7, labels=name)


#sharperatios per batch

main=paste("Sharpe per Batch,  2L ,7N, ",as.character(anz)," Reps , different Majority shares")
plot(sharpmat_1[,3],type="l",col="green",xlab="batch nr",ylab= "sharpe",lwd=2,ylim=c(min(allsharp),max(allsharp)),main=main)
for (i in 1:6){lines(allsharp[,i],col=colors[i],type="l",lwd=2)}



