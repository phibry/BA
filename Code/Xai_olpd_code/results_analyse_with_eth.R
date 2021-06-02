rm(list=ls())

#loading area ####
source("add/libraries.r") 

load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")
outtarget=log_ret_27_03_21["2020-07-01::"]

load("data/GARCH_vola_predictions/garch_out_signal.rda")


#ether
load("data/ETH_2021-05-05.rda")
head(ETH)
tail(ETH)
logret_eth <-na.omit( diff(log(ETH$`ETH-USD.Close`)))
colnames(logret_eth) <- "ETH_LR"
head(logret_eth)
tail(logret_eth)

outtarget_eth=logret_eth["2020-07-01::2021-03-27"]


#-------------------------------------------------------------------------------

#how many standart deviatons for olpd threshold
devi=1
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.3
anz=1000
#---------
#-------------------------------------------------------------------------------
data_string= paste("obj","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
load(paste("data/xai/7_7_withsignal_xai_in/9",data_string,".rda",sep=""))

olpd_string=  paste("alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
nn_string=  paste("alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
sharpmat_string=  paste("sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
olpd_signal_string=  paste("olpd_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
nn_signal_string=  paste("nn_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")



#------------------------------------------------------------------------------


# assign("olpd_1",get(olpd_string))
# assign("nn_1",get(nn_string))
# assign("sharpmat_1",get(sharpmat_string))
# assign("nn_signal_1",get(nn_signal_string))
# assign("olpd_signal_1",get(olpd_signal_string))

# 
# assign("olpd_2",get(olpd_string))
# assign("nn_2",get(nn_string))
# assign("sharpmat_2",get(sharpmat_string))
# assign("nn_signal_2",get(nn_signal_string))
# assign("olpd_signal_2",get(olpd_signal_string))


# assign("olpd_3",get(olpd_string))
# assign("nn_3",get(nn_string))
# assign("sharpmat_3",get(sharpmat_string))
# assign("nn_signal_3",get(nn_signal_string))
# assign("olpd_signal_3",get(olpd_signal_string))

# 




#-------------------------------------------------



cbind(nn_signal_1,garch_out_signal,olpd_signal_1)



##all 3 together define better rule!

signal_fall3_1=nn_signal_1
signal_fall3_1[which(garch_out_signal==0 || olpd_signal_1==0)]<-0
signal_fall3_1[which(olpd_signal_1==0.5)]<-0.5
perfall3_1=signal_fall3_1*outtarget


signal_fall3_2=nn_signal_2
signal_fall3_2[which(garch_out_signal==0 || olpd_signal_2==0)]<-0
signal_fall3_2[which(olpd_signal_2==0.5)]<-0.5
perfall3_2=signal_fall3_2*outtarget


signal_fall3_3=nn_signal_3
signal_fall3_3[which(garch_out_signal==0 || olpd_signal_3==0)]<-0
signal_fall3_3[which(olpd_signal_3==0.5)]<-0.5
perfall3_3=signal_fall3_3*outtarget




## only lpd ####

lpdperf1=outtarget
lpdperf1[which(olpd_signal_1==0)]<-0
lpdperf1[which(olpd_signal_1==0.5)]<-lpdperf1[which(olpd_signal_1==0.5)]*0.5


lpdperf2=outtarget
lpdperf2[which(olpd_signal_2==0)]<-0
lpdperf2[which(olpd_signal_2==0.5)]<-lpdperf1[which(olpd_signal_2==0.5)]*0.5


lpdperf3=outtarget
lpdperf3[which(olpd_signal_3==0)]<-0
lpdperf3[which(olpd_signal_3==0.5)]<-lpdperf1[which(olpd_signal_3==0.5)]*0.5




# trading with ether ####----------------------------------------------------------------------


# trading rule1: just buy ether when btc signal is 0 "no rule"

#lpd performed better with only zeroes therefore all 0.5 signals are 1
olpd_signal_1_only1=olpd_signal_1
olpd_signal_1_only1[which(olpd_signal_1==0.5)]<-1

#lpd performed better with only zeroes therefore all 0.5 signals are 1
olpd_signal_2_only1=olpd_signal_2
olpd_signal_2_only1[which(olpd_signal_2==0.5)]<-1

#lpd performed better with only zeroes therefore all 0.5 signals are 1
olpd_signal_3_only1=olpd_signal_3
olpd_signal_3_only1[which(olpd_signal_3==0.5)]<-1



#lpd signal with only 1--------------------------------------
olpd_1_eth=olpd_1
dummy1=which(olpd_1_eth==0)
olpd_1_eth[dummy1,]=outtarget_eth[dummy1,]

olpd_2_eth=olpd_2
dummy2=which(olpd_2_eth==0)
olpd_2_eth[dummy2,]=outtarget_eth[dummy2,]

olpd_3_eth=olpd_3
dummy3=which(olpd_3_eth==0)
olpd_3_eth[dummy3,]=outtarget_eth[dummy3,]




# trading rule2: signal lpd nn is zero -> ether if the last 5 returns were positive -> negative
# if last 5 were positive -> negative else just buy 


### consecutive rule ####-------------------------------------------------------------------------------
## if 5 consecutive values the next value is - sign of the nconsecutives
checkdata=sign(logret_eth["2020-06-26::2021-03-27"])
filler=checkdata
filler[]<-0
cons=6
for(i in (cons):length(checkdata))
{
   if(i==cons){filler[i]=100}
   if(abs(sum(checkdata[i-1],checkdata[i-2],checkdata[i-3],checkdata[i-4],checkdata[i-5])) == 5)
   {filler[i]=-sign(checkdata[i-1])}
}
filler=filler["2020-07-01::2021-03-27"]



olpd_1_eth=olpd_1
dummy1=which(olpd_1_eth==0)
olpd_1_eth[dummy1,]=outtarget_eth[dummy1,]
com1=which(filler!= 0 & olpd_1_eth==0)
olpd_1_eth[dummy2]<-filler[com1]*outtarget_eth[com1]


olpd_2_eth=olpd_2
dummy2=which(olpd_2_eth==0)
olpd_2_eth[dummy2,]=outtarget_eth[dummy2,]
com2=which(filler!= 0 & olpd_2_eth==0)
olpd_2_eth[dummy2]<-filler[com2]*outtarget_eth[com2]


olpd_3_eth=olpd_3
dummy3=which(olpd_3_eth==0)
olpd_3_eth[dummy3,]=outtarget_eth[dummy3,]
com3=which(filler!= 0 & olpd_3_eth==0)
olpd_3_eth[dummy3]<-filler[com3]*outtarget_eth[com3]








#sharperatio over all-------------------------------------------------------------------------------------
sharpe_bh=sqrt(365)*SharpeRatio(outtarget,FUN="StdDev")



sharpe_olpd_1=sqrt(365)*SharpeRatio(olpd_1,FUN="StdDev")
sharpe_nn_1=sqrt(365)*SharpeRatio(nn_1,FUN="StdDev")
sharpe_lpd_1=sqrt(365)*SharpeRatio(lpdperf1,FUN="StdDev")
sharpe_all3_1=sqrt(365)*SharpeRatio(perfall3_1,FUN="StdDev")
sharpe_olpd_1_eth=sqrt(365)*SharpeRatio(olpd_1_eth,FUN="StdDev")



sharpe_olpd_2=sqrt(365)*SharpeRatio(olpd_2,FUN="StdDev")
sharpe_nn_2=sqrt(365)*SharpeRatio(nn_2,FUN="StdDev")
sharpe_lpd_2=sqrt(365)*SharpeRatio(lpdperf2,FUN="StdDev")
sharpe_all3_2=sqrt(365)*SharpeRatio(perfall3_2,FUN="StdDev")
sharpe_olpd_2_eth=sqrt(365)*SharpeRatio(olpd_2_eth,FUN="StdDev")


sharpe_olpd_3=sqrt(365)*SharpeRatio(olpd_3,FUN="StdDev")
sharpe_nn_3=sqrt(365)*SharpeRatio(nn_3,FUN="StdDev")
sharpe_lpd_3=sqrt(365)*SharpeRatio(lpdperf3,FUN="StdDev")
sharpe_all3_3=sqrt(365)*SharpeRatio(perfall3_3,FUN="StdDev")
sharpe_olpd_3_eth=sqrt(365)*SharpeRatio(olpd_3_eth,FUN="StdDev")





#all sharpmats togheter

sharpmat_1[,1:4]=sharpmat_1[,c(2,4,1,3)]
colnames(sharpmat_1)=c("sharpe_net","sharpe_lpd","sharpe_nn_olpd","sharpe_bh")

sharpmat_2[,1:4]=sharpmat_2[,c(2,4,1,3)]
colnames(sharpmat_2)=c("sharpe_net","sharpe_lpd","sharpe_nn_olpd","sharpe_bh")

sharpmat_3[,1:4]=sharpmat_3[,c(2,4,1,3)]
colnames(sharpmat_3)=c("sharpe_net","sharpe_lpd","sharpe_nn_olpd","sharpe_bh")




allsharp=cbind(sharpmat_1[,-4],sharpmat_2[,-4],sharpmat_3[,-4])

ten=c(sharpe_olpd_1,sharpe_nn_1,sharpe_lpd_1,sharpe_all3_1,sharpe_bh) #10%
twenty=c(sharpe_olpd_2,sharpe_nn_2,sharpe_lpd_2,sharpe_all3_2,sharpe_bh) #20%
thirty=c(sharpe_olpd_3,sharpe_nn_3,sharpe_lpd_3,sharpe_all3_3,sharpe_bh) #20%



df=data.frame(rbind(ten,twenty,thirty))
colnames(df)=c("lpd+nn","nn","lpd","lpd+nn+garch","bh")

allsharpestring=paste("sd= ",as.numeric(devi),"sharpe_overall")
batchsharpestring=paste("sd= ",as.numeric(devi),"sharpe_batch")

assign(allsharpestring,df)
assign(batchsharpestring,allsharp)

# save(list=allsharpestring, file = paste("data/xai/7_7_withsignal_xai_in/",allsharpestring,".rda",sep=""))
# save(list=batchsharpestring, file = paste("data/xai/7_7_withsignal_xai_in/",batchsharpestring,".rda",sep=""))

# 







#plots####
#-----------------------------------------------------------------------------

#eth and best olpd


main=paste("Performance cumulated from 9 splits, λ=",as.character(devi))


data=cbind(cumsum(outtarget),cumsum(olpd_1),cumsum(olpd_1_eth),cumsum(olpd_2),cumsum(olpd_2_eth),cumsum(olpd_3),cumsum(olpd_3_eth))

#save(data, file = paste("data/xai/7_7_withsignal_xai_in/performance_with_eth.rda",sep=""))


colors= c("green","red","pink","violetred","darkorchid","blue","lightblue")
name=c(
      paste("Buy and Hold"," sharpe=",round(sharpe_bh,2)),
      paste("lpd+nn β=0.1"," sharpe=",round(sharpe_olpd_1,2)),
       paste("nn+lpd+eth-if-0 β=0.1"," sharpe=",round(sharpe_olpd_1_eth,2)),
       paste("lpd+nn β=0.2"," sharpe=",round(sharpe_olpd_2,2)),
       paste("nn+lpd+eth-if-0 β=0.2"," sharpe=",round(sharpe_olpd_2_eth,2)),
      paste("lpd+nn β=0.3"," sharpe=",round(sharpe_olpd_3,2)),
      paste("nn+lpd+eth-if-0 β=0.3"," sharpe=",round(sharpe_olpd_3_eth,2))
      )

       
plot.xts(data,col=colors,main=main)
addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")


# real performance with eth

getSymbols("BTC-USD") # loads the newest data from quandl
BTC_USD=na.omit(`BTC-USD`)

return_eth=diff(ETH$`ETH-USD.Close`)["2020-07-01::2021-03-27"]
return_btc=diff(BTC_USD$`BTC-USD.Close`)["2020-07-01::2021-03-27"]




nn_lpd=nn_signal_2
nn_lpd[which(olpd_signal_2==0)]<-0

original_ret_with_signal=return_btc*nn_lpd

dummy4=which(original_ret_with_signal==0)


original_ret_with_signal[dummy4,]=return_eth[dummy4,]



cbind(original_ret_with_signal,return_btc)

plot(cbind(cumsum(original_ret_with_signal),cumsum(return_btc)  ))





#performance cumulated lpdperf3
par(mfrow=c(3,1))

main=paste("Performance cumulated from 9 splits, λ=",as.character(devi))

compare_perf=cbind(cumsum(nn_1),cumsum(lpdperf1),cumsum(olpd_1),cumsum(perfall3_1),cumsum(nn_2),cumsum(lpdperf2),
                   cumsum(olpd_2),cumsum(perfall3_2),cumsum(nn_3),cumsum(lpdperf3),cumsum(olpd_3),cumsum(perfall3_3), cumsum(outtarget))

name=c("nn β=0.1","lpd β=0.1","lpd+nn β=0.1","nn+lpd+garch β=0.1","nn β=0.2","lpd β=0.2","lpd+nn β=0.2",
       "nn+lpd+garch β=0.2","nn β=0.3","lpd β=0.3","lpd+nn β=0.3","nn+lpd+garch β=0.3","Buy and Hold")

colnames(compare_perf)=name

# nn_lpd=nn_signal_2
# nn_lpd[which(olpd_signal_2==0)]<-0
# 
# save(nn_lpd, file = paste("data/xai/7_7_withsignal_xai_in/nn_lpd_without_eth.rda",sep=""))
# save(compare_perf, file = paste("data/xai/7_7_withsignal_xai_in/perfall_without_eth.rda",sep=""))

colors= c("red","pink","violetred","darkorchid","blue","lightblue","turquoise","dodgerblue4","darkorange","goldenrod1","yellow","darkgoldenrod1","green")


plot.xts(compare_perf,main=main,col=colors)
addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")


#sharpe ratios over all
# 
# sharpesave=rbind(sharpe_nn_1,sharpe_lpd_1,sharpe_olpd_1,sharpe_all3_1,
#       sharpe_nn_2,sharpe_lpd_2,sharpe_olpd_2,sharpe_all3_2,
#       sharpe_nn_3,sharpe_lpd_3,sharpe_olpd_3,sharpe_all3_3,
#       sharpe_bh)
# 
# save(sharpesave, file = paste("data/xai/7_7_withsignal_xai_in/sharpeplot_without_eth.rda",sep=""))

plot(rbind(sharpe_nn_1,sharpe_lpd_1,sharpe_olpd_1,sharpe_all3_1,
           sharpe_nn_2,sharpe_lpd_2,sharpe_olpd_2,sharpe_all3_2,
           sharpe_nn_3,sharpe_lpd_3,sharpe_olpd_3,sharpe_all3_3,
           sharpe_bh),main=paste("Sharpe, λ=",as.character(devi)),xaxt="n",ylab="sharpe",xlab=""
     ,col=colors,pch=19,cex=2)
axis(1, at=1:13, labels=name,)


#sharperatios per batch


# save(sharpmat_1, file = paste("data/xai/7_7_withsignal_xai_in/sharpmat_perbatch_without_eth.rda",sep=""))
# save(allsharp, file = paste("data/xai/7_7_withsignal_xai_in/allsharp_without_eth.rda",sep=""))


colorsbatch= c("red","pink","violetred","blue","lightblue","turquoise","darkorange","goldenrod1","yellow","green")


main=paste("Sharpe per batch, λ=",as.character(devi))
plot(sharpmat_1[,4],type="l",col="green",xlab="batch nr",ylab= "sharpe",lwd=4,ylim=c(min(allsharp),max(allsharp)),main=main)
for (i in 1:8){lines(allsharp[,i],col=colorsbatch[i],type="l",lwd=2)}



