rm(list=ls())

#loading area ####
source("add/libraries.r") 

load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")
outtarget=log_ret_27_03_21["2020-07-01::"]

load("data/GARCH_vola_predictions/garch_out_signal.rda")


#-------------------------------------------------------------------------------

#how many standart deviatons for olpd threshold
devi=2
#
# decision rule of nn percentage of half  if NULL majority decision is taken
percentage= 0.3
anz=1000
#---------
#-------------------------------------------------------------------------------
data_string= paste("obj","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
load(paste("data/xai/7_7_withsignal_xai_in/signal1_and_05/9",data_string,".rda",sep=""))

olpd_string=  paste("alloverperf_olpd","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
nn_string=  paste("alloverperf_nn","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
sharpmat_string=  paste("sharpmat","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
olpd_signal_string=  paste("olpd_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")
nn_signal_string=  paste("nn_signal","anz=",as.character(anz),"decision=",as.character(percentage*100),"%","dev=",as.character(devi),sep="_")



#------------------------------------------------------------------------------

#
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


  

  
  
  




#sharperatio over all
sharpe_bh=sqrt(365)*SharpeRatio(outtarget,FUN="StdDev")



sharpe_olpd_1=sqrt(365)*SharpeRatio(olpd_1,FUN="StdDev")
sharpe_nn_1=sqrt(365)*SharpeRatio(nn_1,FUN="StdDev")
sharpe_lpd_1=sqrt(365)*SharpeRatio(lpdperf1,FUN="StdDev")
sharpe_all3_1=sqrt(365)*SharpeRatio(perfall3_1,FUN="StdDev")



sharpe_olpd_2=sqrt(365)*SharpeRatio(olpd_2,FUN="StdDev")
sharpe_nn_2=sqrt(365)*SharpeRatio(nn_2,FUN="StdDev")
sharpe_lpd_2=sqrt(365)*SharpeRatio(lpdperf2,FUN="StdDev")
sharpe_all3_2=sqrt(365)*SharpeRatio(perfall3_2,FUN="StdDev")


sharpe_olpd_3=sqrt(365)*SharpeRatio(olpd_3,FUN="StdDev")
sharpe_nn_3=sqrt(365)*SharpeRatio(nn_3,FUN="StdDev")
sharpe_lpd_3=sqrt(365)*SharpeRatio(lpdperf3,FUN="StdDev")
sharpe_all3_3=sqrt(365)*SharpeRatio(perfall3_3,FUN="StdDev")





#all sharpmats togheter

sharpmat_1[,1:3]=sharpmat_1[,c(2,3,1)]
colnames(sharpmat_1)=c("sharpe_net","sharpe_lpd","sharpe_nn_olpd","sharpe_bh")

sharpmat_2[,1:3]=sharpmat_2[,c(2,3,1)]
colnames(sharpmat_2)=c("sharpe_net","sharpe_lpd","sharpe_nn_olpd","sharpe_bh")

sharpmat_3[,1:3]=sharpmat_3[,c(2,3,1)]
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

save(list=allsharpestring, file = paste("data/xai/7_7_withsignal_xai_in/signal1_and_05/",allsharpestring,".rda",sep=""))
save(list=batchsharpestring, file = paste("data/xai/7_7_withsignal_xai_in/signal1_and_05/",batchsharpestring,".rda",sep=""))



#plots####
#-----------------------------------------------------------------------------

#performance cumulated lpdperf3
par(mfrow=c(3,1))

main=paste("Performance cumulated from 9 splits, λ=",as.character(devi))

compare_perf=cbind(cumsum(nn_1),cumsum(lpdperf1),cumsum(olpd_1),cumsum(perfall3_1),cumsum(nn_2),cumsum(lpdperf2),
                   cumsum(olpd_2),cumsum(perfall3_2),cumsum(nn_3),cumsum(lpdperf3),cumsum(olpd_3),cumsum(perfall3_3), cumsum(outtarget))

name=c("nn β=0.1","lpd β=0.1","lpd+nn β=0.1","nn+lpd+garch β=0.1","nn β=0.2","lpd β=0.2","lpd+nn β=0.2",
       "nn+lpd+garch β=0.2","nn β=0.3","lpd β=0.3","lpd+nn β=0.3","nn+lpd+garch β=0.3","Buy and Hold")

colnames(compare_perf)=name

colors= c("red","pink","violetred","darkorchid","blue","lightblue","turquoise","dodgerblue4","darkorange","goldenrod1","yellow","darkgoldenrod1","green")


 #plot.xts(compare_perf,main=main,col=colors)
addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")


#sharpe ratios over all

plot(rbind(sharpe_nn_1,sharpe_lpd_1,sharpe_olpd_1,sharpe_all3_1,
           sharpe_nn_2,sharpe_lpd_2,sharpe_olpd_2,sharpe_all3_2,
           sharpe_nn_3,sharpe_lpd_3,sharpe_olpd_3,sharpe_all3_3,
           sharpe_bh),main=paste("Sharpe, λ=",as.character(devi)),xaxt="n",ylab="sharpe",xlab=""
     ,col=colors,pch=19,cex=2)
axis(1, at=1:13, labels=name,)


#sharperatios per batch

colorsbatch= c("red","pink","violetred","blue","lightblue","turquoise","darkorange","goldenrod1","yellow","green")


main=paste("Sharpe per batch, λ=",as.character(devi))
plot(sharpmat_1[,4],type="l",col="green",xlab="batch nr",ylab= "sharpe",lwd=4,ylim=c(min(allsharp),max(allsharp)),main=main)
for (i in 1:8){lines(allsharp[,i],col=colorsbatch[i],type="l",lwd=2)}



