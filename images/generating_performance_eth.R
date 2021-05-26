rm(list=ls())


source("add/libraries.r") 
source("add/Functions.r") 




load("data/xai/7_7_withsignal_xai_in/performance_with_eth.rda")

load("data/xai/7_7_withsignal_xai_in/nn_lpd_without_eth.rda")

data.df=as.data.frame(data)
data.df$signal=nn_lpd



main=paste("Performance cumulated from 9 splits, λ=","1")

colors= c("green","red","pink","violetred","darkorchid","blue","lightblue")

name=c(
  paste("Buy and Hold"," sharpe=3.57"),
  paste("lpd+nn β=0.1"," sharpe=2.54"),
  paste("nn+lpd+eth-if-0 β=0.1"," sharpe=3.47"),
  paste("lpd+nn β=0.2"," sharpe=3.76"),
  paste("nn+lpd+eth-if-0 β=0.2"," sharpe=4.41"),
  paste("lpd+nn β=0.3"," sharpe=3.4"),
  paste("nn+lpd+eth-if-0 β=0.3"," sharpe=3.8")
)


xts.plot(data,col=colors,main=main)



addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")


lines(as.xts(data.df$signal), on=NA, lwd=3, col="red" , ylim=c(-1.3, 1.3))








as.xts(data.df)


sum(is.na(data.df))
head(data.df)



