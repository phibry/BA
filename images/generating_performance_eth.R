rm(list=ls())


source("add/libraries.r") 
source("add/Functions.r") 




load("data/xai/7_7_withsignal_xai_in/performance_with_eth.rda")

load("data/xai/7_7_withsignal_xai_in/nn_lpd_without_eth.rda")


signal <- data$BTC.USD.Close
signal$BTC.USD.Close <- nn_lpd



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



load("data/xai/7_7_withsignal_xai_in/performance_with_eth.rda")
load("data/xai/7_7_withsignal_xai_in/nn_lpd_without_eth.rda")
signal <- data$BTC.USD.Close
signal$BTC.USD.Close <- nn_lpd
colors= c("green","red","pink","violetred","darkorchid","blue","lightblue")
plot(data, col=colors, main=TeX(sprintf("Performance cumulated from 9 splits, $\\lambda = %d$", 1)))
addLegend("topleft", 
          legend.names=c(TeX(sprintf("Buy and Hold, $Sharpe = %.2f$", 3.57)),
                         TeX(sprintf("LPD+NN $\\beta = %.1f$, $Sharpe = %.2f$", 0.1, 2.54)),
                         TeX(sprintf("LPD+NN+ETH if 0 $\\beta = %.1f$, $Sharpe = %.2f$", 0.1, 3.47)),
                         TeX(sprintf("LPD+NN $\\beta = %.1f$, $Sharpe = %.2f$", 0.2, 3.76)),
                         TeX(sprintf("LPD+NN+ETH if 0 $\\beta = %.1f$, $Sharpe = %.2f$", 0.2, 4.41)),
                         TeX(sprintf("LPD+NN $\\beta = %.1f$, $Sharpe = %.2f$", 0.3, 3.40)),
                         TeX(sprintf("LPD+NN+ETH if 0 $\\beta = %.1f$, $Sharpe = %.2f$", 0.3, 3.80))),
                         
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")

lines(signal, on=NA, lwd=2, col="red", ylim=c(-1.3, 1.3))






as.xts(data.df)


sum(is.na(data.df))
head(data.df)



