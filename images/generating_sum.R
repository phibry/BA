# lpd plot
rm(list=ls())
# load libraries file
source("add/libraries.r") 
#load functions
source("add/Functions.r") 


load("data/xai/7_7_withsignal_xai_in/sum_explana_out_plot.rda")



lineup=rep(4.0,nrow(sum_explana["::2021-02-08"]))
linedown=rep(2.0,nrow(sum_explana["::2021-02-08"]))
lineup=reclass(lineup,sum_explana["::2021-02-08"])
linedown=reclass(linedown,sum_explana["::2021-02-08"])
sum_with_lines=na.exclude(cbind(as.xts(sum_explana),lineup,linedown))

plot(sum_explana["::2021-02-08"],type=c("b"),main="Sum of lags deviateing λ*sdY from Mean(Y)")

lines(lineup,lwd=2,col="red",lty=1)
lines(linedown,lwd=2,col="green",lty=1)



name=c("η upper","η lower")

addLegend("topright", 
          legend.names=name,
          col=c("red","green"),
          lty=c(1,1),
          lwd=c(2,2),
          ncol=1,
          bg="white")




