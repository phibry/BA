# lpd plot
rm(list=ls())
# load libraries file
source("add/libraries.r") 
#load functions
source("add/Functions.r") 

load("data/xai/7_7_withsignal_xai_in/OLPD_mat_out_plot.rda")
load("data/xai/7_7_withsignal_xai_in/OLPD_mat_in_mean.rda")
load("data/xai/7_7_withsignal_xai_in/OLPD_mat_in_sd.rda")



devi=2

plot.xts(OLPD_mat_out["::2021-02-08."],col=rainbow(ncol(OLPD_mat_out)),main="LPD Out of Sample")
for (i in 1:ncol(OLPD_mat_out))
  
  
  
meanline=rep(as.numeric(mean_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))
meanup=rep(as.numeric(mean_in[4]+devi*sd_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))
meandown=rep(as.numeric(mean_in[4]-devi*sd_in[4]),nrow(OLPD_mat_out["::2021-02-08"]))

meanline=reclass(meanline,OLPD_mat_out["::2021-02-08"])
meanup=reclass(meanup,OLPD_mat_out["::2021-02-08"])
meandown=reclass(meandown,OLPD_mat_out["::2021-02-08"])



lines(meanline,lwd=2,col="black")
lines(meanup,lwd=2,col="red",lty=2)
lines(meandown,lwd=2,col="red",lty=2)

name=c("arithmetic mean ffrom insample lag_j","standard deviation from insample lag_j * λ")
addLegend("bottomleft", 
          legend.names=name,
          col=c("black","red"),
          lty=c(1,2),
          lwd=c(1,1),
          ncol=1,
          bg="white")







