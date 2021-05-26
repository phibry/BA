source("add/libraries.r") 
source("add/Functions.r") 


load("data/xai/7_7_withsignal_xai_in/sharpeplot_with_eth.rda")



main="Performance cumulated from 9 splits, λ=1"


name=c("nn β=0.1","lpd β=0.1","lpd+nn β=0.1","nn+lpd+garch β=0.1","nn β=0.2","lpd β=0.2","lpd+nn β=0.2",
       "nn+lpd+garch β=0.2","nn β=0.3","lpd β=0.3","lpd+nn β=0.3","nn+lpd+garch β=0.3","Buy and Hold")

colnames(compare_perf)=name

colors= c("red","pink","violetred","darkorchid","blue","lightblue","turquoise","dodgerblue4","darkorange","goldenrod1","yellow","darkgoldenrod1","green")


plot.xts(compare_perf,main=main,col=colors)
addLegend("topleft", 
          legend.names=name,
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")
