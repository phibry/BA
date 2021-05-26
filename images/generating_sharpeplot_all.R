



load("data/xai/7_7_withsignal_xai_in/sharpeplot_without_eth.rda")

name=c("nn β=0.1","lpd β=0.1","lpd+nn β=0.1","nn+lpd+garch β=0.1","nn β=0.2","lpd β=0.2","lpd+nn β=0.2",
       "nn+lpd+garch β=0.2","nn β=0.3","lpd β=0.3","lpd+nn β=0.3","nn+lpd+garch β=0.3","Buy and Hold")

colors= c("red","pink","violetred","darkorchid","blue","lightblue","turquoise","dodgerblue4","darkorange","goldenrod1","yellow","darkgoldenrod1","green")

plot(sharpesave,main="Sharpe, λ=1",xaxt="n",ylab="sharpe",xlab=""
     ,col=colors,pch=19,cex=2)

axis(1, at=1:13, labels=name,)
