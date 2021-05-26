load("data/xai/7_7_withsignal_xai_in/sharpmat_perbatch_without_eth.rda")
load("data/xai/7_7_withsignal_xai_in/allsharp_without_eth.rda")



colorsbatch= c("red","pink","violetred","blue","lightblue","turquoise","darkorange","goldenrod1","yellow","green")


main="Sharpe per batch, λ=1"



plot(sharpmat_1[,4],type="l",col="green",xlab="batch nr",ylab= "sharpe",lwd=4,ylim=c(min(allsharp),max(allsharp)),main=main)
for (i in 1:8){lines(allsharp[,i],col=colorsbatch[i],type="l",lwd=2)}

