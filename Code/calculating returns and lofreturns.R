load("data/xai/7_7_withsignal_xai_in/performance_with_eth.rda")
load("data/xai/7_7_withsignal_xai_in/nn_lpd_without_eth.rda")

#datenpunkt vom 30.6.21

9137.993* prod(exp(logret))

9137.993*prod(diff(data[,5]))


#wert vom 1.7.21
1000*prod(exp(logret))

1000* prod( exp(  diff(  data[,5] )[-1,]) )

1000*exp(2.453907267)

*cumsum(logret)



logret=diff(log(Cl(btc))) ["2020-07-01::"]

Cl(btc)["2020-06-30::"]

sum(logret)/ nrow(logret)

020-01-01  0.0009136166
7200.174 * 2.0315514243 

Cl(btc)["2020-01-01::"]



bh=diff(data[,1])

exp(bh[-1,])


load("C:/Users/buehl/Desktop/PA_BA/BA/data/BTC_USD_27_03_21.rda")
load("C:/Users/buehl/Desktop/PA_BA/BA/data/log_ret_27_03_21.rda")


# tryin a sma on logrets ####

#sma
btc=`BTC_USD_27_03_21`

plot(Cl(btc)["2020-07-01::"])
lines(cumsum(exp(logret[-1,]))

plot(cumsum(exp(logret[-1,])))

9228.325

head(Cl(btc)["2020-07-01::"])

logret=diff(log(Cl(btc))) ["2020-01-01::"]
            

cumsum(logret)

x=logret

lines(data[,5]+9228.325)


lines(exp(data[,1])+9228.325)


exp(data[,1])








signal <- data$BTC.USD.Close
signal$BTC.USD.Close <- nn_lpd
colors= c("green","darkorchid")
plot(data[,c(1,5)], col=colors, main=TeX(sprintf("Performance cumulated from 9 splits, $\\lambda = %d$", 1)))

addLegend("left",
          legend.names=c(TeX(sprintf("Buy and Hold, $Sharpe = %.2f$", 3.57)),
                         TeX(sprintf("LPD+NN+ETH if 0 $\\beta = %.1f$, $Sharpe = %.2f$", 0.2, 4.41))),
          col=colors,
          lty=c(rep(1,13),2),
          lwd=c(rep(2,13),3),
          ncol=1,
          bg="white")

events<- xts(LETTERS[1:6], as.Date(c("2020-08-01","2020-09-04","2020-10-18","2021-01-15","2021-02-22","2021-03-12")))
addEventLines(events, srt=0, pos=1, lty=3, col = rep("#004c6d",6),lwd=2, cex=1.2)
# lines(signal, on=NA, lwd=2, col="red", ylim=c(-1.3, 1.3))






#logreturns beschreiben


#pt = 130
#pt-1 = 110
#pt-2 = 120
#pt-3 = 100
#reale daten
x=c(100,120,110,130)


log(130/110)    #logret von t

log(110/120)    #logret von t-1

log(120/100)    #logret von t-2

c(0.1823216,-0.08701138,0.1670541) #log returns t-2 ... t




130/110   #original return  t

110/120  #original return  t-1

120/100 #original return  t-2

c(1.2,0.91666667, 1.181818) #original returns

# 100 franken investieren * alle returns rechnen 

100     *1.2*0.91666667* 1.181818




#probieren mit code

#reale daten
x=c(100,120,110,130)


#logret in returns umwandeln  und mit anfangsvalue rechnen

100* prod(exp(logret))



logx=log(x)

logret=diff(logx)



plot(logx)
lines(cumsum(logret))


sum((logret))





130/120














