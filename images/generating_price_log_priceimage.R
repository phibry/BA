load("data/BTC_USD_27_03_21.rda") 

BTC_USD <- BTC_USD_27_03_21

cl_btc <- Cl(BTC_USD)
cl_btc_log <- log(cl_btc)


first <- min(cl_btc["2019-06-26::2020-09-01"]) # maximales / minamales value zwischen datum 
cl_btc[which(cl_btc==first)]               # index dazu finden

# events generieren für values
events <- xts(LETTERS[1:5], as.Date(c("2015-01-14","2017-12-16","2018-12-15","2019-06-26","2020-03-12")))

#plots
plot.xts(Cl(BTC_USD), col = "black", main="Bitcoin in US $",lwd = 0.8)
addEventLines(events,srt=90,pos=2,lty=3,col = c("darkgreen","red","darkgreen","red","darkgreen"),lwd=2,)


plot.xts(cl_btc_log,col = "black", main="Logarithmic Bitcoin in US $",lwd = 0.8)
addEventLines(events,srt=90,pos=2,lty=3,col = c("darkgreen","red","darkgreen","red","darkgreen"),lwd=2,)


