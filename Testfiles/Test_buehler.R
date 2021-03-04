# testfile Buehler
# this file is only for testing purpose, does not appear in final paper

# acessing data via coinmarket cap https://coinmarketcap.com/
# from yahoofinance ticker BTC-USD
# *Close price adjusted for splits.
#**Adjusted close price adjusted for both dividends and splits.


getSymbols("BTC-USD")


BTC_USD=na.exclude(BTC_USD)





#

head(BTC_USD,n=5)
tail(BTC_USD,n=3)      #  

x_level = Cl(BTC_USD)
plot(log(Cl(BTC_USD)))# 
log_ret   = diff(log(Cl(BTC_USD))) # 


plot(log_ret)
# Line Chart
chartSeries(BTC_USD,type="line", theme=chartTheme("white"))

# Bar Chart
chartSeries(BTC_USD, type="bar",theme=chartTheme("white"))

# Candle Stick Chart
chartSeries(BTC_USD, type="auto", theme=chartTheme("white"))

# 



# log returns btc








