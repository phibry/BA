
# generating density plots form dt an normaldist
library(MASS)
library(latex2exp)
load("data/log_ret_27_03_21.rda")  # loading logreturns closing! data xts
logret=log_ret_27_03_21 

my_data=as.numeric(logret[,1])   # as vector 
fit <- fitdistr(my_data, densfun="normal")  
fit2<- fitdistr(my_data, "t", start = list(m=mean(my_data),s=sd(my_data), df=1), lower=c(-1, 0.001,1)) #algo braucht startwerte
dt_ls <- function(x,mu,sigma,df){1/sigma * dt((x - mu)/sigma, df)}   # converting scale and location in only df

# plotting the data
hist(my_data, pch=20, breaks=70, prob=TRUE, main="Logreturns BTC",xlab="logrets")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=1.5, add=T)
curve(dt_ls(x,fit2$estimate[1], fit2$estimate[2],fit2$estimate[3]), col="blue", lwd=1.5, add=T)
curve(dnorm(x, fit$estimate[1], 0.025), col="orange", lwd=1.5, add=T)
legend("topleft", inset=.02, legend=c(TeX(sprintf("$N(\\mu = 0.00201$,$ \\sigma = 0.01956)$")), TeX(sprintf("$t(l = 0.00245$,$ s = 2.0341$,$ df = 2.0341)$")), TeX(sprintf("$N(\\mu = 0.00201$,$ \\sigma = 0.025)$"))),
       col=c("red", "blue", "orange"), lty = c(1,1,1), cex=1, horiz=F, bty = "n")

