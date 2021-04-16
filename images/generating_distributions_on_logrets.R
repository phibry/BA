library(MASS)

load("data/log_ret_27_03_21.rda")  # loading logreturns closing! data xts
logret=log_ret_27_03_21 

my_data=as.numeric(logret[,1])   # as vector 
fit <- fitdistr(my_data, densfun="normal")  
fit2<- fitdistr(my_data, "t", start = list(m=mean(my_data),s=sd(my_data), df=1), lower=c(-1, 0.001,1)) #algo braucht startwerte

dt_ls <-function(x,mu,sigma,df){1/sigma * dt((x - mu)/sigma, df)}# converting scale and location in only df

beschrnorm= paste("~ N(","mu=",as.character(round(fit$estimate[1],5)),
                  "sd=",as.character(round(fit$estimate[2],5)),")")
beschrt= paste("~ t(","l=",as.character(round(fit2$estimate[1],5)),"s=",
               as.character(round(fit2$estimate[2],5)),"df=",as.character(round(fit2$estimate[3],5)),")")

beschrnorm2= paste("~ N(","mu=",as.character(round(fit$estimate[1],5)),
                   "sd=0.025)")

library(car)
qqPlot(logret)


# plotting the data
hist(my_data, pch=20, breaks=70, prob=TRUE, main="Logreturns BTC",xlab="logrets")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)
curve(dt_ls(x,fit2$estimate[1],fit2$estimate[2],fit2$estimate[3]), col="blue", lwd=2, add=T)
curve(dnorm(x, 0.002012225 ,0.025 ), col="darkorange", lwd=2, add=T)


legend("topleft", inset=.02, legend=c(beschrnorm,beschrt,beschrnorm2),bty="n",
       col=c("red","blue","orange" ), pch = c("-","-","-"),pt.cex = 2 ,cex=1, horiz=F)






beschrnorm= paste("Normal distribution ~ N(","mu=",as.character(round(fit$estimate[1],5)),
                  "sd=",as.character(round(fit$estimate[2],5)),")")
beschrt= paste("T - distribution ~ t(","l=",as.character(round(fit2$estimate[1],5)),"s=",
               as.character(round(fit2$estimate[2],5)),"df=",as.character(round(fit2$estimate[3],5)),")")

beschrnorm2= paste("Normal distribution ~ N(","mu=",as.character(round(fit$estimate[1],5)),
                   "sd=0.025)")

