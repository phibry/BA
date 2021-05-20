source("add/libraries.r") 
load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21


subseti <- log_ret_27_03_21["2020-01-01::"]

# r graph gallery
# Create a df
df_sub <- data.frame(date = ymd(time(subseti)), value = as.numeric(subseti))

plot(df_sub
     ,type="l",lwd=1.5,
     frame.plot = TRUE,
     xaxt="n",
     xlab="Time:  Month-Year",
     ylab="Log Return BTC", ylim=c(-0.2, 0.2),main= "Train/ Test Split")
axis.Date(1, at=seq(min(df_sub$date), max(df_sub$date), by="months"), format="%m-%y")


# Transform timestamp to numeric for the plot
ybot <- par('usr')[3]
ytop <- par('usr')[4]

ydist <- abs(ybot - ytop)
ypartial <- ydist/9

for (i in 1:9) {
  rect_borders <- as.numeric(ymd(dates_mat[i,]))
  
  rect(xleft = rect_borders[1],
       xright = rect_borders[2]+1,
       ybottom = par('usr')[3]+(ypartial*(i-1)),
       ytop = par('usr')[3]+(ypartial*i),
       col="#0000FF1A")
  rect(xleft = rect_borders[3],
       xright = rect_borders[4]+1,
       ybottom = par('usr')[3]+(ypartial*(i-1)),
       ytop = par('usr')[3]+(ypartial*i),
       col="#80FF001A")
}

#col=c("black", "black"),

legend("topleft", inset=.02, legend=c("insample 6 months","Out of Sample 1 month"),
       col =c("#CCCCFF", "#CCFFCC"),  pch=c(15,15),cex=1,2, horiz=F,pt.cex=2)
