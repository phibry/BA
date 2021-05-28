load("data/log_ret_27_03_21.rda")
load("data/dates_mat.rda")

logret <- log_ret_27_03_21


subseti <- logret["2020-01-01::"]

# r graph gallery
# Create a df
df_sub <- data.frame(date = ymd(time(subseti)), value = as.numeric(subseti))

plot(df_sub,
     type="l",
     lwd=1.5,
     frame.plot = FALSE,
     xaxt="n",
     xlab="Time:  Month-Year",
     ylab="Log Return BTC",
     ylim=c(-0.2, 0.2),
     main= "Train/ Test Split")
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

text1 <- as.numeric(ymd(dates_mat[1,]))
posx <- (text1[4] + text1[1])/2
posy <- par('usr')[3] + ypartial/2
text(x=posx, y=posy, labels="1", cex = 1.1)
text(x=text1[4]+55, y=posy, labels="t+1 month", cex = 0.8)
arrows(x0=text1[3]+15, y0=posy, x1=text1[3]+50, y1=posy, length = 0.12, angle = 20)

text1 <- as.numeric(ymd(dates_mat[2,]))
posx <- (text1[4] + text1[1])/2
posy <- par('usr')[3] + ypartial/2 + ypartial
text(x=posx, y=posy, labels="2", cex = 1.1)
text(x=text1[4]+55, y=posy, labels="t+1 month", cex = 0.8)
arrows(x0=text1[3]+15, y0=posy, x1=text1[3]+50, y1=posy, length = 0.12, angle = 20)

text1 <- as.numeric(ymd(dates_mat[3,]))
posx <- (text1[4] + text1[1])/2
posy <- par('usr')[3] + ypartial/2 + ypartial*2
text(x=posx, y=posy, labels="...", cex = 1.1)

text1 <- as.numeric(ymd(dates_mat[8,]))
posx <- (text1[4] + text1[1])/2
posy <- par('usr')[3] + ypartial/2 + ypartial*7
text(x=posx, y=posy, labels="...", cex = 1.1)

text1 <- as.numeric(ymd(dates_mat[9,]))
posx <- (text1[4] + text1[1])/2
posy <- par('usr')[3] + ypartial/2 + ypartial*8
text(x=posx, y=posy, labels="9", cex = 1.1)


legend("topleft", inset=.02, legend=c("In-sample 6 months","Out-of-sample 1 month"),
       col =c("#CCCCFF", "#CCFFCC"),  pch=c(15,15),cex=1,2, horiz=F,pt.cex=2,
       bty = "n")
