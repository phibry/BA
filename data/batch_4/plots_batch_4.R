source("add/libraries.r") 
source("add/Functions.r")
# Batch 4####

# Rolling in/out split Plot All####
# DataFrame for the rectangle subset plots
start_in <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")
end_in <- c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28")
start_out <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01")
end_out <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-27")

dates_mat <- as.data.frame(cbind(start_in, end_in, start_out, end_out))

# Plot Price
subseti <- log_ret_27_03_21["2020-01-01::"]

# r graph gallery
# Create a df
df_sub <- data.frame(date = ymd(time(subseti)), value = as.numeric(subseti))

plot(df_sub,
     type="l",
     xaxt="n",
     xlab="",
     ylab="Log Return", ylim=c(-0.2, 0.2),
     main="Rolling in/out splits")
axis.Date(1, at=seq(min(df_sub$date), max(df_sub$date), by="months"), format="%m-%y")
legend("topleft", legend=c("In-Sample", "Out-of-Sample"), pch=c(15,15), col=c("#0000FF","#80FF00"), cex=0.7, bty = "n")


# Transform timestamp to numeric for the plot
ybot <- par('usr')[3]
ytop <- par('usr')[4]

ydist <- abs(ybot - ytop)
ypartial <- ydist/9

for (i in 1:9) {
  rect_borders <- as.numeric(ymd(dates_mat[i,]))
  
  rect(xleft = rect_borders[1],
       xright = rect_borders[2],
       ybottom = par('usr')[3]+(ypartial*(i-1)),
       ytop = par('usr')[3]+(ypartial*i),
       col="#0000FF1A")
  rect(xleft = rect_borders[3],
       xright = rect_borders[4],
       ybottom = par('usr')[3]+(ypartial*(i-1)),
       ytop = par('usr')[3]+(ypartial*i),
       col="#80FF001A")
}




# Rolling in/out split Plot Single####
# DataFrame for the rectangle subset plots
start_in <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")
end_in <- c("2020-06-30", "2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28")
start_out <- c("2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01")
end_out <- c("2020-07-31", "2020-08-31", "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31", "2021-01-31", "2021-02-28", "2021-03-27")

dates_mat <- as.data.frame(cbind(start_in, end_in, start_out, end_out))

# Plot Price
subseti <- log_ret_27_03_21["2020-01-01::"]

# r graph gallery
# Create a df
df_sub <- data.frame(date = ymd(time(subseti)), value = as.numeric(subseti))

# Transform timestamp to numeric for the plot
for (i in 1:9) {
  plot(df_sub,
       type="l",
       xaxt="n",
       xlab="",
       ylab="Log Return", ylim=c(-0.2, 0.2),
       # main="Rolling in/out splits"
       main=paste("Rolling In/Out-Split: ", i))
  axis.Date(1, at=seq(min(df_sub$date), max(df_sub$date), by="months"), format="%m-%y")
  legend("top", legend=c("In-Sample", "Out-of-Sample"), pch=c(15,15), col=c("#0000FF","#80FF00"), cex=0.7, bty = "n")
  
  rect_borders <- as.numeric(ymd(dates_mat[i,]))
  
  rect(xleft = rect_borders[1],
       xright = rect_borders[2],
       ybottom = par('usr')[3],
       ytop = par('usr')[4],
       col="#0000FF1A")
  rect(xleft = rect_borders[3],
       xright = rect_borders[4],
       ybottom = par('usr')[3],
       ytop = par('usr')[4],
       col="#80FF001A")
}
