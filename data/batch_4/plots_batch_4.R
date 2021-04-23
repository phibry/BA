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
load("data/log_ret_27_03_21.rda")
subseti <- log_ret_27_03_21["2020-01-01::"]

chart.ACF(log_ret_27_03_21, maxlag = 15)
chart.ACF(subseti, maxlag = 15)
par()


# 1
chart.ACF(log_ret_27_03_21["2020-01-01::2020-07-31"], maxlag = 15, main=1)
chart.ACF(log_ret_27_03_21["2020-02-01::2020-08-31"], maxlag = 15, main=2)
chart.ACF(log_ret_27_03_21["2020-03-01::2020-09-30"], maxlag = 15, main=3)
chart.ACF(log_ret_27_03_21["2020-04-01::2020-10-31"], maxlag = 15, main=4)
chart.ACF(log_ret_27_03_21["2020-05-01::2020-11-30"], maxlag = 15, main=5)
chart.ACF(log_ret_27_03_21["2020-06-01::2020-12-31"], maxlag = 15, main=6)
chart.ACF(log_ret_27_03_21["2020-07-01::2021-01-31"], maxlag = 15, main=7)
chart.ACF(log_ret_27_03_21["2020-08-01::2021-02-28"], maxlag = 15, main=8)
chart.ACF(log_ret_27_03_21["2020-09-01::2021-03-27"], maxlag = 15, main=9)

# r graph gallery
# Create a df
df_sub <- data.frame(date = ymd(time(subseti)), value = as.numeric(subseti))
head(df_sub)

as.numeric(df_sub[1,1])
plot(df_sub,
     type="l",
     xaxt="n",
     xlab="",
     ylab="Log Return", ylim=c(-0.2, 0.2),
     main="Rolling in/out splits")

par('xaxt')


axis.Date(1, at=seq(min(df_sub$date), max(df_sub$date), by="months"), format="%m-%y")
legend("topleft", legend=c("In-Sample", "Out-of-Sample"), pch=c(15,15), col=c("#0000FF","#80FF00"), cex=0.7, bty = "n")


# Transform timestamp to numeric for the plot
ybot <- par('usr')[3]
ytop <- par('usr')[4]
par('usr')

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




diag(cor((nn_10_3_50_1[,in_samp_seq]), (nn_10_3_50_1[,out_samp_seq])))

head(nn_10_3_50_1)

in_samp_seq <- seq(1, 50*4, 4)
out_samp_seq <- seq(2, 50*4, 4)
nn_10_3_50_1[1,in_samp_seq]
nn_10_3_50_1[1,out_samp_seq]

cor((nn_10_3_50_1[,in_samp_seq]), (nn_10_3_50_1[,out_samp_seq]))
?cor

plot(as.numeric(nn_10_3_50_1[1,in_samp_seq]),as.numeric(nn_10_3_50_1[1,out_samp_seq]))


#.####
# Load Files####
load("data/batch_4/nn_10_3_50_1.rda")
load("data/batch_4/nn_10_3_50_2.rda")
load("data/batch_4/nn_10_3_50_3.rda")
load("data/batch_4/nn_10_3_50_4.rda")
load("data/batch_4/nn_10_3_50_5.rda")
load("data/batch_4/nn_10_3_50_6.rda")
load("data/batch_4/nn_10_3_50_7.rda")
load("data/batch_4/nn_10_3_50_8.rda")
load("data/batch_4/nn_10_3_50_9.rda")

# Merge Files####
mse_in_mean_1 <- apply(X=nn_10_3_50_1[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_2 <- apply(X=nn_10_3_50_2[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_3 <- apply(X=nn_10_3_50_3[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_4 <- apply(X=nn_10_3_50_4[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_5 <- apply(X=nn_10_3_50_5[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_6 <- apply(X=nn_10_3_50_6[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_7 <- apply(X=nn_10_3_50_7[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_8 <- apply(X=nn_10_3_50_8[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
mse_in_mean_9 <- apply(X=nn_10_3_50_9[, seq(1, 200, 4)], MARGIN=1, FUN=mean)


mse_in <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_2[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_3[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_4[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_5[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_6[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_7[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_8[, seq(1, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_9[, seq(1, 200, 4)], MARGIN=1, FUN=mean)
));colnames(mse_in) <- 1:9

mse_out <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_2[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_3[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_4[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_5[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_6[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_7[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_8[, seq(2, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_9[, seq(2, 200, 4)], MARGIN=1, FUN=mean)
));colnames(mse_out) <- 1:9

sharpe_in <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_2[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_3[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_4[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_5[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_6[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_7[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_8[, seq(3, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_9[, seq(3, 200, 4)], MARGIN=1, FUN=mean)
));colnames(sharpe_in) <- 1:9

sharpe_out <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_2[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_3[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_4[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_5[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_6[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_7[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_8[, seq(4, 200, 4)], MARGIN=1, FUN=mean),
  apply(X=nn_10_3_50_9[, seq(4, 200, 4)], MARGIN=1, FUN=mean)
));colnames(sharpe_out) <- 1:9

# save(mse_in, mse_out, sharpe_in, sharpe_out, file = "data/batch_4/meanerino.rda")

load("data/batch_4/meanerino.rda")


min(mse_in[1,])


layers <- sapply(X=rownames(nn_10_3_50_9), FUN=str_splitter, USE.NAMES=FALSE)
layers <- as.numeric(table(layers))
layers <- cumsum(layers)

which(mse_in[1,] == min(mse_in[1,]))
which(mse_in[2,] == min(mse_in[2,]))

par(mfrow=c(1,1))

plot_mse_mean <- function(mse_in, mse_out, title="",scale_fac=3) {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mse_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  split_colors <- c("#59C7EB",
                    "#E0607E",
                    "#0A9086",
                    "#FEA090",
                    "#3E5496",
                    "#EFDC60",
                    "#8E2043",
                    "#9AA0A7",
                    "#AC9A8C")
  # MSE in
  for(i in 1:dim(mse_in)[2]) {
    if (i == 1) {
      plot(mse_in[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mse_in) ,max(mse_in)),
           xlim=c(1, dim(mse_in)[1]),
           col=split_colors[i],
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(mse_in[,i], col=split_colors[i])
    }
  }

  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mse_in),
         ytop = max(mse_in),
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # MSE out
  for(i in 1:dim(mse_out)[2]) {
    if (i == 1) {
      plot(mse_out[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mse_out) ,min(mse_out)*scale_fac),
           xlim=c(1, dim(mse_out)[1]),
           col=split_colors[i],
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(mse_out[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_out),
         # ytop = max(mse_out),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}
plot_mse_mean(mse_in=mse_in, mse_out=mse_out, title="Mean MSE over all 9 splits", 20)



plot_sharpe_mean <- function(sharpe_in, sharpe_out, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(sharpe_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  split_colors <- c("#59C7EB",
                    "#E0607E",
                    "#0A9086",
                    "#FEA090",
                    "#3E5496",
                    "#EFDC60",
                    "#8E2043",
                    "#9AA0A7",
                    "#AC9A8C")
  # Sharpe in
  for(i in 1:dim(sharpe_in)[2]) {
    if (i == 1) {
      plot(sharpe_in[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(sharpe_in) ,max(sharpe_in)),
           xlim=c(1, dim(sharpe_in)[1]),
           col=split_colors[i],
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(sharpe_in[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(sharpe_in),
         ytop = max(sharpe_in),
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.1)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  # Sharpe out
  for(i in 1:dim(sharpe_out)[2]) {
    if (i == 1) {
      plot(sharpe_out[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(sharpe_out) ,max(sharpe_out)),
           xlim=c(1, dim(sharpe_out)[1]),
           col=split_colors[i],
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
    } else {
      lines(sharpe_out[,i], col=split_colors[i])
    }
  }
  
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         # ybottom = min(mse_out),
         # ytop = max(mse_out),
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[i])
    ydistance <- par('usr')[4] - par('usr')[3]
    textlocation <- par('usr')[3] + (ydistance * 0.9)
    text(startl[i]+(endl[i]-startl[i])/2, textlocation , i)
  }
  
  par(par_default)
}
plot_sharpe_mean(sharpe_in=sharpe_in, sharpe_out=sharpe_out, title="Mean Sharpe over all 9 splits")



#.####
# Testerino####
# (1)-NN-Architecture
# In-Sample MSE
nn_10_3_50_1[1,seq(1, 200, 4)]
# Out-of-Sample MSE
nn_10_3_50_1[1,seq(2, 200, 4)]
# In-Sample Sharpe
nn_10_3_50_1[1,seq(3, 200, 4)]
# Out-of-Sample Sharpe
nn_10_3_50_1[1,seq(4, 200, 4)]



# MSE####
# 1#### 
# width=1400
# height=900
mse_1 <- nn_10_3_50_1[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
plot_all_rect(mati=mse_1, real=50, title="MSE Split: 1")
plot_by_layer_rect(mati=mse_1, real=50, title="MSE Split: 1")

# 2####
mse_2 <- nn_10_3_50_2[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_2, real=50, title="In/Out Split: 2")
plot_all_rect_scale(mati=mse_2, real=50, title="MSE Split: 2")
# plot_by_layer_rect(mati=mse_2, real=50, title="In/Out Split: 2")
plot_by_layer_rect_scale(mati=mse_2, real=50, title="MSE Split: 2",
                         scale_vec=c(2, 2, 4))

# 3####
mse_3 <- nn_10_3_50_3[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_3, real=50, title="In/Out Split: 3")
plot_all_rect_scale(mati=mse_3, real=50, title="MSE Split: 3")
# plot_by_layer_rect(mati=mse_3, real=50, title="In/Out Split: 3")
plot_by_layer_rect_scale(mati=mse_3, real=50, title="MSE Split: 3",
                         scale_vec=c(5, 8, 2))
# 4####
mse_4 <- nn_10_3_50_4[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_4, real=50, title="In/Out Split: 4")
plot_all_rect_scale(mati=mse_4, real=50, title="MSE Split: 4", scale_fac=9)
# plot_by_layer_rect(mati=mse_4, real=50, title="In/Out Split: 4")
plot_by_layer_rect_scale(mati=mse_4, real=50, title="MSE Split: 4",
                   scale_vec=c(4, 8, 10))

# 5####
mse_5 <- nn_10_3_50_5[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_5, real=50, title="In/Out Split: 5")
plot_all_rect_scale(mati=mse_5, real=50, title="MSE Split: 5", scale_fac=11)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")

# plot_by_layer_rect(mati=mse_5, real=50, title="In/Out Split: 5")
plot_by_layer_rect_scale(mati=mse_5, real=50, title="MSE Split: 5",
                   scale_vec=c(4, 10, 10))

# 6####
mse_6 <- nn_10_3_50_6[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_6, real=50, title="In/Out Split: 6")
plot_all_rect_scale(mati=mse_6, real=50, title="MSE Split: 6", scale_fac=30)
# plot_by_layer_rect(mati=mse_6, real=50, title="In/Out Split: 6")
plot_by_layer_rect_scale(mati=mse_6, real=50, title="MSE Split: 6",
                         scale_vec=c(10, 20, 10))

# 7####
mse_7 <- nn_10_3_50_7[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_7, real=50, title="In/Out Split: 7")
plot_all_rect_scale(mati=mse_7, real=50, title="MSE Split: 7", scale_fac=30)
# plot_by_layer_rect(mati=mse_7, real=50, title="In/Out Split: 7")
plot_by_layer_rect_scale(mati=mse_7, real=50, title="MSE Split: 7",
                         scale_vec=c(30, 15, 5))

# 8####
mse_8 <- nn_10_3_50_8[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_8, real=50, title="In/Out Split: 8")
plot_all_rect_scale(mati=mse_8, real=50, title="MSE Split: 8", scale_fac=10)
# plot_by_layer_rect(mati=mse_8, real=50, title="In/Out Split: 8")
plot_by_layer_rect_scale(mati=mse_8, real=50, title="MSE Split: 8",
                         scale_vec=c(30, 15, 7))

# 9####
mse_9 <- nn_10_3_50_9[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_9, real=50, title="In/Out Split: 9")
plot_all_rect_scale(mati=mse_9, real=50, title="MSE Split: 9", scale_fac=10)
# plot_by_layer_rect(mati=mse_9, real=50, title="In/Out Split: 9")
plot_by_layer_rect_scale(mati=mse_9, real=50, title="MSE Split: 9",
                   scale_vec=c(10, 20, 8))





# Mean Plots####
split_colors <- c("#59C7EB", "#E0607E", "#0A9086", "#FEA090", "#3E5496", "#EFDC60", "#8E2043", "#9AA0A7", "#AC9A8C")

load("data/batch_4/meanerino.rda")
plot_mse_mean(mse_in=mse_in, mse_out=mse_out, title="Mean MSE over all 9 splits", 20)
legend("left", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")


plot_sharpe_mean(sharpe_in=sharpe_in, sharpe_out=sharpe_out, title="Mean Sharpe over all 9 splits")
legend("left", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")




colorcodes <- c("#FF00001A", # red
                "#0000FF1A", # blue
                "#80FF001A", # green
                "#FF80001A", # orange
                "#00FFFF1A", # teal
                "#8000FF1A") # purple
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=3, cex=1, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=3,bty='n',
       col = 1:9, horiz=TRUE, title="Train/Test Splits")
?legend


# library('unikn') 
# usecol(pal_unikn_pref, n=10)
# seecol(pal_unikn_pref, n=9)
# seecol(pal_unikn_pref, n=10)
split_colors <- c("#59C7EB", "#E0607E", "#0A9086", "#FEA090", "#3E5496", "#EFDC60", "#8E2043", "#9AA0A7", "#AC9A8C")
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=3,bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")


?pch
?legend
