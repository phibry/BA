source("add/libraries.r") 
source("add/Functions.r")
# Load Files####
load("data/batch_5/nn_10_3_50_1.rda")
load("data/batch_5/nn_10_3_50_2.rda")
load("data/batch_5/nn_10_3_50_3.rda")
load("data/batch_5/nn_10_3_50_4.rda")
load("data/batch_5/nn_10_3_50_5.rda")
load("data/batch_5/nn_10_3_50_6.rda")
load("data/batch_5/nn_10_3_50_7.rda")
load("data/batch_5/nn_10_3_50_8.rda")
load("data/batch_5/nn_10_3_50_9.rda")


# ACF####
# Plot Price
load("data/log_ret_27_03_21.rda")
subseti <- log_ret_27_03_21["2020-01-01::"]

chart.ACF(log_ret_27_03_21, maxlag = 15, main="Max Length")
chart.ACF(log_ret_27_03_21["2020-01-01::2021-03-27"], maxlag = 15, main="Subset")

chart.ACF(log_ret_27_03_21["2020-01-01::2020-07-31"], maxlag = 15, main="Subset 1")
chart.ACF(log_ret_27_03_21["2020-02-01::2020-08-31"], maxlag = 15, main="Subset 2")
chart.ACF(log_ret_27_03_21["2020-03-01::2020-09-30"], maxlag = 15, main="Subset 3")
chart.ACF(log_ret_27_03_21["2020-04-01::2020-10-31"], maxlag = 15, main="Subset 4")
chart.ACF(log_ret_27_03_21["2020-05-01::2020-11-30"], maxlag = 15, main="Subset 5")
chart.ACF(log_ret_27_03_21["2020-06-01::2020-12-31"], maxlag = 15, main="Subset 6")
chart.ACF(log_ret_27_03_21["2020-07-01::2021-01-31"], maxlag = 15, main="Subset 7")
chart.ACF(log_ret_27_03_21["2020-08-01::2021-02-28"], maxlag = 15, main="Subset 8")
chart.ACF(log_ret_27_03_21["2020-09-01::2021-03-27"], maxlag = 15, main="Subset 9")


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

save(mse_in, mse_out, sharpe_in, sharpe_out, file = "data/batch_5/meanerino.rda")

load("data/batch_5/meanerino.rda")



# Merge Files Robust####
mse_in_mean_1 <- apply(X=nn_10_3_50_1[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_2 <- apply(X=nn_10_3_50_2[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_3 <- apply(X=nn_10_3_50_3[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_4 <- apply(X=nn_10_3_50_4[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_5 <- apply(X=nn_10_3_50_5[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_6 <- apply(X=nn_10_3_50_6[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_7 <- apply(X=nn_10_3_50_7[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_8 <- apply(X=nn_10_3_50_8[, seq(1, 200, 4)], MARGIN=1, FUN=median)
mse_in_mean_9 <- apply(X=nn_10_3_50_9[, seq(1, 200, 4)], MARGIN=1, FUN=median)


mse_in_rob <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_2[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_3[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_4[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_5[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_6[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_7[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_8[, seq(1, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_9[, seq(1, 200, 4)], MARGIN=1, FUN=median)
));colnames(mse_in_rob) <- 1:9

mse_out_rob <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_2[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_3[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_4[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_5[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_6[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_7[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_8[, seq(2, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_9[, seq(2, 200, 4)], MARGIN=1, FUN=median)
));colnames(mse_out_rob) <- 1:9

sharpe_in_rob <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_2[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_3[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_4[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_5[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_6[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_7[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_8[, seq(3, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_9[, seq(3, 200, 4)], MARGIN=1, FUN=median)
));colnames(sharpe_in_rob) <- 1:9

sharpe_out_rob <- as.data.frame(cbind(
  apply(X=nn_10_3_50_1[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_2[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_3[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_4[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_5[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_6[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_7[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_8[, seq(4, 200, 4)], MARGIN=1, FUN=median),
  apply(X=nn_10_3_50_9[, seq(4, 200, 4)], MARGIN=1, FUN=median)
));colnames(sharpe_out_rob) <- 1:9

save(mse_in_rob, mse_out_rob, sharpe_in_rob, sharpe_out_rob, file = "data/batch_5/medianirino.rda")

load("data/batch_5/medianirino.rda")


#.####
# 1#### 
# width=1400
# height=900
mse_1 <- nn_10_3_50_1[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
plot_all_rect(mati=mse_1, real=50, title="MSE Split: 1")
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
plot_by_layer_rect(mati=mse_1, real=50, title="MSE Split: 1")

# 2####
mse_2 <- nn_10_3_50_2[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_2, real=50, title="In/Out Split: 2")
plot_all_rect_scale(mati=mse_2, real=50, title="MSE Split: 2")
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_2, real=50, title="In/Out Split: 2")
plot_by_layer_rect_scale(mati=mse_2, real=50, title="MSE Split: 2",
                         scale_vec=c(2, 2, 4))

# 3####
mse_3 <- nn_10_3_50_3[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_3, real=50, title="In/Out Split: 3")
plot_all_rect_scale(mati=mse_3, real=50, title="MSE Split: 3")
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_3, real=50, title="In/Out Split: 3")
plot_by_layer_rect_scale(mati=mse_3, real=50, title="MSE Split: 3",
                         scale_vec=c(3, 3, 3))
# 4####
mse_4 <- nn_10_3_50_4[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_4, real=50, title="In/Out Split: 4")
plot_all_rect_scale(mati=mse_4, real=50, title="MSE Split: 4", scale_fac=9)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
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
                         scale_vec=c(10, 10, 10))

# 6####
mse_6 <- nn_10_3_50_6[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_6, real=50, title="In/Out Split: 6")
plot_all_rect_scale(mati=mse_6, real=50, title="MSE Split: 6", scale_fac=7)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_6, real=50, title="In/Out Split: 6")
plot_by_layer_rect_scale(mati=mse_6, real=50, title="MSE Split: 6",
                         scale_vec=c(10, 25, 10))

# 7####
mse_7 <- nn_10_3_50_7[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_7, real=50, title="In/Out Split: 7")
plot_all_rect_scale(mati=mse_7, real=50, title="MSE Split: 7", scale_fac=10)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_7, real=50, title="In/Out Split: 7")
plot_by_layer_rect_scale(mati=mse_7, real=50, title="MSE Split: 7",
                         scale_vec=c(35, 25, 5))

# 8####
mse_8 <- nn_10_3_50_8[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_8, real=50, title="In/Out Split: 8")
plot_all_rect_scale(mati=mse_8, real=50, title="MSE Split: 8", scale_fac=5)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_8, real=50, title="In/Out Split: 8")
plot_by_layer_rect_scale(mati=mse_8, real=50, title="MSE Split: 8",
                         scale_vec=c(30, 15, 5))

# 9####
mse_9 <- nn_10_3_50_9[,c(rbind(seq(1, 200, 4), seq(2, 200, 4)))]
# plot_all_rect(mati=mse_9, real=50, title="In/Out Split: 9")
plot_all_rect_scale(mati=mse_9, real=50, title="MSE Split: 9", scale_fac=7)
legend("center", legend=c("1 Layer", '2 Layers', '3 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D', '#80FF004D'), horiz=TRUE, title="Layer partitioning")
# plot_by_layer_rect(mati=mse_9, real=50, title="In/Out Split: 9")
plot_by_layer_rect_scale(mati=mse_9, real=50, title="MSE Split: 9",
                         scale_vec=c(10, 20, 8))





# Mean Plots####
split_colors <- c("#59C7EB", "#E0607E", "#0A9086", "#FEA090", "#3E5496", "#EFDC60", "#8E2043", "#9AA0A7", "#AC9A8C")

load("data/batch_5/meanerino.rda")
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




# Only two Layers
split_colors <- c("#59C7EB", "#E0607E", "#0A9086", "#FEA090", "#3E5496", "#EFDC60", "#8E2043", "#9AA0A7", "#AC9A8C")

plot_mse_mean <- function(mse_in, mse_out, title="",scale_fac=3) {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mse_in), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Only the first two
  # layers <-layers[1:2]
  
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
mse_in[1:110,]

load("data/batch_5/meanerino.rda")
plot_mse_mean(mse_in=mse_in[1:110,], mse_out=mse_out[1:110,], title="Mean MSE over all 9 splits", 50)
legend("left", legend=c("1 Layer", '2 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")

# rob
plot_mse_mean(mse_in=mse_in_rob[1:110,], mse_out=mse_out_rob[1:110,], title="Median MSE over all 9 splits", 20)
legend("left", legend=c("1 Layer", '2 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")



plot_sharpe_mean(sharpe_in=sharpe_in[1:110,], sharpe_out=sharpe_out[1:110,], title="Mean Sharpe over all 9 splits")
legend("left", legend=c("1 Layer", '2 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")

# rob
plot_sharpe_mean(sharpe_in=sharpe_in_rob[1:110,], sharpe_out=sharpe_out_rob[1:110,], title="Median Sharpe over all 9 splits")
legend("left", legend=c("1 Layer", '2 Layers'), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = c('#FF00004D', '#0000FF4D'), horiz=TRUE, title="Layer partitioning")
legend("right", legend=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), pch=15, pt.cex=2, cex=0.8, bty='n',
       col = split_colors, horiz=TRUE, title="Train/Test Splits")
