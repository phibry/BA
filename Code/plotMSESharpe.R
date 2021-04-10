# Sharpe-MSE Plots####
plot_all_rect_mse <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  color <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  seq_mse_in <- seq(1, real*4, 4)
  seq_mse_out <- seq(2, real*4, 4)
  seq_sharpe_in <- seq(3, real*4, 4)
  seq_sharpe_out <- seq(4, real*4, 4)
  
  
  # MSE: In-sample
  for(i in seq_mse_in) {
    if (i == 1) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_mse_in]) ,max(mati[,seq_mse_in])),
           xlim=c(1, dim(mati)[1]),
           col=color,
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  # for (i in head(layers, -1)) {
  #   abline(v=(1+i), lty=2)
  # }
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,seq_mse_in]),
         ytop = max(mati[,seq_mse_in]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, min(mati[,seq_mse_in])*1.02, i)
  }
  
  
  # MSE: Out-of-sample
  color <- 1
  for(i in seq_mse_out) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_mse_out]), max(mati[,seq_mse_out])),
           col=color,
           ylab="MSE",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  # for (i in head(layers, -1)) {
  #   abline(v=(1+i), lty=2)
  # }
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,seq_mse_out]),
         ytop = max(mati[,seq_mse_out]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, max(mati[,seq_mse_out])*0.98, i)
  }
  
  par(par_default)
}
plot_all_rect_sharpe <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Plots mit Rect
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  ## In-Sample
  # color indizes for plots
  color <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  seq_mse_in <- seq(1, real*4, 4)
  seq_mse_out <- seq(2, real*4, 4)
  seq_sharpe_in <- seq(3, real*4, 4)
  seq_sharpe_out <- seq(4, real*4, 4)
  
  
  # Sharpe: In-sample
  for(i in seq_sharpe_in) {
    if (i == 3) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_sharpe_in]) ,max(mati[,seq_sharpe_in])),
           xlim=c(1, dim(mati)[1]),
           col=color,
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  # for (i in head(layers, -1)) {
  #   abline(v=(1+i), lty=2)
  # }
  
  startl <- c(1, head(layers, -1)+1)
  endl <- layers
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,seq_sharpe_in]),
         ytop = max(mati[,seq_sharpe_in]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, min(mati[,seq_sharpe_in])*0.9, i)
  }
  
  
  # MSE: Out-of-sample
  color <- 1
  for(i in seq_sharpe_out) {
    if (i == 4) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,seq_sharpe_out]), max(mati[,seq_sharpe_out])),
           col=color,
           ylab="Sharpe",
           frame.plot = FALSE,
           xaxt="n",
           xlab="")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  # for (i in head(layers, -1)) {
  #   abline(v=(1+i), lty=2)
  # }
  
  for (i in 1:length(layers)) {
    rect(xleft = startl[i],
         xright = endl[i],
         ybottom = min(mati[,seq_sharpe_out]),
         ytop = max(mati[,seq_sharpe_out]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, max(mati[,seq_sharpe_out])*0.9, i)
  }
  
  par(par_default)
}

# Start: 2018-01-01
# End: 2021-03-27
# Seperator: 2020-05-01


#.####
# 5N 2L 20R####
load("data/batch_2/nn_5_2_20.rda")
plot_all_rect_mse(nn_5_2_20, 20, title="5N2L20R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
plot_all_rect_sharpe(nn_5_2_20, 20, title="5N2L20R, Start:18-01-01, End:21-03-27, Sep:20-05-01")


# 5N 5L 20R####
load("data/batch_2/nn_5_5_20.rda")
plot_all_rect_mse(nn_5_5_20, 20, title="5N5L20R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
plot_all_rect_sharpe(nn_5_5_20, 20, title="5N5L20R, Start:18-01-01, End:21-03-27, Sep:20-05-01")


# 5N 5L 50R####
load("data/batch_2/nn_5_5_50.rda")
plot_all_rect_mse(nn_5_5_50, 50, title="5N5L50R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
plot_all_rect_sharpe(nn_5_5_50, 50, title="5N5L50R, Start:18-01-01, End:21-03-27, Sep:20-05-01")


# 12N 2L 50R####
load("data/batch_2/nn_10_3_40.rda")
plot_all_rect_mse(nn_10_3_40, 40, title="10N3L40R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
plot_all_rect_sharpe(nn_10_3_40, 40, title="10N3L40R, Start:18-01-01, End:21-03-27, Sep:20-05-01")


# 12N 2L 50R####
load("data/batch_2/nn_12_2_50.rda")
plot_all_rect_mse(nn_12_2_50, 50, title="12N2L50R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
plot_all_rect_sharpe(nn_12_2_50, 50, title="12N2L50R, Start:18-01-01, End:21-03-27, Sep:20-05-01")
