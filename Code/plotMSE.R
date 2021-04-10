# MSE Plots####
plot_all_rect <- function(mati, real, title="") {
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
  
  in_samp_seq <- seq(1, real*2, 2)
  for(i in in_samp_seq) {
    if (i == 1) {
      plot(mati[,i],
           # main="In-Sample",
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,in_samp_seq]) ,max(mati[,in_samp_seq])),
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
         ybottom = min(mati[,in_samp_seq]),
         ytop = max(mati[,in_samp_seq]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, min(mati[,in_samp_seq])*1.02, i)
  }
  
  
  ## Out-of-Sample
  color <- 1
  out_of_samp_seq <- seq(2, real*2, 2)
  for(i in out_of_samp_seq) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,out_of_samp_seq]), max(mati[,out_of_samp_seq])),
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
         ybottom = min(mati[,out_of_samp_seq]),
         ytop = max(mati[,out_of_samp_seq]),
         col=colorcodes[i])#,
    # border = "transparent")
    text(startl[i]+(endl[i]-startl[i])/2, max(mati[,out_of_samp_seq])*0.98, i)
  }
  
  par(par_default)
}
plot_by_layer_rect <- function(mati, real, title="") {
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Plots by Layer
  par_default <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar=c(3,5,3,2))
  in_samp_seq <- seq(1, real*2, 2)
  out_of_samp_seq <- seq(2, real*2, 2)
  iter <- 1
  prev_it <- 1
  
  # color codes for the rect
  colorcodes <- c("#FF00001A", # red
                  "#0000FF1A", # blue
                  "#80FF001A", # green
                  "#FF80001A", # orange
                  "#00FFFF1A", # teal
                  "#8000FF1A") # purple
  
  for(i in layers) {
    color <- 1
    
    for(j in in_samp_seq) {
      mini_in <- min(mati[prev_it:i, in_samp_seq])
      maxi_in <- max(mati[prev_it:i, in_samp_seq])
      if (j == 1) {
        plot(mati[prev_it:i, j],
             ylim=c(mini_in, maxi_in),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE")
        color = color + 1

      } else {
        lines(mati[prev_it:i, j], col=color)
        # points(mati[prev_it:i, j], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    color <- 1
    for(k in out_of_samp_seq) {
      mini_out <- min(mati[prev_it:i, out_of_samp_seq])
      maxi_out <- max(mati[prev_it:i, out_of_samp_seq])
      if (k == 2) {
        plot(mati[prev_it:i, k],
             ylim=c(mini_out, maxi_out),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE")
        color = color + 1 
      } else {
        lines(mati[prev_it:i, k], col=color)
        # points(mati[prev_it:i, k], col=color)
        color = color + 1
      }
    }
    
    rect(xleft=par('usr')[1],
         xright=par('usr')[2],
         ybottom=par('usr')[3],
         ytop=par('usr')[4],
         col=colorcodes[iter])
    
    prev_it <- i+1
    iter <- iter + 1
  }
  par(par_default)
}

plot_all <- function(mati, real, title="") {
  # Layer Breakpoints
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  
  
  # Full Plots
  par(mfrow=c(2,1))
  # In-Sample
  color <- 1
  
  in_samp_seq <- seq(1, real*2, 2)
  for(i in in_samp_seq) {
    if (i == 1) {
      plot(mati[,i],
           main=paste(title, ": In-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,in_samp_seq]) ,max(mati[,in_samp_seq])),
           xlim=c(1, dim(mati)[1]),
           col=color,
           ylab="MSE")
      
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  for (i in head(layers, -1)) {
    abline(v=(1+i), lty=2)
  }
  
  
  # Out-of-Sample
  color <- 1
  out_of_samp_seq <- seq(2, real*2, 2)
  for(i in out_of_samp_seq) {
    if (i == 2) {
      plot(mati[,i],
           main=paste(title, ": Out-of-Sample", sep=""),
           type="l",
           ylim=c(min(mati[,out_of_samp_seq]), max(mati[,out_of_samp_seq])),
           col=color,
           ylab="MSE")
      color = color + 1
    } else {
      lines(mati[,i], col=color)
      color = color + 1
    }
  }
  for (i in head(layers, -1)) {
    abline(v=(1+i), lty=2)
  }
  
}
plot_by_layer <- function(mati, real, title="") {
  str_splitter <- function(x) {
    return(length(as.numeric(unlist(strsplit(x, ", ")))))
  }
  
  layers <- sapply(X=rownames(mati), FUN=str_splitter, USE.NAMES=FALSE)
  layers <- as.numeric(table(layers))
  layers <- cumsum(layers)
  # Plots by Layer
  par(mfrow=c(2,1))
  in_samp_seq <- seq(1, real*2, 2)
  out_of_samp_seq <- seq(2, real*2, 2)
  iter <- 1
  prev_it <- 1
  # mini_in <- min(mati[,in_samp_seq])
  # maxi_in <- max(mati[,in_samp_seq])
  
  # mini_out <- min(mati[,out_of_samp_seq])
  # maxi_out <- max(mati[,out_of_samp_seq])
  
  for(i in layers) {
    color <- 1
    
    for(j in in_samp_seq) {
      mini_in <- min(mati[prev_it:i, in_samp_seq])
      maxi_in <- max(mati[prev_it:i, in_samp_seq])
      if (j == 1) {
        plot(mati[prev_it:i, j],
             ylim=c(mini_in, maxi_in),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE")
        
        color = color + 1
      } else {
        lines(mati[prev_it:i, j], col=color)
        color = color + 1
      }
    }
    
    
    color <- 1
    for(k in out_of_samp_seq) {
      mini_out <- min(mati[prev_it:i, out_of_samp_seq])
      maxi_out <- max(mati[prev_it:i, out_of_samp_seq])
      if (k == 2) {
        plot(mati[prev_it:i, k],
             ylim=c(mini_out, maxi_out),
             main=paste(title, " Layer: ", iter, sep=""),
             type="l",
             col=color,
             ylab="MSE")
        color = color + 1 
      } else {
        lines(mati[prev_it:i, k], col=color)
        color = color + 1
      }
    }
    
    prev_it <- i+1
    iter <- iter + 1
  }
}



#.####
# 5N 5L 10R####
load("data/batch_1/optim_5_5_10.rda")
cor(optim_5_5_10[,1:2])
plot(optim_5_5_10[,1:2])

cor(optim_5_5_10[,3:4])
plot(optim_5_5_10[,3:4])

cor(optim_5_5_10[,5:6])
cor(optim_5_5_10[,7:8])
cor(optim_5_5_10[,9:10])

cor(optim_5_5_10[,11:12])
plot(optim_5_5_10[,11:12])


# rect
plot_all_rect(optim_5_5_10, 10, "5N 5L 10R")
plot_by_layer_rect(optim_5_5_10, 10, "5N 5L 10R")
# normal
plot_all(optim_5_5_10, 10, "5N 5L 10R")
plot_by_layer(optim_5_5_10, 10, "5N 5L 10R")



# 5N 5L 20R####
load("data/batch_1/optim_5_5_20.rda")
# rect
plot_all_rect(optim_5_5_20, 20, "5N 5L 20R")
plot_by_layer_rect(optim_5_5_20, 20, "5N 5L 20R")
# normal
plot_all(optim_5_5_20, 20, "5N 5L 20R")
plot_by_layer(optim_5_5_20, 20, "5N 5L 20R")



# 5N 3L 30R####
load("data/batch_1/optim_5_3_30.rda")
# rect
plot_all_rect(optim_5_3_30, 30, "5N 3L 30R")
plot_by_layer_rect(optim_5_3_30, 30, "5N 3L 30R")
# normal
plot_all(optim_5_3_30, 30, "5N 3L 30R")
plot_by_layer(optim_5_3_30, 30, "5N 3L 30R")



# 6N 2L 40R####
load("data/batch_1/optim_6_2_40.rda")
# rect
plot_all_rect(optim_6_2_40, 40, "6N 2L 40R")
plot_by_layer_rect(optim_6_2_40, 40, "6N 2L 40R")
# normal
plot_all(optim_6_2_40, 40, "6N 2L 40R")
plot_by_layer(optim_6_2_40, 40, "6N 2L 40R")



# 10N 2L 20R####
load("data/batch_1/optim_10_2_10.rda")
# rect
plot_all_rect(optim_10_2_10, 10, "10N 2L 10R")
plot_by_layer_rect(optim_10_2_10, 10, "10N 2L 10R")
# normal
plot_all(optim_10_2_10, 10, "10N 2L 10R")
plot_by_layer(optim_10_2_10, 10, "10N 2L 10R")









#.####
# Test####


par_default <- par(no.readonly = TRUE)

par(mfrow=c(1,1), mar=c(3,5,3,2))


plot(cumsum(rnorm(1000)), type="l")
par()
par('usr')[1]
rect(xleft=par('usr')[1],
     xright=par('usr')[2],
     ybottom=par('usr')[3],
     ytop=par('usr')[4],
     col="blue")
par(par_default)


par(mfrow=c(1,2))
plot(cumsum(rnorm(1000)), type="l")
par()
rect(xleft=par('usr')[1],
     xright=par('usr')[2],
     ybottom=par('usr')[3],
     ytop=par('usr')[4],
     col="#FF00001A")

plot(rnorm(1000))
rect(xleft=par('usr')[1],
     xright=par('usr')[2],
     ybottom=par('usr')[3],
     ytop=par('usr')[4],
     col="#FF00001A")


# 
# par(mfrow=c(1,1))
# set.seed(123)
# dat <- rnorm(41)
# plot(dat, type="l", xaxt="n")
# par()
# par("xaxp")[2]
# 
# end <- par("xaxp")[2]
# splitter <- par("xaxp")[3]
# multi <- round(end / splitter)
# 
# at <- c(1, cumsum(rep(multi, splitter)))
# labels <- c()
# 
# axis(1, at=at, labels=lables)
# ?axis