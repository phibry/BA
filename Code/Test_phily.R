# Imports####
source("add/libraries.r") 
source("add/Functions.r")


# Load Data####
source("add/libraries.r") 
load("data/log_ret_27_03_21.rda")
logret <- log_ret_27_03_21



# Last Period####
# Test: Last Month
# Training: Last 6 Months
data_obj <- data_function(x=logret, lags=6, in_out_sep="2021-03-01", start="2020-09-01", end="2021-03-27")
nn_7_7_50_last <- nn_nl_comb_sharpe_mse(maxneuron=7,
                                    maxlayer=7,
                                    real=50,
                                    data_obj=data_obj)
# Don't do that
# 960799 nets * 50 Iterations = 48039950
# 9.2h Computation for 16482 of 960799 (2%)





# Plots####

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
     frame.plot = FALSE,
     xaxt="n",
     xlab="",
     ylab="Log Return", ylim=c(-0.2, 0.2))
axis.Date(1, at=seq(min(df_sub$date), max(df_sub$date), by="months"), format="%m-%y")


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

