##### Flowchart Overview Methodology ###### 

#install.packages("DiagrammeR")
library(DiagrammeR)

grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = oval, color = Black, style = 'solid']
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3', shape = diamond]
  tab4 [label = '@@4']

  tab1 -> tab2 -> tab3 -> tab4;
  tab3 -> tab1 [label = '  if prediction inaccurate'];
}
  
  [1]: '1) Selection of a suitable neural network'
  [2]: '2) Explainable artificial intelligence (XAI)'    
  [3]: '3) Predictive accuracy' 
  [4]: '4) Implementation trading strategy'
  ")










##### Volatility prediction for trading signals: Method 2 using rugarch #####

# Data prep
dat <- log_ret_27_03_21
head(dat)

# Separation of data
sep <- "2020-05-01"
dat[as.Date(sep)]

# Set window size
window <- 365

# Start prediction using package rugarch
# Set window range to 365 days and iterate daily

# Just to inform: Window for first iteration
as.Date(sep)-window # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep)-window)]
as.Date(sep) # or for data.frame: index(dat)[which(row.names(dat) == as.Date(sep))]

# Just to inform: Window for last iteration
as.Date("2021-03-26")-window
as.Date("2021-03-26")

# Predictions with rolling window one-step-ahead forecasts
# Defining left border of rolling window in the beginning (date_0) and at the end (date_n)
date_0 <- as.Date(sep)-window
date_n <- as.Date("2021-03-26")-window
datum <- date_0

# Transform to data frame for ugarch functions
dat <- as.data.frame(dat)

# Define prediction matrix
vola_mat <- data.frame(Date = NA, Volatility = rep(NA, difftime("2021-03-27", as.Date(sep))+1))
?data.frame
# Specify using ugarchspec() from rugarch package
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)))

# One-step-ahead forecasts
j <- 1

while(datum <= date_n){
  # Estimation of model
  garch_fit <- ugarchfit(spec = garch_spec, 
                         data = dat$`BTC-USD.Close`[(index(dat)[which(row.names(dat) == datum)])
                                                    :(index(dat)[which(row.names(dat) == datum + window)])
                         ]) # or datum + window?
  
  print(paste("Iteration: ", j, " Date: ", datum, "End: ", datum + window))
  datum <- datum + 1
  
  # Predictions
  vola_mat$Date[j] <- as.Date(datum + window + 1, format = "%Y-%m-%d")
  vola_mat$Volatility[j] <- sigma(ugarchforecast(fitORspec = garch_fit, n.ahead = 1))
  j <- j + 1
}

plot(vola_mat, type = "l")

