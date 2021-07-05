############################
# this file loads sales
############################

# Clear all variables in workspace
rm(list=ls())

# load the forecasting package
library(fpp2)

# load data
data <- read.csv("Alcohol_Sales.csv")

#declare this time series data 
Y <- ts(data[,2],start=c(1992,1),frequency = 12)

##################
# perliminary Analyis
##################

# Time Plot
autoplot(Y) +
  ggtitle("time Plot: sales per day") +
  ylab("Alcohol Sales")


# Data has a strong trend. Investigate transformations.
# take the first difference of the data to remove the trend 
DY <- diff(Y) 

# Time Plot
autoplot(DY) +
  ggtitle("time Plot: sales per day") +
  ylab("Alcohol Sales")

# series appears stationary, use to investigate seasonally 
ggseasonplot(DY) + 
  ggtitle("seasonal Plot: change in Daily retail sales") +
  ylab("Alcohol Sales")

# lets look at another seasonal plot 
ggsubseriesplot(DY)

###########################
# Our series Y, has trend and seasonality 
#to remove the trend, we take the first difference 
# The first difference series still has seasonality 
##########################

##########
# use a benchmark method 
# lets use the seasonal naive method as our benchmark
# y_t = y_{t-s} + e_t
#########
fit <- snaive(DY) # Resdisual SD = 589.59
print(summary(fit))
checkresiduals(fit)

###############################
# Fit ETS method 
###############################
fit_ets <- ets(Y) # Residual SD = 0.0455
print(summary(fit_ets))
checkresiduals(fit_ets)

##############################
fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE, approximation =FALSE, trace =TRUE) # Residual SD = 322.4
print(summary(fit_arima))
checkresiduals(fit_arima)

##############################
# forecast with ARIMA model
#####################
fcst <- forecast(fit_arima,h=24)
autoplot(fcst,include=60)
print(summary(fcst))

###########################
sales <- read.csv(file.choose())
salestimeseries <- ts(sales)
plot.ts(salestimeseries)
salestimeseriesdiff1 <- diff(salestimeseries, differences=1)
plot.ts(salestimeseriesdiff1)
#adf.test((salestimeseries), alternative="stationary", k=0)
#?adf.test
#library(aTSA)
#stationary.test(salestimeseriesdiff1)
#stationary.test(salestimeseriesdiff1, method = "pp") 
#stationary.test(salestimeseriesdiff1, method = "kpss")
#acf(salestimeseriesdiff1, lag.max=20) 
#acf(salestimeseriesdiff1, lag.max=20, plot=FALSE)
pacf(salestimeseriesdiff1, lag.max=20) 
pacf(salestimeseriesdiff1, lag.max=20, plot=FALSE)
sales <- scan("Alcohol_Sales.csv")
salestimeseries <- ts(sales)
plot.ts(salestimeseries)
salestimeseriesdiff1 <- diff(salestimeseries, differences=1)
plot.ts(salestimeseriesdiff1)
pacf(salestimeseriesdiff1, lag.max=20) 
pacf(salestimeseriesdiff1, lag.max=20, plot=FALSE)
?auto.arima()
auto.arima(sales) # On the original Series
auto.arima(salestimeseries) # On the time-series converted series
auto.arima(salestimeseriesdiff1)