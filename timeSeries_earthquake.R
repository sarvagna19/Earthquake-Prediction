rm(list = ls())
install.packages("lubridate")
library(lubridate)
install.packages("xts")
library(xts)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)


inf_data <- read.csv("C:/Users/Mayurakshi/Desktop/mba/1st semester/Machine Learning/Data/database.csv")
inf_data<-subset(inf_data,Magnitude>=7)
View(inf_data)

summary(inf_data)
inf_data$Date <- dmy(inf_data$Date)
View(inf_data) ## changing date to yyyy-mm-dd format
plot(inf_data$Date,inf_data$Magnitude, type="l")

#creating ts object
data<-ts(inf_data[,c('Magnitude')],start = c(1965,1,2),end = c(2017,4,1), frequency =360)
plot(data, xlab='Years', ylab = 'Magnitude' , type = "l", col = "blue")

#plot(data, xlab='Years', ylab = 'Close Price' , type = "l", col = "red")

deseasonal_data <- na.omit(data)

#A series is said to be stationary when its mean, variance, and autocovariance are time invariant.


#Augmented Dickey-Fuller (ADF) test 

adf.test(deseasonal_data)
plot(diff(log10(deseasonal_data)),ylab='Differenced Log (Magnitude)')
adf.test(diff(log10(deseasonal_data)))
par(mfrow = c(1,1))
acf(ts(diff(log10(deseasonal_data))),main='ACF Magnitude')
pacf(ts(diff(log10(deseasonal_data))),main='PACF Magnitude')

# Fitting arima model
auto.arima(deseasonal_data, seasonal = FALSE)
ARIMAfit<-auto.arima(deseasonal_data, seasonal=FALSE)
forecast<-forecast(ARIMAfit,h=100)
plot(forecast)
summary(forecast)
acf(ts(ARIMAfit$residuals),main='ACF Residual', lag.max = 20)
pacf(ts(ARIMAfit$residuals),main='PACF Residual')

