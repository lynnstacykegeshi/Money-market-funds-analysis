rm(list = ls())

library(dplyr)
library(lubridate)
library(xts)
library(tseries)
library(forecast)
library(seastests)
library(FinTS)
library(tidyverse)
library(ggplot2)
library(ggplot2)

MMF_Data<-read.csv("CMA Data.csv", header = TRUE,  sep = ",")
View(MMF_Data)
class(MMF_Data)

MMF_Data$MMF <- as.numeric(MMF_Data$MMF) 

summary(MMF_Data)

MMF_Data$Date
 TS<- ts(MMF_Data$MMF, start = c(2018, 6), frequency = 4)

class(TS)
plot.ts(TS,main = "MMF AUM With Time", xlab = "Time in years", ylab="AUM" )



###Preliminary Analysis
#Boxplot the data
boxplot(MMF~Date, 
        data=MMF_Data, 
        main="Box plot showing AUM for each quarter",
        xlab="Quarters",
        ylab="AUM in Kshs",
        col="deepskyblue",
        border="darkblue")


#Decomposing the data
MMF_dec<-decompose(TS, "multiplicative")
MMF_dec
plot(MMF_dec)

#The time series has an upward trend.


#Plot the original data ACF and PACF
ggtsdisplay(TS)
#??if you notice that in the acf chart there are a few significant spikes at the beginning but the significant spikes quickly decrease to zero line in the middle at the same time in the pacf chart you only have a one or two significant spike at that moment you should use ar model

#ACF is geometric 
#PACF is significant till 1st lag
#Conclusion : AR (1) model
#The ACF plot shows slow decay of lag to 0 indicating an AR model. The PACF plot suggests AR model of the order 1 AR(1) as PACF number is close to 0 after lag 1.


#Test for stationarity
# If p<5%, adf (non-stationary) and kpss test (stationary)
adf.test(TS)
#pvalue>0.05: 0.9643>0.05, Fail to reject the null hypothesis and conclude that the time series is non stationary

kpss.test(TS)
#p-value is 0.01796. 0.01796<0.05, Reject null hypothesis and conclude that the time series is non stationary.
#data is not stationary under variance so we get the log transform in order to amke it stationary

###Seasonality
#ggseasonplot(TS)
#in general there's not much seasonality

ggsubseriesplot(TS)
#this tell us that the mean each quarter is basically almost the same

isSeasonal(TS, test="wo") #FALSE tells us the data is not seasonal
#Conclusion the time series does not exhibit evidence of seasonality.

#Log transform the data to stabilize the non-constant variance
#In finance, continuously compounding returns are more common. That is why we will calculate the log first difference.
TSlog<-log(TS)
plot.ts(TSlog)
adf.test(TSlog)

#Check how many differeneces are needed to make the time series stationary
ndiffs(TS)
ndiffs(TSlog)

#We then get the first difference to remove the trend

diffMMF<-diff(TSlog)
ndiffs(diffMMF) #check for number of differences to see if the time series has been detrended
plot.ts(diffMMF, main="Differenced AUM of MMF",xlab="Quarters", ylab="AUM", col="blue")
ggtsdisplay(diffMMF)


#We can confirm that our data is stationary by applying ADF test.
adf.test(diffMMF)#Reject null hypothesis and conclude that the time series is stationary
kpss.test(diffMMF)#Fail to reject the null hypotheis and conclude that the time series is stationary

#Get the ACF and PACf of the stationary series
ggtsdisplay(diffMMF)

#Plot the stationary time series
plot.zoo(diffMMF,main = "Differenced Data", lwd = 2, col = "blue")


################
# fit ARIMA model
MMF_arima <- auto.arima(TS, seasonal = F, trace = T)
MMF_arima
summary(MMF_arima)

#calculate residuals of each model
checkresiduals(MMF_arima)
Box.test(MMF_arima$resid, lag = 5, type = "Ljung-Box")


#Forecast the time series
forecast_MMF = forecast(MMF_arima,h=8)
forecast_MMF

# plot forecast for model
autoplot(forecast_MMF)
forecastedvaluesextracted = as.numeric(forecast_MMF$mean)
finalforecastedvalues=exp(forecastedvaluesextracted)
finalforecastedvalues

##################################################################################
#AAnother method
acf(TS) #there's low correlation
pacf(TS)
adf.test(TS) #p-value should be less than 0.05 then it means the data is not stationary

ts_model <- auto.arima(TS,ic="aic", trace = TRUE)
auto.arima(TS)

ts_model

pacf(ts(ts_model$residuals))

forecast_ts <- forecast(ts_model,level = c(95),h=8)
forecast_ts
plot(forecast_ts)

Box.test(forecast_ts, lag = 2, type = "Ljung-Box") #p-value should be greater than .05



