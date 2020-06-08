rm(list = ls())
#setting up the working directory
setwd("E:/TUM/TUM_Semester_2/Satistical learning/Exam Tasks/Problem_Set_3/Task 3")

#install.packages(astsa)
library(astsa)
library(forecast)
library(tseries)

data = read.csv("traffic_data.csv")


head(data)

summary(data$volume)
summary(data$speed)

#setting up time series data
ts_volume = as.ts(data$volume)
ts_speed = as.ts(data$speed)

#2. Stationary test by unit root test
adf.test(ts_volume,  alternative = "stationary")
adf.test(ts_speed, alternative = "stationary")

adf.test(diff(ts_volume), alternative = "stationary")

acf2(ts_volume)

#plots to detect trend
plot(ts_volume, xlab='Hours', ylab = 'Volume', main = "Volume Time Series")
abline(reg=lm(ts_volume~time(ts_volume)))

plot(ts_speed, xlab='Hours', ylab = 'Volume', main = "Speed Time Series")
abline(reg=lm(ts_speed~time(ts_speed)))

#Splitting the time series to model the data and validate the accuracy of the forecast
train_ts_speed = as.ts(data$speed[1:602],)
test_ts_speed = as.ts(data$speed[603:605],)

train_ts_volume = as.ts(data$volume[1:602],)
test_ts_volume = as.ts(data$volume[603:605])

#ACF and PACF  analysis
acf2(diff(ts_volume), max.lag = 15)
acf2(diff(ts_speed), max.lag = 15)

#Finding the p,d,q values for the model - volume
auto.arima(train_ts_volume, ic = "aic", trace = TRUE, allowdrift = FALSE)
auto.arima(train_ts_volume, ic = "bic", trace = TRUE, allowdrift = FALSE)

sarima(train_ts_volume, 4,1,4)
sarima(train_ts_volume, 1,1,4)
sarima(train_ts_volume, 0,1,4)

#sarima(train_ts_volume, 0,1,4) chosen
model_volume = sarima(train_ts_volume, 0,1,4, no.constant = TRUE)
model_volume$ttable
sarima(train_ts_volume, 0,1,4,no.constant = TRUE)

#forecase_volume = sarima.for(train_ts_volume, n.ahead = 3,0,1,0,3,1,1,4 )
forecase_volume$pred
test_ts_volume

#Finding the p,d,q values for the model - speed
plot(ts_speed)
auto.arima(train_ts_speed, ic = "aic", trace = TRUE, allowdrift = FALSE)
auto.arima(train_ts_speed, ic = "bic", trace = TRUE, allowdrift = FALSE)

arima

arima(train_ts_speed, order = c(2,1,2))
arima(train_ts_speed, order = c(4,1,2))
arima(train_ts_speed, order = c(2,1,4))

sarima(train_ts_speed, 2,1,2)
sarima(train_ts_speed, 2,1,4)
sarima(train_ts_speed, 4,1,2)
sarima(train_ts_speed, 5,1,4)
model_speed = sarima(train_ts_speed, 5,1,4)
model_speed$ttable


auto.arima(train_ts_speed, ic = "aic", trace = TRUE)
sarima(ts_speed, 4,1,4,no.constant = TRUE)

acf2(diff(ts_speed))
sarima.for(ts_speed,n.ahead = 25, 4,1,4 )
sarima.for(ts_volume, n.ahead = 25,5,1,4)


#------------------------------Forecasting----------------------------------------------------

#speed data forecast

m_fct_speed = Arima(train_ts_speed, order = c(5,1,4))
forecast_speed = forecast(m_fct_speed,h = 3)
plot(forecast_speed)

forecast_speed_2 = sarima.for(train_ts_speed, n.ahead = 3, 5,1,4)
plot(forecast_speed_2)

forecast_speed_3 = predict(m_fct_speed, n.ahead = 3)
ts.plot(train_ts_speed, forecast_speed_3$pred)

plot(forecast_speed$x, col = "red")

plot(m_fct_speed$x, col = "red")
lines(fitted(m_fct_speed), col ="blue")
legend(460,30, legend = c("Original Values", "Fitted Values"),col=c("red", "blue"), lty=1:1, cex=0.6)


#volume data forecast
m_fct_volume = Arima(train_ts_volume, order = c(0,1,4),include.constant = FALSE)
#forecasted value plot
forecast_volume_2 = sarima.for(train_ts_volume, n.ahead = 3, 0,1,4)

#Original and fitted plot
plot(m_fct_volume$x, col = "red")
lines(fitted(m_fct_volume), col = "blue")
legend(380,37, legend = c("Original Values", "Fitted Values"),col=c("red", "blue"), lty=1:1, cex=0.6)


#-----------forecast accuracy
accuracy(f = m_fct_volume, test = train_ts_volume)
accuracy( f = m_fct_speed, test = train_ts_speed)


mape(as.numeric(test_ts_speed), as.numeric(forecast_speed_2$pred))
mape(as.numeric(test_ts_volume), as.numeric(forecast_volume_2$pred))
