library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols('CINE.L',from='2020-03-16',to='2020-09-01')
CINE.L
class(CINE.L)
Cineworld_close<-CINE.L[,4]
Cineworld_close
plot(Cineworld_close)
class(Cineworld_close)
par(mfrow=c(1,1))

#na.pass ignores missing values, only uses complete cases
acf(Cineworld_close,main='ACF',na.action = na.pass)
pacf(Cineworld_close,main="PACF",na.action = na.pass)
auto.arima(Cineworld_close,seasonal=FALSE)
log=diff(log(Cineworld_close),lag=1)
log=log[!is.na(log)]
plot(log,type='l',main='log returns')
#small p value, removed non stationary data
print(adf.test(log))
auto.arima(log,seasonal = FALSE)
str(log)


sampleSize<-floor(0.8*nrow(log))
set.seed(2)
train_ind<-sample(seq_len(nrow(log)),size=sampleSize)
train<-log[train_ind,]
test<-log[-train_ind,]
par(mfrow=c(1,2))
acf(train,main='acf')
pacf(train,main='pacf')
auto.arima(train,seasonal=FALSE)
#custom (1,0,1)
par(mfrow=c(2,2))

model_1<-auto.arima(train,seasonal=FALSE)
tsdisplay(residuals(model_1),lag.max = 40,main='(0,0,1) MODEL')



model_1<-auto.arima(train,seasonal=FALSE)
tsdisplay(residuals(model_1),lag.max = 40,main='(0,0,1) MODEL')
#arima based off acf pacf of training data 
model_2=arima(train,order=c(1,0,1))
tsdisplay(residuals(model_2),lag.max=40,main='(1,0,1) model')

model_3<-auto.arima(Cineworld_close,seasonal=FALSE)
tsdisplay(residuals(model_3),lag.max = 40,main='Original non- Log returns model')


model_4<-arima(Cineworld_close,order=c(1,0,1))
tsdisplay(residuals(model_4),lag.max = 40,main='Original non- Log returns (1,0,1) model')

par(mfrow=c(2,2))

period<-100
fcast1<-forecast(model_1,h=period)
plot(fcast1)
fcast2<-forecast(model_2,h=period)
plot(fcast2)
fcast3<-forecast(model_3,h=period)
plot(fcast3)
fcast4<-forecast(model_4,h=period)
plot(fcast4)

par(mfrow=c(1,2))
plot(fcast3)
plot(fcast4)

#check for mean squared error
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)
#test data not accurate, high MAPE,
fcast3
