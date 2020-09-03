
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)



# download DAX data from Yahoo


#auto.assign=FALSE puts cineworld into a data frame

Stock_1<-getSymbols("CINE.L",auto.assign = FALSE)


Stock_1<-tail(Stock_1,100)
Stock_1
#not time series plot,
plot(Stock_1)
#plots time series chart
plot(Stock_1$CINE.L.Close)

#daily returns
dailyR_Stock1<-dailyReturn(Stock_1$CINE.L.Close)
plot(dailyR_Stock1)

#weekly returns
weeklyR_Stock1<-weeklyReturn(Stock_1$CINE.L.Close)
plot(weeklyR_Stock1)


#monthly returns
monthlyR_Stock1<-monthlyReturn(Stock_1$CINE.L.Close)
plot(monthlyR_Stock1)

#distribution of returns
hist(dailyR_Stock1)
hist(weeklyR_Stock1)
hist(monthlyR_Stock1,freq =40)
#summary, min return max ret etc
summary(dailyR_Stock1)

#most recent closing price valuevalue
value1<-last(Stock_1$CINE.L.Close)
value1

# bootstrapping
plot(NULL,xlim=c(2020.4,2020.9)
     ,ylim=c(20,200),
     xlab="Time",
     ylab="Cineworld Value")
abline(h=value1,col="blue",lty=2)
#lower limit, where you bought the stock
abline(h=48,col="red",lty=2)

#Random sample from empirical distribution of returns
#as.vector is needed as returns is in time series
#12 samples, replace=TRUE gives us equal probability at each time
for(i in 1:1000){
  return1<-sample(as.vector(weeklyR_Stock1),12,replace=TRUE)
  #1+ to make into real value
  return2<-1+return1
  #compounding is the reason for cumprod
  return3<-cumprod(return2)
  #turn back into actual value
  value2<-return3*as.numeric(value1)
  #turn it into a time series
  #c(2020,x), x is the week to begin at 
  value3<-ts(value2,start=c(2020,40),frequency=52)
  lines(value3)
}
#sample returns
return1
#now numbers near 1, below 1 is loosing money, above is gaining
return2
#compounded
return3
value2

value3<-ts(value2,start=c(2020,20),frequency=52)
value3
