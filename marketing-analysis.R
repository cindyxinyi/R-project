#part 1
#a) Load the dataset and the forecast package.
##load data
library(readxl)
Data <- read.csv("Documents/rochester winter/marketing analyti using r/homework/hw3/Homework 3 Student Data.csv")
View(Data)
##install forecast package
install.packages('forecast')
library('forecast')

#b) Get the total sales by week by aggregating the initial dataset. 
    #Set aside the last 52 weeks of the dataset as a holdout sample. 
    #Convert this total sales by week into a time series object using the ts function. 
    #Set freq=52,to specify that seasonal periods repeat themselves every 52 weeks.


# clean data
total_sales_by_week=aggregate(totalCost ~ overallWeekNum, data=Data, sum)
holdout=tail(total_sales_by_week,52)
train=total_sales_by_week[1:(nrow(total_sales_by_week)-52),]
train=as.ts(train,frequency=52)
tsy=ts(train[,2],frequency=52)

#c) Plot the time series. Then, use the acf function to plot the autocorrelations
par(mfrow=c(1,2))
plot(tsy)
acf(tsy)


  #d) Using the Arima function, to estimate an ARMA model with degree (3, 2) and a drift term.
arma1=Arima(tsy,order=c(3,0,2),include.drift=TRUE)

#e) Using the auto.arima function
aarma1=auto.arima(tsy,D=0,seasonal=FALSE,approximation = FALSE,stepwise=FALSE)
aarma2=auto.arima(tsy,D=0,seasonal=TRUE,approximation = FALSE,stepwise=FALSE)
aarma1
#f) Compare the performance of the two models in question 1e by
#calculate AIC
AIC(aarma1)
AIC(aarma2)
#using hold out sample calculate mse

predictedValues1=forecast(aarma1,h=52)$mean
predictedValues2=forecast(aarma2,h=52)$mean

#calculate mse
mse1=mean((holdout[,2]-predictedValues1)^2)^.5
mse2=mean((holdout[,2]-predictedValues2)^2)^.5
mse1
mse2
#plot 
plot(forecast(aarma1,h=52))
plot(forecast(aarma2,h=52))
#ci
confint(aarma1,'ma1')
confint(aarma2,'ma1')

#confidence interval(compute confidence interval)
#compute seasonal prediction interval
seasonupper=as.numeric(forecast(aarma2,h=52)$upper[,2])
seasonlower=as.numeric(forecast(aarma2,h=52)$lower[,2])
#compute no seasonal prediction interval
noseaonupper=as.numeric(forecast(aarma1,h=52)$upper[,2])
noseasonlower=as.numeric(forecast(aarma1,h=52)$lower[,2])
#compute mean
means=mean(seasonupper-seasonlower)
meann=mean(noseaonupper-noseasonlower)
means
meann
#part2 Attenuation Bias
#a) Calculate the Price per Can
Price_per_Can=Data$totalCost/Data$units
#b) Run a regression
LM1=lm(log(units)~Price_per_Can,data=Data)
#c)
Data2=aggregate(cbind(units,totalCost)~productNum+overallWeekNum,data=Data,FUN = mean)
Data2["Price_per_Can"]=Data2$totalCost/Data2$units
LM2=lm(log(units)~Price_per_Can,data=Data2)

#d)
Data3=aggregate(cbind(units,totalCost)~overallWeekNum,data=Data,FUN = mean)
Data3["Price_per_Can"]=Data3$totalCost/Data3$units
LM3=lm(log(units)~Price_per_Can,data=Data3)


#part 3 Omitted Variable Bias
#a) Calculate the Price per Can
Price_per_Can=Data$totalCost/Data$units

#b) Run a regression
LM1=lm(log(units)~Price_per_Can,data=Data)

#c) Repeat the regression in part 3b, but control for isFeature.
LM4=lm(log(units)~Price_per_Can+isFeature,data=Data)

#d) Repeat the regression in part 3c, but control for isDisplay.
LM5=lm(log(units)~Price_per_Can+isFeature+isDisplay,data=Data)

#e) Repeat the regression in part 3d, but control for the storeNum.
LM6=lm(log(units)~Price_per_Can+isFeature+isDisplay+as.factor(storeNum),data=Data)

#f) Repeat the regression in part 3e, but control for the productNum
LM7=lm(log(units)~Price_per_Can+isFeature+isDisplay+as.factor(storeNum)+as.factor(productNum),data=Data)

#g) Repeat the regression in part 3f, but control for the weekInYearNum as a factor variable.
LM8=lm(log(units)~Price_per_Can+isFeature+isDisplay+as.factor(storeNum)+as.factor(productNum)+as.factor(weekInYearNum),data=Data)

