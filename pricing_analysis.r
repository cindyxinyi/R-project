
#Part 1: Pricing with LM
#load data
Data <- read.csv("Documents/rochester winter/marketing analyti using r/homework/hw4/Homework 4 Student Data.csv")

#a)find the most popular productNum
Data1=aggregate(units ~ productNum,data=Data,FUN=sum)
most_popular_productNum=Data1$productNum[which.max(Data1$units)]
most_popular_productNum
#b)establish upcFile
upcFile=Data[Data$productNum==most_popular_productNum,]
#c)
aggUPCFile = aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)
aggUPCFile$pricePerCan = aggUPCFile$totalCost/aggUPCFile$units
model1 = lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile)
summary(model1)
#d)
possiblePrices = data.frame(price = seq(0,10,.01))
possiblePrices$demand = NA
newData = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)
#e)calculate predict demand
demand=exp(predict(model1,newData))
#combine demand to a new dataset
newData1=newData
newData1$demand=demand

#f)calculate expected profit
expected_profit=demand*(newData$pricePerCan-0.3)
#combine expected_profit  to a new data set
newData1$expected_profit=expected_profit

#g) optimal price and optimal profit
optimal_price=newData$pricePerCan[which.max(expected_profit)]
optimal_profit=newData1$expected_profit[which.max(expected_profit)]
optimal_profit
optimal_price
#h)
#create a new model
model2 = lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)
summary(model2)
#calculate predict demand
demand2=exp(predict(model2,newData))
#combine demand to a new dataset
newData2=newData
newData2$demand=demand2
#calculate expected profit
expected_profit2=demand2*(newData$pricePerCan-0.3)
#combine expected_profit  to a new data set
newData2$expected_profit2=expected_profit2
# optimal price and optimal profit
optimal_price2=newData$pricePerCan[which.max(expected_profit2)]
optimal_profit2=newData2$expected_profit2[which.max(expected_profit2)]
optimal_profit2
optimal_price2

#Part 2: Pricing with nnet
#a)
library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
#b) 
newData3=newData[newData$pricePerCan==0.5,]
newData4=newData[newData$pricePerCan==1,]
demand3=exp(predict(nnet1 ,newData3))
demand4=exp(predict(nnet1 ,newData4))
demand5=exp(predict(nnet2 ,newData3))
demand6=exp(predict(nnet2 ,newData4))

#c)
#nnet1
#calculate predict demand
demandn1=exp(predict(nnet1,newData))
#combine demand to a new dataset
newDatan1=newData
newDatan1$demandn1=demandn1
#calculate expected profit
expected_profitn1=demandn1*(newData$pricePerCan-0.3)
#combine expected_profit  to a new data set
newDatan1$expected_profitn1=expected_profitn1
# optimal price and optimal profit
optimal_pricen1=newData$pricePerCan[which.max(expected_profitn1)]
optimal_profitn1=newDatan1$expected_profitn1[which.max(expected_profitn1)]
optimal_profitn1
optimal_pricen1

#nnet2
#calculate predict demand
demandn2=exp(predict(nnet2,newData))
#combine demand to a new dataset
newDatan2=newData
newDatan2$demandn2=demandn2
#calculate expected profit
expected_profitn2=demandn2*(newData$pricePerCan-0.3)
#combine expected_profit  to a new data set
newDatan2$expected_profitn2=expected_profitn2
# optimal price and optimal profit
optimal_pricen2=newData$pricePerCan[which.max(expected_profitn2)]
optimal_profitn2=newDatan2$expected_profitn2[which.max(expected_profitn2)]
optimal_profitn2


#d)
#plot the original model 
plot(newDatan1$pricePerCan,expected_profitn1)

newDatan11=newData[newData$pricePerCan>min(aggUPCFile$pricePerCan) & newData$pricePerCan<max(aggUPCFile$pricePerCan), ]
#calculate predict demand
demandn11=exp(predict(nnet1,newDatan11))
#combine demand to a new dataset
newDatan111=newDatan11
newDatan111$demandn11=demandn11
#calculate expected profit
expected_profitn11=demandn11*(newDatan11$pricePerCan-0.3)
#combine expected_profit  to a new data set
newDatan111$expected_profitn11=expected_profitn11
# optimal price and optimal profit
optimal_pricen11=newDatan11$pricePerCan[which.max(expected_profitn11)]
optimal_profitn11=newDatan111$expected_profitn11[which.max(expected_profitn11)]
optimal_profitn11
optimal_pricen11








