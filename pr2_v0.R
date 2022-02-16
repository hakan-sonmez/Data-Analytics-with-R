wranglerelantra = read.csv("wranglerElantra.csv")
str(wranglerelantra)
train = subset (wranglerelantra, Year <= 2015)
test = subset (wranglerelantra, Year > 2015)
str(train)
str(test)
model1 = lm(WranglerSales ~ Year + Unemployment + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model1)
model2 = lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model2)
onedata1 = data.frame(Unemployment,WranglerQueries)
cor(onedata[,c("WranglerSales", "Year", "Unemployment", "WranglerQueries","CPI.All", "CPI.Energy")])
Year = c(wranglerelantra$Year)
WranglerSales = c(wranglerelantra$WranglerSales)
Unemployment = c(wranglerelantra$Unemployment)
WranglerQueries=c(wranglerelantra$WranglerQueries)
CPI.All = c(wranglerelantra$CPI.All)
CPI.Energy = c(wranglerelantra$CPI.Energy)
onedata=data.frame(WranglerSales,Year,Unemployment,WranglerQueries,CPI.All, CPI.Energy)
cor(onedata[,c("WranglerSales", "Year", "Unemployment", "WranglerQueries","CPI.All", "CPI.Energy")])
model3 = lm(WranglerSales ~ Year  + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model3)
model4 = lm(WranglerSales ~ Year + WranglerQueries + CPI.All, data=train)
summary(model4)
model5 = lm(WranglerSales ~ WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model5)
model6 = lm(WranglerSales ~ Year + WranglerQueries + CPI.All, data=train)
summary(model6)
model7 = lm(WranglerSales ~ WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model7)
model8 = lm(WranglerSales ~ Year  + WranglerQueries + CPI.Energy, data=train)
summary(model8)
model9 = lm(WranglerSales ~ WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model9)
model1 = lm(WranglerSales ~ Year + Unemployment + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model1)
model10 = lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy, data=train)
summary(model10)
summary(model3)
model11 = lm(WranglerSales ~ Year + MonthFactor + Unemployment + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model11)
model12 = lm(WranglerSales ~ Year + MonthFactor + WranglerQueries + CPI.All + CPI.Energy, data=train)
summary(model12)
model13 = lm(WranglerSales ~ Year + MonthFactor + WranglerQueries + CPI.All + CPI.Energy, data=train)

model21 = lm(ElantraSales ~ Year + Unemployment + ElantraQueries + CPI.All + CPI.Energy, data=train)
summary(model21)
model22 = lm(ElantraSales ~ Year + ElantraQueries + CPI.All + CPI.Energy, data=train)
summary(model22)
model22 = lm(ElantraSales ~ Year + Unemployment + ElantraQueries + CPI.All, data=train)
summary(model22)
model23 = lm(ElantraSales ~ Year + ElantraQueries + CPI.All, data=train)
summary(model23)
model24 = lm(ElantraSales ~ Year + ElantraQueries, data=train)
summary(model24)
model25 = lm(ElantraSales ~ ElantraQueries + CPI.All, data=train)
summary(model25)
ElantraPredictions = predict(model25, newdata=test)
summary(ElantraPredictions)
str(ElantraPredictions)
SSE = sum((ElantraPredictions-test$ElantraSales)^2)
SSE
SST = sum((test$ElantraSales-mean(train$ElantraSales))^2)
SST
R2=1-SSE/SST
R2
train1 = subset (wranglerelantra, MonthFactor == "January" | MonthFactor == "February" | MonthFactor == "May" | MonthFactor == "September" | MonthFactor == "October" | MonthFactor == "November", Year <= 2015)
train1
model12
model121 = lm(WranglerSales ~ Year + MonthFactor + WranglerQueries + CPI.All + CPI.Energy, data=train1)
summary(model121)

WranglerPredictions = predict(model12, newdata=test)
summary(WranglerPredictions)
str(WranglerPredictions)
SSE = sum((WranglerPredictions-test$WranglerSales)^2)
SSE
SST = sum((test$WranglerSales-mean(train$WranglerSales))^2)
SST
R2=1-SSE/SST
R2
MAE=sum(abs(WranglerPredictions-test$WranglerSales))
MAE
WranglerPredictions-test$WranglerSales
MAE=sum(abs(WranglerPredictions-test$WranglerSales))/12
MAE
RMSE=sqrt(sum((WranglerPredictions-test$WranglerSales)^2)/12)
RMSE
