### Problem-2: Framingham Heart Study
### Part-a: Memo to a Policy Maker

### Part-a: i-ii-iii.)
framingham= read.csv("framingham.csv")
library(caTools)
library(ROCR)
library(dplyr)
library(ggplot2)
set.seed(144)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.67)
table(split)
train = framingham[split == TRUE,]
test = framingham[split == FALSE,]
table(train$TenYearCHD)
table(train$TenYearCHD)/nrow(train)
model1 = glm(TenYearCHD ~., data=train, family="binomial")
summary(model1)

### Part-a: iv.) Test-set performance
PredictTrain=predict(model1, newdata = train, type="response")
table(train$TenYearCHD,PredictTrain > 0.0734)
PredictTest=predict(model1, newdata = test, type="response")
table(test$TenYearCHD,PredictTest > 0.0734)
table(train$TenYearCHD,PredictTrain > 0.0734)/nrow(train)
table(test$TenYearCHD,PredictTest > 0.0734)/nrow(test)
table(test$TenYearCHD,PredictTest > 0.0734)
Accuracy=(342+165)/(342+165+19+681)
Accuracy
TPR=165/(165+19)
TPR
FPR=681/(681+342)
FPR

### Part-a: v.) Baseline Model
summary(test$TenYearCHD==1)
summary(test$TenYearCHD==0)
BaselineAccuracy=1023/(184+1023)
BaselineAccuracy


p=0.0734
p/2.3
(1-p/2.3)

### Part-a: vii.) Prediction on New Patient
play.obs = data.frame(male=1, age=57, education="College",currentSmoker=1, cigsPerDay=15, BPMeds=0, prevalentStroke=0,
                      prevalentHyp=1 ,diabetes=0, totChol=220, sysBP=140, diaBP=100, BMI=32, heartRate=63, glucose=81)

predict(model1, newdata=play.obs, type="response")

### Part-b: ROC Curve ####
ROCRpred = prediction(PredictTest,test$TenYearCHD)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
abline(0,1)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

### Part-e: Updated Logistic Regression Model with 3 variables ###
model2 = glm(TenYearCHD ~ male + age + cigsPerDay, data=train, family="binomial")
summary(model2)

PredictTrain2=predict(model2, newdata = train, type="response")
table(train$TenYearCHD,PredictTrain2 > 0.0734)
table(train$TenYearCHD,PredictTrain2 > 0.0734)/nrow(train)

PredictTest2=predict(model2, newdata=test, type="response")
table(test$TenYearCHD, PredictTest2 > 0.0734)
table(test$TenYearCHD, PredictTest2 > 0.0734)/nrow(test)

New_Accuracy=0.20712510 + 0.13918807
New_Accuracy
New_TPR=168/(168+16)
New_TPR
New_FPR=773/(773+250)
New_FPR
