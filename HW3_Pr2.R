### HW3 Problem-2:Preventing Hospital Readmissions
library(caTools)
library(ROCR)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

###Part-A
readmission=read.csv("readmission.csv")
summary(readmission)
str(readmission)
table(readmission$readmission)
11357/(11357+90409)
table(readmission$gender)
54708/(47055+54708)

###Part-B
p=0.13333
p*25000
(0.7*p)*26000+(1-0.7*p)*1000
LFP=1
LFN=(1-p)/p
LFN

###Part-C
set.seed(144)
idx = sample.split(readmission$readmission, 0.65)
readm.train = readmission[idx,]
readm.test = readmission[!idx,]
train = readmission[idx,]
test = readmission[!idx,]

table(train$readmission)
table(test$readmission)

#tree1 = rpart(readmission ~ ., data=train, method="class", cp=0.002, minbucket=1)
#tree2 = rpart(readmission ~ ., data=train, method="anova", minbucket = 25, cp=0.002)
#tree3 = rpart(readmission ~ ., data=train, method="class", parms=list(split="information", loss=matrix(c(0,1,6.5,0), byrow=TRUE, nrow=2)))
#tree4 = rpart(readmission ~ ., data=train, method="class",cp=0.002, parms=list(split="information", loss=matrix(c(0,1,6.5,0), byrow=TRUE, nrow=2)))

tree5 = rpart(readmission ~ ., data=train, method="class",cp=0.002, minbucket=25, parms=list(split="information", loss=matrix(c(0,1,6.5,0), byrow=TRUE, nrow=2)))
prp(tree5)

summary(tree5)
tree5

PredictTest = predict(tree5, newdata = test, type="class")
PredictTrain = predict(tree5, newdata = train, type="class")

table(test$readmission,PredictTest)
7070+1594
cost1=8664*1000
0.3*1594
savings1=478.2*25000-8864000
savings1
478.2*25000
2381+1594
25000*3975
8864000 + (1594*0.7)*25000 + 2381*25000
99375000 - 96284000

###Part-D:
c=1348.71
8864*c + (1594*0.7)*25000 + 2381*25000  
e=0.23
8864*1000 + 1594*(1-e)*25000 + 2381*25000 


###PART-E
tree6 = rpart(readmission ~ ., data=train, method="class",cp=0.002, minbucket=25, parms=list(split="information", loss=matrix(c(0,1,3.65,0), byrow=TRUE, nrow=2)))

PredictTrain6 = predict(tree6, newdata = train, type="class")
table(train$readmission,PredictTrain6)

PredictTest6 = predict(tree6, newdata = test, type="class")
table(test$readmission,PredictTest6)

(2588+1044)/(2588+1044+56178+6338)

###Part-F
logisticmodel1 = glm(readmission ~., data=train, family="binomial")
summary(logisticmodel1)

PredictTest2=predict(logisticmodel1, newdata = test, type="response")
table(test$readmission,PredictTest2 > 0.133333)

22182+5188
2238+1226

