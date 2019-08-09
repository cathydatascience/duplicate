## q1

library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

## q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(CompressiveStrength, row.names(training), colour=Age, data=training)
library(Hmisc)
cutAge<-cut2(training$Age, g=4)
qplot(CompressiveStrength, row.names(training), colour=cutAge, data=training)

## q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
summary(training$Superplasticizer)
hist(log10(training$Superplasticizer+1))

## q4 & q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
trainingconfusionMatrix(testing$diagnosis, predict(mdfit1, testing)) = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL<-training[, c(grep("^IL", names(training)), 1)]
testingIL<-testing[, c(grep("^IL", names(testing)), 1)]

# q4
preProc<-preProcess(trainingIL[, -13], method="pca", thresh = 0.9)

#q5
mdfit1<-train(diagnosis~., method="glm", data=trainingIL)
confusionMatrix(testing$diagnosis, predict(mdfit1, testing))

mdfit2<-train(diagnosis~., method="glm", preProcess="pca", data=trainingIL)
confusionMatrix(testingIL$diagnosis, predict(mdfit2, testingIL))

#an alternative
preProc<-preProcess(trainingIL[, -13], method="pca", thresh = 0.8)
trainPC<-predict(preProc, trainingIL[, -13])
mdfit2<-train(x=trainPC, y=trainingIL$diagnosis, method="glm")

testPC<-predict(preProc, testingIL[, -13])
confusionMatrix(testingIL$diagnosis, predict(mdfit2, testPC))




