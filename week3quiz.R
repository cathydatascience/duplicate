# q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)
#inTrain<-createDataPartition(y=segmentationOriginal$Class, p=0.7, list=FALSE)
inTrain<-segmentationOriginal$Case
training<-segmentationOriginal[inTrain=="Train", ]
testing<-segmentationOriginal[inTrain=="Test", ]
dim(training); dim(testing)

modFit<-train(Class~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE, main="Classification Tree") # dendogram
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

# q3
library(pgmm)
data(olive)
olive = olive[,-1]

modFit<-train(Area~., method="rpart", data=olive)
print(modFit$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata=newdata)

# q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modFit<-glm(as.factor(chd)~age+alcohol+obesity+tobacco+typea+ldl, family=binomial, data=trainSA)
summary(modFit)

predict1<-predict(modFit, newdata=testSA)
predict2<-predict(modFit, newdata=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, predict1)
missClass(trainSA$chd, predict2)

# q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

set.seed(33833)

modFit<-train(y~., data=vowel.train, method="rf", prox=TRUE)
modFit$finalModel$importance

modFit2<-randomForest(y~., data=vowel.train, importance=TRUE)
importance(modFit2)
varImp(modFit2)



