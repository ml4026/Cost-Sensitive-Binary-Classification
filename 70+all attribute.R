set.seed(6690)
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

data <- na.omit(data)
data$Class <- as.factor(data$Class)

#Partition train 70% test 30%
sampleind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
data.train <- data[sampleind == 1,]
data.test <- data[sampleind == 2,]

#Decision Tree
library(tree)
model.tree <- tree(Class ~ .-Index, data = data.train)
#summary(model.tree)
plot(model.tree)
text(model.tree)
pred.tree <- predict(model.tree, newdata=data.test, type = "class")
table(pred.tree, data.test$Class)

#linear/radial SVM
library(e1071)
svmfit.linear <- svm(Class ~.-Index, data = data.train, kernel = "linear")
svmfit.radial <- svm(Class ~.-Index, data = data.train, kernel = "radial")
pred.svm.linear <- predict(svmfit.linear, newdata=data.test)
table(pred.svm.linear, data.test$Class)
pred.svm.radial <- predict(svmfit.radial, newdata=data.test)
table(pred.svm.radial, data.test$Class)

#Boosting
library(gbm)
Classi=ifelse(Class==2,0,1)
data=data.frame(data,Classi)
data.train.boost = data[sampleind == 1,]
data.test.boost = data[sampleind == 2,]
boost.fit=gbm(Classi~.-Index-Class,data=data.train.boost,distribution="bernoulli",n.trees=5000,interaction.depth=4)
boost.pred=predict(boost.fit,newdata=data.test.boost,n.trees=5000, type="response")
boost.predict_class=ifelse(boost.pred > 0.5,1,0)
table(boost.predict_class,data.test.boost$Class)

#Logistic Regression
attach(data)
#Classi=ifelse(Class==2,0,1)
#data=data.frame(data,Classi)
glm.fits=glm(Class~.-Index,data=data.train,family=binomial)
glm.pred=predict(glm.fits,newdata=data.test,type="response")
glm.predict_class <- ifelse(glm.pred>0.5,1,0)
table(glm.predict_class,data.test$Class)

#Bagging
library(randomForest)
#attach(data)
bag.fit=randomForest(Class~.-Index,data=data.train,mtry=10)
bag.pred = predict(bag.fit,newdata = data.test)
table(bag.pred,data.test$Class)

#LDA
library(MASS)
lda.fit  = lda(Class~.-Index, data = data.train)
#lda.fit
lda.pred = predict(lda.fit, data.test)
lda.class = lda.pred$class
table(lda.class,data.test$Class)

#QDA
qda.fit = qda(Class~.-Index, data = data.train)
qda.fit
qda.pred = predict(qda.fit, data.test)
qda.class = qda.pred$class
table(qda.class,data.test$Class)

#Random Forest
library(randomForest)
#attach(data)
bag.fit=randomForest(Class~.-Index,data=data.train,mtry=3)
bag.pred = predict(bag.fit,newdata = data.test)
table(bag.pred,data.test$Class)

#AdaBoost
#install.packages("ggplot2",dependencies=TRUE)
library(adabag)
library(ggplot2)
#attach(data)
data$Class <- as.factor(data$Class)
data.train$Class <- as.factor(data.train$Class)
data.test$Class <- as.factor(data.test$Class)
adaboost<-boosting(Class~.-Index, data=data.train, boos=TRUE, mfinal=20,coeflearn='Breiman')
#for(i in 1:20){
#  data.fit <- boosting(Class~., data=data, mfinal=i)
#  data.pred <- predict.boosting(data.fit,data = data)
#}
ada.pred = predict(adaboost,data.test)
ada.pred$confusion

