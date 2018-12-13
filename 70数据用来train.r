attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

#Omit rows with missing value?
data <- na.omit(data)

set.seed(1)
train = sample(1:nrow(data),478)
data.test = data[-train,]
data.train = data[train,]

#LDA
library(MASS)
lda.fit  = lda(Class~., data = data.train)
#lda.fit
lda.pred = predict(lda.fit, data.test)
lda.class = lda.pred$class
table(lda.class,data.test$Class)

#QDA
qda.fit = qda(Class~., data = data.train)
qda.fit
qda.pred = predict(qda.fit, data.test)
qda.class = qda.pred$class
table(qda.class,data.test$Class)

#Random Forest
library(randomForest)
set.seed(1)
attach(data)
Classi=ifelse(Class<=2,"No","Yes")
data=data.frame(data,Classi)
data.test = data[-train,]
data.train = data[train,]
bag.fit=randomForest(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data.train,mtry=3)
bag.pred = predict(bag.fit,newdata = data.test)
table(bag.pred,data.test$Classi)

#AdaBoost
#install.packages("ggplot2",dependencies=TRUE)
library(adabag)
library(ggplot2)
set.seed(1)
attach(data)
data$Class <- as.factor(data$Class)
data.train$Class <- as.factor(data.train$Class)
data.test$Class <- as.factor(data.test$Class)
adaboost<-boosting(Class~., data=data.train, boos=TRUE, mfinal=20,coeflearn='Breiman')
#for(i in 1:20){
#  data.fit <- boosting(Class~., data=data, mfinal=i)
#  data.pred <- predict.boosting(data.fit,data = data)
#}
ada.pred = predict(adaboost,data.test)
ada.pred$confusion


