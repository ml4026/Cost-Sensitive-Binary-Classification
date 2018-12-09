attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

#Omit rows with missing value?
data <- na.omit(data)

#LDA
library(MASS)
lda.fit  = lda(Class~., data = data)
lda.fit
lda.pred = predict(lda.fit, data)
lda.class = lda.pred$class
table(lda.class,data$Class)

#QDA
qda.fit = qda(Class~., data = data)
qda.fit
qda.pred = predict(qda.fit, data)
qda.class = qda.pred$class
table(qda.class,data$Class)

#Random Forest
library(randomForest)
set.seed(1)
#attach(data)
Classi=ifelse(Class<=2,"No","Yes")
data=data.frame(data,Classi)
bag.fit=randomForest(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,mtry=3)
bag.pred = predict(bag.fit,data)
table(bag.pred,data$Classi)

#AdaBoost
#install.packages("ggplot2",dependencies=TRUE)
library(adabag)
library(ggplot2)
set.seed(1)
data$Class <- as.factor(data$Class)
adaboost<-boosting(Class~., data=data, boos=TRUE, mfinal=20,coeflearn='Breiman')
#for(i in 1:20){
#  data.fit <- boosting(Class~., data=data, mfinal=i)
#  data.pred <- predict.boosting(data.fit,data = data)
#}
ada.pred = predict(adaboost,data)
ada.pred$confusion


