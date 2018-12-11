#Load data from website
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

#Omit rows with missing value?
data <- na.omit(data)


#Logistic Regression
set.seed(11)
Classi=ifelse(Class==2,0,1)
data=data.frame(data,Classi)
glm.fits=glm(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,family=binomial)
glm.pred=predict(glm.fits,type="response")
glm.predict_class <- ifelse(glm.pred>0.5,1,0)
table(result,data$Classi)


#Bagging
library(randomForest)
set.seed(22)
#attach(data)
Classi=ifelse(Class==2,"0","1")
data=data.frame(data,Classi)
bag.fit=randomForest(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,mtry=10)
bag.pred = predict(bag.fit,data)
bag.predict_class=ifelse(bag.pred > 0.5,1,0)
table(bag.predict_class,data$Classi)



#Boosting
set.seed(1)
library(gbm)
#set.seed(1)
Classi=ifelse(Class==2,0,1)
data=data.frame(data,Classi)
boost.fit=gbm(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,distribution="bernoulli",n.trees=5000,interaction.depth=4)
boost.pred=predict(boost.fit,newdata=data,n.trees=5000, type="response")
boost.predict_class=ifelse(boost.pred > 0.5,1,0)
table(boost.predict_class,data$Classi)
