#Load data from website
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

#Omit rows with missing value?
data <- na.omit(data)
train = sample(1:nrow(data),478)
data.test = data[-train,]
data.train = data[train,]

#Logistic Regression
attach(data)
Classi=ifelse(Class==2,0,1)
data=data.frame(data,Classi)
glm.fits=glm(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data.train,family=binomial)
glm.pred=predict(glm.fits,data.test,type="response")
glm.predict_class <- ifelse(glm.pred>0.5,1,0)
table(glm.predict_class,data$Classi)


#Bagging
library(randomForest)
attach(data)
Classi=ifelse(Class==2,"0","1")
data=data.frame(data,Classi)
bag.fit=randomForest(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,mtry=10)
bag.pred = predict(bag.fit,data,type='class')
bag.predict_class=ifelse(bag.pred > 0.5,1,0)
table(bag.predict_class,data$Classi)



#Boosting
library(gbm)
#set.seed(1)
Classi=ifelse(Class==2,0,1)
data=data.frame(data,Classi)
boost.fit=gbm(Classi~Index+CT+CellSize+CellShape+MA+SE_CellSize+BN+BC+NN+Mitoses,data=data,distribution="bernoulli",n.trees=5000,interaction.depth=4)
boost.pred=predict(boost.fit,newdata=data,n.trees=5000, type="response")
boost.predict_class=ifelse(boost.pred > 0.5,1,0)
table(boost.predict_class,data$Classi)

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
data$Class <- as.factor(data$Class)
adaboost<-boosting(Class~., data=data, boos=TRUE, mfinal=20,coeflearn='Breiman')
#for(i in 1:20){
#  data.fit <- boosting(Class~., data=data, mfinal=i)
#  data.pred <- predict.boosting(data.fit,data = data)
#}
ada.pred = predict(adaboost,data)
ada.pred$confusion

data$Class <- as.factor(data$Class)

library(tree)
model.tree <- tree(Class ~ .-Index, data = data)
summary(model.tree)
plot(model.tree)
text(model.tree)
pred.tree <- predict(model.tree, data, type = "class")
table(data$Class, pred.tree)

library(e1071)
svmfit.linear <- svm(Class ~.-Index, data = data, kernel = "linear")
svmfit.radial <- svm(Class ~.-Index, data = data, kernel = "radial")
pred.svm.linear <- predict(svmfit.linear, data)
table(data$Class, pred.svm.linear)
pred.svm.radial <- predict(svmfit.radial, data)
table(data$Class, pred.svm.radial)

library(neuralnet)
data4nn <- data
data4nn$Class <- as.numeric(data4nn$Class) - 1
f <- as.formula(paste("Class ~", paste(attr.names[!attr.names %in% c("Index", "Class")], collapse = "+")))
nnfit <- neuralnet(f, data = data4nn, hidden = 64, linear.output = FALSE)
pred.nn.compute <- compute(nnfit, data4nn[, 2:10])
pred.nn.logit <- pred.nn.compute$net.result
pred.nn <- ifelse(pred.nn.logit > 0.5, 4, 2)
table(data$Class, pred.nn)
