#Load data from website
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

data <- na.omit(data)
data$Class <- as.factor(data$Class)

#Decision Tree
library(tree)
model.tree <- tree(Class ~ .-Index, data = data)
summary(model.tree)
plot(model.tree)
text(model.tree)
pred.tree <- predict(model.tree, data, type = "class")
table(data$Class, pred.tree)

#linear/radial SVM
library(e1071)
svmfit.linear <- svm(Class ~.-Index, data = data, kernel = "linear")
svmfit.radial <- svm(Class ~.-Index, data = data, kernel = "radial")
pred.svm.linear <- predict(svmfit.linear, data)
table(data$Class, pred.svm.linear)
pred.svm.radial <- predict(svmfit.radial, data)
table(data$Class, pred.svm.radial)

#neural networks
library(neuralnet)
data4nn <- data
data4nn$Class <- as.numeric(data4nn$Class) - 1
f <- as.formula(paste("Class ~", paste(attr.names[!attr.names %in% c("Index", "Class")], collapse = "+")))
nnfit <- neuralnet(f, data = data4nn, hidden = 64, linear.output = FALSE)
pred.nn.compute <- compute(nnfit, data4nn[, 2:10])
pred.nn.logit <- pred.nn.compute$net.result
pred.nn <- ifelse(pred.nn.logit > 0.5, 4, 2)
table(data$Class, pred.nn)
