library(adabag)
library(ggplot2)
set.seed(6690)
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

data <- na.omit(data)
data$Class <- as.factor(data$Class)

#Partition train 70% test 30%
sampleind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
data.train <- data[sampleind == 1,]
data.val <- data[sampleind == 2,]




#Initialization
cost.test <- runif(9, min = 0, max = 10) #Test cost of each attribute
cost.test <- c(0, cost.test)
cost.FP <- 1000
cost.FN <- 1000
A.known <- c() #Used attributes
A.unknown <- 2:10 #Unused attributes
cost.exp.test <- 0 #Cumulative test cost
funcstr <- "Class ~" #function string
cost.exp <- rep(0, 9) #Record experiment cost with 1-9 attributes

for (i in 1:9){
  cost.min <- max(cost.FP, cost.FN) * nrow(data.val) * 2 #A large number
  cost.argmin <- 0
  for (ind in A.unknown){
    fs.temp <- paste(funcstr, "+", attr.names[ind]) #Temp function string
    func <- as.formula(fs.temp)
    model.temp <- boosting(func, data = data.train, boos=TRUE, mfinal=20,coeflearn='Breiman') #Fit model
    pred.temp <- predict(model.temp, data.val, type = "class") #Predict labels
    fp <- sum(pred.temp$class == 2 & data.val$Class == 4) #False positive
    fn <- sum(pred.temp$class == 4 & data.val$Class == 2) #False negative
    cost.temp <- cost.test[ind] * nrow(data.val) + fp * cost.FP + fn * cost.FN
    if (cost.temp < cost.min){
      cost.min <- cost.temp
      cost.argmin <- ind
    }
  }
  cost.exp[i] <- cost.exp.test + cost.min # Previous test + current test
  cost.exp.test <- cost.exp.test + cost.test[cost.argmin] * nrow(data.val)
  funcstr <- paste(funcstr, "+", attr.names[cost.argmin])
  A.known <- c(A.known, cost.argmin) #Include new test in the known attribute
  A.unknown <- A.unknown[A.unknown != cost.argmin] #Exclude new test in the unknown attribute
}
cost.exp <- cost.exp / nrow(data.val)
cost.exp
num <- 1:9
plot(num,cost.exp,type="o",xlab="Number of Attributes",ylab="Cost",lwd=2,main="Cost of AdaBoost")
axis(1,at=seq(1,10))
