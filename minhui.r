#Load data from website
attr.names <- c("Index","CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F,
                 col.names = attr.names, na.strings = "?")

#Omit rows with missing value?
data <- na.omit(data)

P.VmCk <- array(0, dim=c(9,10,2))
N.Ck <- c(sum(data[,11] == 2), sum(data[,11] == 4))

#Train by calculating conditional probabilities
for (ck in 1:2){
  for (feature in 2:(ncol(data)-1)){
    for (v in 1:10){
      P.VmCk[feature - 1, v, ck] <- sum(data[,feature] == v & data[,11] == ck * 2) / N.Ck[ck]
    }
  }
}

u = 0 #uncertainty
data.real <- data #Real data
cost.test <- runif(9, min = 0, max = 100) #Test cost of each attribute
cost.FP <- 600
cost.FN <- 1000
A.known <- c()
A.unknown <- 2:10
cost.exp.test <- 0

for (j in 1:nrow(data)){
  #Pij
  P.ij <- array(0, dim=c(9,10))
  for (attrib in 1:9){
    for (v in 1:10){
      P.ij[attrib, v] <- u / 8
    }
    P.ij[attrib, data[j, attrib + 1]] <- 1 - u
  }
  
  while (length(A.unknown) > 0){
    #Calculate Util
    l <- length(A.unknown)
    util <- rep(0, l)
    cost.exp.unknown <- rep(0, l)
    for (i in 1:l){
      
      P.ACk <- c(1, 1)
      P.AACk <- c(1, 1)
      for (ck in 1:2){
        for (attrib in A.known){
          P.AiCk <- sum(P.ij[attrib,] * P.VmCk[attrib,,ck])
          P.ACk[ck] <- P.ACk[ck] * P.AiCk 
        }
        P.AuCk <- sum(P.ij[A.unknown[i] - 1,] * P.VmCk[A.unknown[i] - 1,,ck])
        P.AACk[ck] <- P.ACk[ck] * P.AuCk
      }
      P.CkA <- c(1, 1)
      P.CkAA <- c(1, 1)
      P.A <- sum(P.ACk * N.Ck) / nrow(data)
      P.AA <- sum(P.AACk * N.Ck) / nrow(data)
      for (ck in 1:2){
        P.CkA[ck] <- P.ACk[ck] * N.Ck[ck] / nrow(data) / P.A
        P.CkAA[ck] <- P.AACk[ck] * N.Ck[ck] / nrow(data) / P.AA
      }
      R.known <- c(0, 0)
      R.unknown <- c(0, 0)
      R.known[1] <- cost.FP * P.CkA[2]
      R.known[2] <- cost.FN * P.CkA[1]
      cost.exp.known <-min(R.known)
      R.unknown[1] <- cost.FP * P.CkAA[2]
      R.unknown[2] <- cost.FN * P.CkAA[1]
      cost.exp.unknown[i] <-min(R.unknown)
      util[i] <- cost.exp.known - cost.exp.unknown[i] - cost.test[A.unknown[i]]
    }
    max.arg <- which.max(util) #Temporary index of argmax
    max.util <- util[max.arg]
    
    if (max.util <= 0){
      break
    }
    else{
      max.attr <- A.unknown[max.arg] #Select attribute to test
    }
    cost.exp.test <- cost.exp.test + cost.test[max.attr] #Accumulate test cost
    A.known <- c(A.known, max.attr) #Include new test in the known attribute
    A.unknown <- A.unknown[A.unknown != max.attr] #Exclude new test in the unknown attribute
  }
}

 

