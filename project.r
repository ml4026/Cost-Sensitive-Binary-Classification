data_origin <- read.csv(file.choose(), header = F)
data <- data_origin[,-1]
names(data) <- c("CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
#remove missing data
data<- na.omit(data)

#a <- array(0, dim=c(9,10,2)) 
library(plyr)
b = ddply(data,.(CT),summarize,number=length(CT))

# Partition data - train 70%, test 30%
#set.seed(511)
#ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
#train <- data[ind ==1,]
#test <- data[ind ==2,]
