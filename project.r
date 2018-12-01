data_origin <- read.csv(file.choose(), header = F)
data <- data_origin[,-1]
names(data) <- c("CT","CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class")
#remove missing data
data<- na.omit(data)

PC_vm_ck <- array(0, dim=c(9,10,2)) 
attributes = data.frame(attribute=c('CT',"CellSize","CellShape","MA","SE_CellSize","BN","BC","NN","Mitoses","Class"),
                    stringsAsFactors=F)

library(plyr)

# A = array(0, dim=c(9,20,3)) 
# A[1,,] = ddply(data,.(CT,Class),summarize,number=length(CT))
# A[2,,] = ddply(data,.(CellSize,Class),summarize,number=length(CellSize))
# A[3,,] = ddply(data,.(CellShape,Class),summarize,number=length(CellShape))
# A[4,,] = ddply(data,.(MA,Class),summarize,number=length(MA))
# A[5,,] = ddply(data,.(SE_CellSize,Class),summarize,number=length(SE_CellSize))
# A[6,,] = ddply(data,.(BN,Class),summarize,number=length(BN))
# A[7,,] = ddply(data,.(BC,Class),summarize,number=length(BC))
# A[8,,] = ddply(data,.(NN,Class),summarize,number=length(NN))
# A[9,,] = ddply(data,.(Mitoses,Class),summarize,number=length(Mitoses))
# for (j in 1:9){
#   for (i in 1:nrow(A[j,i])){
#   PC_vm_ck[j, A[j,i,1], A[j,i,2]/2] = A[j,i,3]
#   }
# }


A1 = ddply(data,.(CT,Class),summarize,number=length(CT))
for (i in 1:nrow(A1)){
  PC_vm_ck[1, A1[i,1], A1[i,2]/2] = A1[i,3]
}

A2 = ddply(data,.(CellSize,Class),summarize,number=length(CellSize))
for (i in 1:nrow(A2)){
  PC_vm_ck[2, A2[i,1], A2[i,2]/2] = A2[i,3]
}

A3 = ddply(data,.(CellShape,Class),summarize,number=length(CellShape))
for (i in 1:nrow(A3)){
  PC_vm_ck[3, A3[i,1], A3[i,2]/2] = A3[i,3]
}

A4 = ddply(data,.(MA,Class),summarize,number=length(MA))
for (i in 1:nrow(A4)){
  PC_vm_ck[4, A4[i,1], A4[i,2]/2] = A4[i,3]
}

A5 = ddply(data,.(SE_CellSize,Class),summarize,number=length(SE_CellSize))
for (i in 1:nrow(A5)){
  PC_vm_ck[5, A5[i,1], A5[i,2]/2] = A5[i,3]
}

A6 = ddply(data,.(BN,Class),summarize,number=length(BN))
for (i in 1:nrow(A6)){
  PC_vm_ck[6, A6[i,1], A6[i,2]/2] = A6[i,3]
}

A7 = ddply(data,.(BC,Class),summarize,number=length(BC))
for (i in 1:nrow(A7)){
  PC_vm_ck[7, A7[i,1], A7[i,2]/2] = A7[i,3]
}

A8 = ddply(data,.(NN,Class),summarize,number=length(NN))
for (i in 1:nrow(A8)){
  PC_vm_ck[8, A8[i,1], A8[i,2]/2] = A8[i,3]
}

A9 = ddply(data,.(Mitoses,Class),summarize,number=length(Mitoses))
for (i in 1:nrow(A9)){
  PC_vm_ck[9, A9[i,1], A9[i,2]/2] = A9[i,3]
}



# Partition data - train 70%, test 30%
#set.seed(511)
#ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
#train <- data[ind ==1,]
#test <- data[ind ==2,]
