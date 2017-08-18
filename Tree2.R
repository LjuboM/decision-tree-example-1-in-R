rm(list=ls())
require("rpart")
require("rpart.plot")
data<-read.table("Skin_NonSkin.txt",sep="\t", header=FALSE)
rowsWithIndexNumberEndingWithZero <- which((1:nrow(data))%%10 == 0)

#Podjela podataka na trening i test dio
training_data<-data[-rowsWithIndexNumberEndingWithZero,]
test_data<-data[rowsWithIndexNumberEndingWithZero,]
y_training<-training_data[,4]
x_training<-training_data[,-4]
y_test<-test_data[,4]
x_test<-test_data[,-4]
tree<-rpart(y_training~.,x_training,control=rpart.control(minsplit=5), method="class")
rpart.plot(tree)
prediction<-predict(tree,x_test,type="class")

conf <- table(y_test, prediction)
conf
accuracy<-(conf[1,1]+conf[2,2])/sum(conf)
accuracy
error_possibility<-1-tocnost
error_possibility
