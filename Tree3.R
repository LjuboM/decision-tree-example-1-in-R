rm(list=ls())
require("rpart")
require("rpart.plot")
data<-read.table("C:/Users/LjuboMamic/Desktop/Skin_NonSkin.txt",sep="\t", header=FALSE)
zeros_vector<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
accuracy_matrix<-matrix(zeros_vector,nrow=10,byrow=TRUE)
found<-FALSE
for(i in 1:11){
	if(i==11){
		j<-1
		max_accuracy<-max(accuracy_matrix[,2])
		while(max_accuracy!=accuracy_matrix[j,2]){
			j<-j+1
		}
		i<-j
		found<-TRUE
	}
	tenth <- which((1:nrow(data))%%10 == i-1)
	training_data<-data[-tenth,]
	test_data<-data[tenth,]
	y_training<-training_data[,4]
	x_training<-training_data[,-4]
	y_test<-test_data[,4]
	x_test<-test_data[,-4]
	tree<-rpart(y_training~.,x_training,control=rpart.control(minsplit=5), method="class")
	prediction<-predict(tree,x_test,type="class")
	
	conf <- table(y_test, prediction)
	conf
	accuracy<-(conf[1,1]+conf[2,2])/sum(conf)
	accuracy
	accuracy_matrix[i,1]<-i
	accuracy_matrix[i,2]<-accuracy
	error_possibility<-1-accuracy
	error_possibility
	if(found==TRUE){
		rpart.plot(tree)
		i<-11
	}
}
accuracy_matrix
j #indeks of tree with best accuracy.

