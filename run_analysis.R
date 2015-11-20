run_analysis<- function(){
     ##load files
     xtest<-read.table("project/test/X_test.txt")
     ytest<-read.table("project/test/y_test.txt")
     subjecttest<-read.table("project/test/subject_test.txt")
     xtrain<-read.table("project/train/X_train.txt")
     ytrain<-read.table("project/train/y_train.txt")
     subjecttrain<-read.table("project/train/subject_train.txt")
     
     ##load names of data
     features<-read.table("project/features.txt")
     
     ##set names of data 
     names(xtest)<-features[,2]
     names(xtrain)<-features[,2]
     
     ##find names containing mean or std but not meanfreq
     means<-grep("mean",names(xtest))
     stds<-grep("std",names(xtest))
     meanfreqs<-grep("meanFreq",names(xtest))
     
     ##columns we want
     columns<-c(stds, setdiff(means,meanfreqs))
     
     ##subset xtest and xtrain to be just the columns we want
     xtest<-xtest[,columns]
     xtrain<-xtrain[,columns]
     
     ##cbind on the subject and activity to testing and training sets and name columns in test
     test<-cbind(subjecttest,ytest,xtest)
     names(test)[1]<-"subject"
     names(test)[2]<-"activity"
     train<-cbind(subjecttrain,ytrain,xtrain)
     names(train)[1]<-"subject"
     names(train)[2]<-"activity"
    
     
     ##rbind the data sets together
     output<-rbind(test,train)
     
     ##set names of activities
     output$activity[output$activity==1]<-"WALKING"
     output$activity[output$activity==2]<-"WALKING_UPSTAIRS"
     output$activity[output$activity==3]<-"WALKING_DOWNSTAIRS"
     output$activity[output$activity==4]<-"SITTING"
     output$activity[output$activity==5]<-"STANDING"
     output$activity[output$activity==6]<-"LAYING"
     
     
     ##create a melt and then dcast it to have mean by each variable
     library(reshape2)
     melt<-melt(output,id=c("subject","activity"),measure.vars=names(output)[3:68])
     output2<-dcast(melt, subject+activity~variable, mean)
     
     ##output tidy data
     write.table(output2, "tidydata.txt", row.names=FALSE)
}