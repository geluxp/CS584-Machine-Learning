##clear the memory
rm(list = ls())


data1<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/svar-set1.dat")
data2<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/svar-set2.dat")
data3<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/svar-set3.dat")
data4<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/svar-set4.dat")
data5<-read.csv("D:/Kun_Mei_ass1/data/eye.txt")


singlePlotTest<-function(data){
  x<-data[,1]
  y<-data[,2]
  
  plot(x,y)
  model<-lm(y~x)
  abline(model)
  # add titles to an existing plot
   
}
par(mfrow=c(2,2))
singlePlotTest(data1)
# add titles to an existing plot
title(main="Plot of svar-set1.dat")
singlePlotTest(data2)
# add titles to an existing plot
title(main="Plot of svar-set2.dat")
singlePlotTest(data3)
# add titles to an existing plot
title(main="Plot of svar-set3.dat")
singlePlotTest(data4)
# add titles to an existing plot
title(main="Plot of svar-set4.dat")





polyerror<-function(data,polynomial,size){

  

     
  if (polynomial==1){
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    
    data<-data[1:size,]
   
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
     
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[,1] 
      yTrain<-trainData[,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain)  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
      
    }
    error=colMeans(temp)
    return(error)
   
  }
  if (polynomial==2){
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    data<-data[1:size,]
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[,1] 
      yTrain<-trainData[,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain+I(xTrain^2))  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=(xTrain^2)*theta[3]+xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=(xTest^2)*theta[3]+xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
      
    }
    error=colMeans(temp)
    return(error)
  }
  if (polynomial==3) {
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    data<-data[1:size,]
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[,1] 
      yTrain<-trainData[,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain+I(xTrain^2)+I(xTrain^3))  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=(xTrain^3)*theta[4]+(xTrain^2)*theta[3]+xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=(xTest^3)*theta[4]+(xTest^2)*theta[3]+xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
  }


    error=colMeans(temp)
    return(error)
}

}  

polyerror(data1,1,200)
polyerror(data1,2,200)
polyerror(data1,3,200)
polyerror(data2,1,200)
polyerror(data2,2,200)
polyerror(data2,3,200)
polyerror(data3,1,200)
polyerror(data3,2,200)
polyerror(data3,3,200)
polyerror(data4,1,200)
polyerror(data4,2,200)
polyerror(data4,3,200)


reducevalid<-function(data,polynomial,size){
  ##split the data and test different polynomial models
  
  
  
  if (polynomial==1){
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    data<-data[1:size,]
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
      
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[1:size/2,1] 
      yTrain<-trainData[1:size/2,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain)  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
      
    }
    error=colMeans(temp)
    return(error)
    
  }
  if (polynomial==2){
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    data<-data[1:size,]
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[1:size/2,1] 
      yTrain<-trainData[1:size/2,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain+I(xTrain^2))  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=(xTrain^2)*theta[3]+xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=(xTest^2)*theta[3]+xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
      
    }
    error=colMeans(temp)
    return(error)
  }
  if (polynomial==3) {
    #Randomly shuffle the data
    data<-data[sample(nrow(data)),]
    data<-data[1:size,]
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
    temp=matrix(,nrow=10,ncol=4) #use a matrix to temporarily store the training and testing error
    for(i in 1:10){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
      testData <- data[testIndexes, ]            #pick up the testData 
      trainData <- data[-testIndexes, ]          #pick up the trainData
      
      ##fetch the x,y in training set and testing set respectivly.
      xTrain<-trainData[1:size/2,1] 
      yTrain<-trainData[1:size/2,2]
      
      xTest<-testData[,1]
      yTest<-testData[,2]
      
      ##fit the linear regreesion model and get the training and testing error
      model<-lm(yTrain~xTrain+I(xTrain^2)+I(xTrain^3))  #fit the linear regreesion mode
      theta=model$coefficients  #get the theta1 and theta2
      
      yHatTrain=(xTrain^3)*theta[4]+(xTrain^2)*theta[3]+xTrain*theta[2]+theta[1] #compute the yhat-estimate value of yTraining
      mTrain<-length(yTrain)     #length of y
      MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
      RSETrain<-(sum(((yHatTrain-yTrain)^2)/yTrain^2))/mTrain #RSE
      
      yHatTest=(xTest^3)*theta[4]+(xTest^2)*theta[3]+xTest*theta[2]+theta[1] #compute the yhat-estimate value of yTesting
      mTest<-length(yTest)     #length of y
      MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
      RSETest<-(sum(((yHatTest-yTest)^2)/yTest^2))/mTest #RSE
      
      
      temp[i,]<-c(MSETrain,RSETrain,MSETest,RSETest)
    }
    
   
    error=colMeans(temp)
    return(error)
  }
  
}  


reducevalid(data1,1,200)
reducevalid(data1,2,200)
reducevalid(data1,3,200)
reducevalid(data2,1,200)
reducevalid(data2,2,200)
reducevalid(data2,3,200)
reducevalid(data3,1,200)
reducevalid(data3,2,200)
reducevalid(data3,3,200)
reducevalid(data4,1,200)
reducevalid(data4,2,200)
reducevalid(data4,3,200)

  

