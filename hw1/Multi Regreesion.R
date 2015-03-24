##clear the memory
rm(list = ls())

data1<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set1.dat")
data2<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set2.dat")
data3<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set3.dat")
data4<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set4.dat")
data5<-read.csv("D:/Kun_Mei_ass1/data/servo.data",sep=",", header=F)
data5<-data.frame(data5[,3:5])


multiregress<-function(data,method="less",polynomial){
  ##load the variables
  if (method=="less"){
   if (polynomial==1){
## do 10-fold cross validation
#Randomly shuffle the data
data<-data[sample(nrow(data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)


#Perform 10 fold cross validation

temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
for(i in 1:10){
  #Segement your data by fold using the which() function 
  
  testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
  testData <- data[testIndexes, ]            #pick up the testData 
  trainData <- data[-testIndexes, ]          #pick up the trainData
  
  ##fetch the x,y in training set and testing set respectivly.
  xTrain<-trainData[,1:2] 
  xTrain<-cbind(xTrain)
  
  yTrain<-trainData[,3]
  
  xTest<-testData[,1:2]
  yTest<-testData[,3]
  
  ##fit the linear regreesion model and get the training and testing error
  
  model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
  theta=model$coefficients  #get the theta values
  
  xTrain<-as.matrix(xTrain)
  
  xTrain<-cbind(1,xTrain)
  xTest<-cbind(1,xTest)
  xTrain<-as.matrix(xTrain)
  xTest<-as.matrix(xTest)
  
  yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
  yHatTest=xTest %*% (theta) #compute the yhat-estimate value of yTraining
  
  mTrain<-length(yTrain)     #length of yTrain
  MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
  mTest<-length(yTest)     #length of yTest
  MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE

  

  temp[i,]<-c(MSETrain,MSETest)


  }
error=colMeans(temp)
return(error)
}
   if (polynomial==2){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  x<-data[,1:2]
  
  xsqrt<-matrix(0,ncol=3,nrow=nrow(data))
  
  k=1
  for(i in 1:2){
    for (j in i:2)
    {xsqrt[,k]<-x[,i]*x[,j]
    k<-k+1}
    
  }
  y<-data[,3]
  data<-data.frame(xsqrt,x,y)
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:5] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,6]
    
    xTest<-testData[,1:5]
    yTest<-testData[,6]
    
    ##fit the linear regreesion model and get the training and testing error
    
    model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
    theta=model$coefficients  #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% (theta) 
    
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
  return(error)
}
  if (polynomial==3){
# do 10-fold cross validation
     #Randomly shuffle the data
     data<-data[sample(nrow(data)),]
     x<-data[,1:2]
     xsqrt<-matrix(0,ncol=3,nrow=2500)
     k=1
     for(i in 1:2){
       for (j in i:2)
       {xsqrt[,k]<-x[,i]*x[,j]
        k<-k+1}
       
     }
     xcubic<-matrix(0,ncol=5,nrow=2500)
     
     k=1
     for(i in 1:2){
       for (j in i:2){
         for (t in j:2)
       {xcubic[,k]<-x[,i]*x[,j]
        k<-k+1}
       
     }
     }
     
     y<-data[,3]
     data<-data.frame(xcubic,xsqrt,x,y)
     #Create 10 equally size folds
     folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
     
     
     #Perform 10 fold cross validation
     
     temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
     for(i in 1:10){
       #Segement your data by fold using the which() function 
       
       testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
       testData <- data[testIndexes, ]            #pick up the testData 
       trainData <- data[-testIndexes, ]          #pick up the trainData
       
       ##fetch the x,y in training set and testing set respectivly.
       xTrain<-trainData[,1:10] 
       xTrain<-cbind(xTrain)
       
       yTrain<-trainData[,11]
       
       xTest<-testData[,1:10]
       yTest<-testData[,11]
       
       ##fit the linear regreesion model and get the training and testing error
       
       model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
       theta=model$coefficients  #get the theta values
       
       xTrain<-as.matrix(xTrain)
       
       xTrain<-cbind(1,xTrain)
       xTest<-cbind(1,xTest)
       xTrain<-as.matrix(xTrain)
       xTest<-as.matrix(xTest)
       
       yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
       yHatTest=xTest %*% (theta) 
       
       
       mTrain<-length(yTrain)     #length of yTrain
       MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
       mTest<-length(yTest)     #length of yTest
       MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
       
       
       
       temp[i,]<-c(MSETrain,MSETest)
       
       
     }
     error=colMeans(temp)
     return(error)}

}
  if (method=="more"){
   
    
  }
if (polynomial==1){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:5] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,6]
    
    xTest<-testData[,1:5]
    yTest<-testData[,6]
    
    ##fit the linear regreesion model and get the training and testing error
    
    model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
    theta=model$coefficients  #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% (theta) #compute the yhat-estimate value of yTraining
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
  return(error)
}
if (polynomial==2){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  x<-data[,1:5]
  xsqrt<-matrix(0,ncol=15,nrow=100000)
  
  k=1
  for(i in 1:5){
    for (j in i:5)
    {xsqrt[,k]<-x[,i]*x[,j]
    k<-k+1}
  }
  y<-data[,6]
  data<-data.frame(xsqrt,x,y)
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:20] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,21]
    
    xTest<-testData[,1:20]
    yTest<-testData[,21]
    
    ##fit the linear regreesion model and get the training and testing error
    
    model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
    theta=model$coefficients  #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% (theta) 
    
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
  return(error)
}
if (polynomial==3){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  x<-data[,1:5]
  xsqrt<-matrix(0,ncol=3,nrow=2500)
  k=1
  for(i in 1:5){
    for (j in i:5)
    {xsqrt[,k]<-x[,i]*x[,j]
     k<-k+1}
    
  }
  xcubic<-matrix(0,ncol=36,nrow=2500)
  
  k=1
  for(i in 1:5){
    for (j in i:5){
      for (t in j:5)
      {xcubic[,k]<-x[,i]*x[,j]
       k<-k+1}
      
    }
  }
  
  
  y<-data[,6]
  data<-data.frame(xcubic,xsqrt,x,y)
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:5] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,6]
    
    xTest<-testData[,1:5]
    yTest<-testData[,6]
    
    ##fit the linear regreesion model and get the training and testing error
    
    model<-lm(yTrain~., data=xTrain)  #fit the linear regreesion mode
    theta=model$coefficients  #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% (theta) 
    
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
  return(error)
}


}




multiregress(data1,"less",1)








# define the gradient function dJ/dtheata: 1/m * (h(x)-y))*x where h(x) = x*theta
# in matrix form this is as follows:
grad <- function(x, y, theta) {
  m<-length(y)
  gradient <- (1/m)* (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}
# define gradient descent update algorithm
grad.descent <- function(x, y,maxit){
  theta <- matrix(0, ncol=3,nrow=1) # Initialize the parameters
  
  alpha = .05 # set learning rate
  for (i in 1:maxit) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}






#iterativeMethod
iterativeMethod<-function(data,maxit){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:2] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,3]
    
    xTest<-testData[,1:2]
    yTest<-testData[,3]
    
    ##fit the linear regreesion model and get the training and testing error
    
    x<-data[,1:2]
    x<-as.matrix(cbind(x,matrix(1,ncol=1,nrow=nrow(x))))
    y<-data[,3]
    theta=grad.descent(x,y,maxit)#fit the linear regreesion mode
     #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% t(theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% t(theta) #compute the yhat-estimate value of yTraining
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
 return(error)

}
## http://www.r-bloggers.com/machine-learning-ex3-multivariate-linear-regression/
explicitMethod<-function(data){
  ## do 10-fold cross validation
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  
  
  #Perform 10 fold cross validation
  
  temp=matrix(,nrow=10,ncol=2) #use a matrix to temporarily store the training and testing error
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    
    testIndexes <- which(folds==i,arr.ind=TRUE)   #create the testIndexes
    testData <- data[testIndexes, ]            #pick up the testData 
    trainData <- data[-testIndexes, ]          #pick up the trainData
    
    ##fetch the x,y in training set and testing set respectivly.
    xTrain<-trainData[,1:2] 
    xTrain<-cbind(xTrain)
    
    yTrain<-trainData[,3]
    
    xTest<-testData[,1:2]
    yTest<-testData[,3]
    
    ##fit the linear regreesion model and get the training and testing error
    
    x<-data[,1:2]
    x<-as.matrix(cbind(x,matrix(1,ncol=1,nrow=nrow(x))))
    y<-data[,3]
    theta= solve(t(x) %*% x) %*% (t(x) %*% y)
    #get the theta values
    
    xTrain<-as.matrix(xTrain)
    
    xTrain<-cbind(1,xTrain)
    xTest<-cbind(1,xTest)
    xTrain<-as.matrix(xTrain)
    xTest<-as.matrix(xTest)
    
    yHatTrain=xTrain %*% (theta) #compute the yhat-estimate value of yTraining
    yHatTest=xTest %*% (theta) #compute the yhat-estimate value of yTraining
    
    mTrain<-length(yTrain)     #length of yTrain
    MSETrain<-sum((yHatTrain-yTrain)^2)/mTrain   #MSE
    mTest<-length(yTest)     #length of yTest
    MSETest<-sum((yHatTest-yTest)^2)/mTest   #MSE
    
    
    
    temp[i,]<-c(MSETrain,MSETest)
    
    
  }
  error=colMeans(temp)
  return(error)
}