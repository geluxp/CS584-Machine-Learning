################################
# GDA n d features with k class
################################
##clear the memory
rm(list = ls())

################################
# loading essential package
################################
require(cvTools) ##load cross validation package
require(MASS) ##load LDA package

######################################################################################
# loading the datasets wine and transforming the datasets into 1d features with 2 class
#######################################################################################
wine <- read.table('D:/CS584/KUN_MEI_ass2/data/wine.txt',sep=',')
windata <- as.data.frame(cbind(wine[,1],scale(wine[,-1])))
featureNum <- ncol(windata[,-1])

################################
##compute the prior probilibty 
################################
m1 <- nrow(windata[which(windata$V1=='1'),]) #number of class 1 examples
m2 <- nrow(windata[which(windata$V1=='2'),])#number of class 2 examples
m3 <- nrow(windata[which(windata$V1=='3'),])#number of class 3 examples
m <- nrow(windata) #number of total examples
prior <- c(m1/m,m2/m,m3/m) #prior probability

################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,3,3) # confusion matrix computing by R package 
confusionMatrix <- matrix(0,3,3) # confusion matrix computing by implemented function




#####################
## main loop
####################
##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(windata), K = k, type = "interleaved")
for(i in 1:k){
  testdata  <- subset(windata, folds$which == i)
  traindata <- subset(windata, folds$which != i)
  mean1 <- colMeans(traindata[which(traindata$V1=='1'),2:14])   #getting the mean of the class 1
  mean2 <- colMeans(traindata[which(traindata$V1=='2'),2:14])   #getting the mean of the class 2
  mean3 <- colMeans(traindata[which(traindata$V1=='3'),2:14])   #getting the mean of the class 3
  sigma <- cov(traindata[,2:14])      #getting the covariance matrix
  ##getting the inverse matrix 
  beta12 <- ginv(sigma)%*%(mean1-mean2)   
  beta13 <- ginv(sigma)%*%(mean1-mean3)
  beta23 <- ginv(sigma)%*%(mean2-mean3)
  
  constant12 <- log((prior[1])/(prior[2]))
  constant13 <- log((prior[1])/(prior[3]))
  constant23 <- log((prior[2])/(prior[3]))
  gdaData <- as.matrix(testdata[,2:14])
  yhatEstimate <- vector()
  ## computing the yhatEstimate using implemented function parameters
  for (i in 1:nrow(gdaData)){
    dx12 <- t(beta12)%*%(gdaData[i,]-((mean1+mean2)/2)) - constant12
    dx13 <- t(beta13)%*%(gdaData[i,]-((mean1+mean3)/2)) - constant13
    dx23 <- t(beta23)%*%(gdaData[i,]-((mean2+mean3)/2)) - constant23
    if (dx12>0 && dx13>0)
      yhatEstimate <- c(yhatEstimate,1)
    else if(dx12<0 && dx23>0)
      yhatEstimate <- c(yhatEstimate,2)
    else
      yhatEstimate <- c(yhatEstimate,3)
  }
  
  ## computing the yhat using the premade R package LDA
  model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2,m3)/m) 
  pred <- predict(model, testdata)
  yhat <- apply(pred$posterior,1,which.max)
  
  #############################################################
  ## using a cumulation method to store the confusionMatrix.
  ##############################################################
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1])
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,1])
  
}

#print confusion matrix
confusionMatrixDefault
confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/sum(confusionMatrix[1,]),confusionMatrix[2,2]/sum(confusionMatrix[2,]),confusionMatrix[3,3]/sum(confusionMatrix[3,])))
recall <- as.matrix(c(confusionMatrix[1,1]/sum(confusionMatrix[,1]),confusionMatrix[2,2]/sum(confusionMatrix[,2]),confusionMatrix[3,3]/sum(confusionMatrix[,3])))
accuracy <- (confusionMatrix[3,3]+confusionMatrix[2,2] + confusionMatrix[1,1])/sum(confusionMatrix)
Fmeasure <- 2*precision*recall/(precision+recall)
