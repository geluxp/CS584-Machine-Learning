################################
# GDA n d features with 2 class
################################
##clear the memory
rm(list = ls())


################################
# loading essential package
################################
require(cvTools) ##load cross validation package
require(MASS) ##load LDA package

######################################################################################
# loading the datasets wine and transforming the datasets into 13d features with 2 class
#######################################################################################
wine <- read.table('D:/CS584/KUN_MEI_ass2/data/wine.txt',sep=',')
data <- wine[1:130,]
## in order to imporve the performance, we scale the data 
windata <- as.data.frame(cbind(data[,1],scale(data[,-1])))      
featureNum <- ncol(windata[,-1]) # feature number=13

################################
##compute the prior probilibty 
################################
m1 <- nrow(windata[which(windata$V1=='1'),]) #number of class 1 examples
m2 <- nrow(windata[which(windata$V1=='2'),]) #number of class 2 examples
m <- nrow(windata) #number of total examples
prior <- c(m1/m,m2/m) #prior probability

################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,2,2) # creating a confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) # creating a confusion matrix computing by implemented function


#####################
## main loop
####################
##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(windata), K = k, type = "interleaved")
for(i in 1:k){
  testdata  <- subset(windata, folds$which == i)
  traindata <- subset(windata, folds$which != i)
  mean1 <- colMeans(traindata[which(traindata$V1=='1'),2:14])  #getting the mean of the class 1
  mean2 <- colMeans(traindata[which(traindata$V1=='2'),2:14])  #getting the mean of the class 2
  sigma <- cov(traindata[,2:14])       #getting the covariance matrix
  ##getting the inverse matrix 
  beta <- ginv(sigma)%*%(mean1-mean2)
  constant <- log((prior[1])/(prior[2]))
  gdaData <- as.matrix(testdata[,2:14])
  yhatEstimate <- vector()
  ## computing the yhatEstimate using implemented function parameters
  for (i in 1:nrow(gdaData)){
    dx <- t(beta)%*%(gdaData[i,]-((mean1+mean2)/2)) - constant
    if (dx>0)
      yhatEstimate <- c(yhatEstimate,1)
    else
      yhatEstimate <- c(yhatEstimate,2)
  }
  
  ## computing the yhat using the premade R package LDA
  model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2)/m) 
  pred <- predict(model, testdata)
  yhat <- apply(pred$posterior,1,which.max)
  
  
  
  #############################################################
  ## using a cumulation method to store the confusionMatrix.
  ##############################################################
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1])
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,1])
  
}

#print confusion matrix
confusionMatrixDefault      ## a confusion matrix computing by R package 
confusionMatrix    ## a confusion matrix computing by implemented function
