################################
# GDA 1 d features with 2 class
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
winedata <- wine[1:130,1:2] 

################################
##compute the prior probilibty 
################################
m1 <- nrow(winedata[which(winedata$V1=='1'),]) #number of class 1 examples
m2 <- nrow(winedata[which(winedata$V1=='2'),]) #number of class 2 examples
m <- nrow(winedata) #number of total examples
prior <- c(m1/m,m2/m) #prior probability


################################
## GDA 1D main function  
################################
gda <- function(x,prior){
  g1 <- (-0.5*(x-mu1)^2/var+log(prior[1]))
  g2 <- (-0.5*(x-mu2)^2/var+log(prior[2]))
  dx <- g1-g2
}


################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,2,2) # creating a confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) # creating a confusion matrix computing by implemented function


#####################
## main loop
####################
k <- 10 #10 folds
folds <- cvFolds(nrow(winedata), K = k, type = "interleaved")  #using CVTools
for(i in 1:k){
  testdata  <- subset(winedata, folds$which == i)
  traindata <- subset(winedata, folds$which != i)  
 mean1 <- mean(traindata[which(traindata$V1=='1'),]$V2)  #getting the mean of the class 1
 mean2 <- mean(traindata[which(traindata$V1=='2'),]$V2)   #getting the mean of the class 2
  var <- var(traindata$V2)              #getting the total variation.
  dx <- gda(testdata[,2],prior)        #using GDA to compute the dx
  yhatEstimate <- vector()  #initate yhatEstimate
  ## computing the yhatEstimate using implemented function parameters
  for (i in 1:length(dx)){
    if(dx[i]>0)
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

