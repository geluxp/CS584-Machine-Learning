######################
####logistical regression k class
######################

require(cvTools) ##load cross validation package

#Load data
wine <- read.table('D:/KUN_MEI_ASS3/DATA/wine.txt',sep=',')
data <- wine[1:130,2:14]
numberOfLabel1 <- nrow(wine[which(wine$V1=='1'),])
numberOfLabel2 <- nrow(wine[which(wine$V1=='2'),])
numberOfLabel3 <- nrow(wine[which(wine$V1=='3'),])
data1 <- cbind(c(rep(0,numberOfLabel1),rep(1,numberOfLabel2+numberOfLabel3)),wine[2:14])
data2 <- cbind(c(rep(1,numberOfLabel1),rep(0,numberOfLabel2),rep(1,numberOfLabel3)),wine[2:14])
data3 <- cbind(c(rep(1,numberOfLabel1),rep(1,numberOfLabel2),rep(0,numberOfLabel3)),wine[2:14])
names(data1)[1]<-paste("V1")
names(data2)[1]<-paste("V1")
names(data3)[1]<-paste("V1")

##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")
confusionMatrix1 <- matrix(0,2,2) # confusion matrix computing by implemented function
confusionMatrix2 <- matrix(0,2,2)
confusionMatrix3 <- matrix(0,2,2)
#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:14]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:14]))
  Y <- as.matrix(traindata$V1)
  #Cost Function
  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(J)
  }
  initial_theta <- rep(0,ncol(Xtrain))
  cost(initial_theta)
  theta_optim <- optim(par=initial_theta,fn=cost)
  theta <- theta_optim$par
  theta_optim$value
  df <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(df)){
    if(df[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix1 <- confusionMatrix1 + table(yhat,testdata[,1])
}


for(i in 1:k){
  testdata  <- subset(data2, folds$which == i)
  traindata <- subset(data2, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:14]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:14]))
  Y <- as.matrix(traindata$V1)
  #Cost Function
  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(J)
  }
  initial_theta <- rep(0,ncol(Xtrain))
  cost(initial_theta)
  theta_optim <- optim(par=initial_theta,fn=cost)
  theta <- theta_optim$par
  theta_optim$value
  df <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(df)){
    if(df[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix2 <- confusionMatrix2 + table(yhat,testdata[,1])
}


for(i in 1:k){
  testdata  <- subset(data3, folds$which == i)
  traindata <- subset(data3, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:14]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:14]))
  Y <- as.matrix(traindata$V1)
  #Cost Function
  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(J)
  }
  initial_theta <- rep(0,ncol(Xtrain))
  cost(initial_theta)
  theta_optim <- optim(par=initial_theta,fn=cost)
  theta <- theta_optim$par
  theta_optim$value
  df <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(df)){
    if(df[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix3 <- confusionMatrix3 + table(yhat,testdata[,1])
}


confusionMatrix1
confusionMatrix2
confusionMatrix3


##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix1[1,1]/(confusionMatrix1[1,1] + confusionMatrix1[1,2]),confusionMatrix2[1,1]/(confusionMatrix2[1,1] + confusionMatrix2[1,2]),confusionMatrix3[1,1]/(confusionMatrix3[1,1] + confusionMatrix3[1,2])))
rownames(precision) <- c("class1","class2","class3")
recall <- as.matrix(c(confusionMatrix1[1,1]/(confusionMatrix1[1,1] + confusionMatrix1[2,1]),confusionMatrix2[1,1]/(confusionMatrix2[1,1] + confusionMatrix2[2,1]),confusionMatrix3[1,1]/(confusionMatrix3[1,1] + confusionMatrix3[2,1])))
rownames(recall) <- c("class1","class2","class3")
accuracy <- c((confusionMatrix1[2,2] + confusionMatrix1[1,1])/(confusionMatrix1[2,2] + confusionMatrix1[1,1]+ confusionMatrix1[1,2] + confusionMatrix1[2,1]),
(confusionMatrix2[2,2] + confusionMatrix2[1,1])/(confusionMatrix2[2,2] + confusionMatrix2[1,1]+ confusionMatrix2[1,2] + confusionMatrix2[2,1]),
(confusionMatrix3[2,2] + confusionMatrix3[1,1])/(confusionMatrix3[2,2] + confusionMatrix3[1,1]+ confusionMatrix3[1,2] + confusionMatrix3[2,1]))
Fmeasure <- c(2*precision[1]*recall[1]/(precision[1]+recall[1]),
2*precision[2]*recall[2]/(precision[2]+recall[2]),
2*precision[3]*recall[3]/(precision[3]+recall[3]))

accuracy
precision
recall
Fmeasure