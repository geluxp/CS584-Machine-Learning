####logistical regression 2 class nonlieanr

require(cvTools) ##load cross validation package

#Load data
wine <- read.table('D:/KUN_MEI_ASS3/DATA/wine.txt',sep=',')
data <- wine[1:130,2:14]
numberOfLabel1 <- nrow(wine[which(wine$V1=='1'),])
numberOfLabel2 <- nrow(wine[which(wine$V1=='2'),])
#transform the data into non-linear form
data1 <- cbind(c(rep(0,numberOfLabel1),rep(1,numberOfLabel2)),data,data[,2]*data[,3])
names(data1)[1]<-paste("V1")

##10 folds cross-validation 
k <- 10 #number of folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")
confusionMatrix <- matrix(0,2,2) # confusion matrix computing by implemented function

#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:15]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:15]))
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
  confusionMatrix <- confusionMatrix + table(yhat,testdata[,1])
}

confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2]),confusionMatrix[2,2]/(confusionMatrix[2,1] + confusionMatrix[2,2])))
rownames(precision) <- c("class1","class2")
recall <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[2,1]),confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[1,2])))
rownames(recall) <- c("class1","class2")
accuracy <- (confusionMatrix[2,2] + confusionMatrix[1,1])/(confusionMatrix[2,2] + confusionMatrix[1,1]+ confusionMatrix[1,2] + confusionMatrix[2,1])
Fmeasure <- 2*precision*recall/(precision+recall)

accuracy
precision
recall
Fmeasure

