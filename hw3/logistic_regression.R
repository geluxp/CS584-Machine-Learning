######################
####logistical regression 2 class
######################
##clear the memory
rm(list = ls())

require(cvTools) ##load cross validation package

#Load data
data <- read.csv("D:/KUN_MEI_ASS3/DATA/data.csv")

#Create plot
plot(data$score.1,data$score.2,col=as.factor(data$label),xlab="Score-1",ylab="Score-2")

#Predictor variables
X <- as.matrix(data[,c(1,2)])


#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

kk=1
Xnew=matrix(0,nrow=nrow(X),ncol=6)
for (ii in 1:3){
  for (jj in ii:3)
  {Xnew[,kk]=X[,ii]*X[,jj]
   kk=kk+1}
}

#Response variable
Y <- as.matrix(data$label)

data1<-data.frame(Xnew[,-1],Y)

#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}


#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}
#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
prob <- sigmoid(t(c(1,45,85))%*%theta)


################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,2,2) # creating a confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) # creating a confusion matrix computing by implemented function


#####################
## main loop for no combinationa
####################
k <- 10 #10 folds
folds <- cvFolds(nrow(data), K = k, type = "interleaved")  #using CVTools


for(i in 1:k){
  testdata  <- subset(data, folds$which == i)
  traindata <- subset(data, folds$which != i)  
 
  #Predictor variables
  X <- as.matrix(traindata[,c(1,2)])
  #Add ones to X
  X <- cbind(rep(1,nrow(X)),X)  
  #Response variable
  Y <- as.matrix(traindata$label)  
  #Intial theta
  initial_theta <- rep(0,ncol(X))
  
  #Cost at inital theta
 # cost(initial_theta)
  
  # Derive theta using gradient descent using optim function
  theta_optim <- optim(par=initial_theta,fn=cost)
  
  #set theta
  theta <- theta_optim$par
  
  #cost at optimal value of the theta
  theta_optim$value
  
  Xtest<-as.matrix(testdata[,c(1,2)])
  Xtest<-cbind(rep(1,nrow(Xtest)),Xtest)
               
  prob <- sigmoid(Xtest%*%theta)         
  
  yhatEstimate<-vector()
  for (j in 1:length(prob)){
    if(prob[j]<0.5)
      yhatEstimate<-c(yhatEstimate,0)
    else
      yhatEstimate<-c(yhatEstimate,1)
  }

  
  ## computing the yhat using the premade R package LDA
 # model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2)/m) 
#  pred <- predict(model, testdata)
 # yhat <- apply(pred$posterior,1,which.max)
  
  #############################################################
  ## using a cumulation method to store the confusionMatrix.
  ##############################################################
  
 # confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1])
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,3])
  
}


#####################
## main loop for non-linear combinationS
####################
k <- 10 #10 folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")  #using CVTools


k=1
for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)  
  
  #Predictor variables
  X <- as.matrix(traindata[,c(1,5)])
  #Add ones to X
  X <- cbind(rep(1,nrow(X)),X)  
  

  
  #Response variable
  Y <- as.matrix(traindata$label)  
  #Intial theta
  initial_theta <- rep(0,ncol(X))
  
  #Cost at inital theta
  # cost(initial_theta)
  
  # Derive theta using gradient descent using optim function
  theta_optim <- optim(par=initial_theta,fn=cost)
  
  #set theta
  theta <- theta_optim$par
  
  #cost at optimal value of the theta
  theta_optim$value
  
  Xtest<-as.matrix(testdata[,c(1,5)])
  Xtest<-cbind(rep(1,nrow(Xtest)),Xtest)
  
  prob <- sigmoid(Xtest%*%theta)         
  
  yhatEstimate<-vector()
  for (j in 1:length(prob)){
    if(prob[j]<0.5)
      yhatEstimate<-c(yhatEstimate,0)
    else
      yhatEstimate<-c(yhatEstimate,1)
  }
  
  
  ## computing the yhat using the premade R package LDA
  # model <- lda(V1 ~ ., data = traindata, prior = c(m1,m2)/m) 
  #  pred <- predict(model, testdata)
  # yhat <- apply(pred$posterior,1,which.max)
  
  #############################################################
  ## using a cumulation method to store the confusionMatrix.
  ##############################################################
  
  # confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1])
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,6])
  
}

confusionMatrix
