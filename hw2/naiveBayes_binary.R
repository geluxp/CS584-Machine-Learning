################################
# naiveBayes with binary features
################################
##clear the memory
################################
rm(list = ls())
# loading essential package
################################
require(cvTools)
require(MASS)
require(e1071)

######################################################################################
# loading the datasets wine and transforming the datasets into  with 2 class
#######################################################################################
wine <- read.table('D:/CS584/KUN_MEI_ass2/data/wine.txt', sep=',')
data <- wine [1:130,]

############################
## we are using a trick here: in order to get binary features, we transforming the continuous values in this way: 
## first we compute the column(feature)wise mean, then we compare each of the value with the corresponding mean,
## if the value is larger then the mean, we set the value to 1 (postive),otherwise 0.
##############
data1 <- matrix(0, 130, 13)
data1 <- cbind(data[,1],data1)
data1 <- as.data.frame(data1)
for (i in 2:14){
  split <- mean(data[1:130,i])
  for (j in 1:130){
    if(data[j,i] >= split){
      data1[j,i] <- 1
    }
  }
}
################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,2,2) #computed by R package
confusionMatrix <- matrix(0,2,2) #computed by implementing functions
################################
##compute the prior probilibty 
################################
m1 <- nrow(data1[which(data1$V1=='1'),])
m2 <- nrow(data1[which(data1$V1=='2'),])
m <- nrow(data1)
prior <- c(m1/m,m2/m)


##10 folds cross-validation
k <- 10
folds <- cvFolds(nrow(data), K = k, type = "interleaved")


for (i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)
  alpha1 <- colMeans(traindata[which(traindata$V1=='1'),2:14])
  alpha2 <- colMeans(traindata[which(traindata$V1=='2'),2:14]) 
  yhatEstimate<-vector()
  for (j in 1:13){
    g1 <- 1
    g2 <- 1
    for (k in 1:13){
      if (testdata[j,k+1]==1){
        g1 <- g1*(alpha1[k])*prior[1]
        g2 <- g2*(alpha2[k])*prior[2]
      } else{
        g1 <- g1*(1-alpha1[k])*prior[1]
        g2 <- g2*(1-alpha2[k])*prior[2]
      } 
    }
    dx <- g1-g2
    if (dx>0){
      yhatEstimate <- c(yhatEstimate,1)
    }else{
      yhatEstimate <- c(yhatEstimate,2)
    }  
    
  } 
  #Naive Bayes readymade R function
  model <- naiveBayes(as.factor(V1) ~ ., data = traindata)
  yhat <- predict(model, testdata[,-1])
  
  #compute confusion matrix
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1]) 
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,1])
  
}

#print confusion matrix
confusionMatrixDefault
confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2]),confusionMatrix[2,2]/(confusionMatrix[2,1] + confusionMatrix[2,2])))
rownames(precision) <- c("class1","class2")
recall <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[2,1]),confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[1,2])))
rownames(recall) <- c("class1","class2")
accuracy <- (confusionMatrix[2,2] + confusionMatrix[1,1])/(confusionMatrix[2,2] + confusionMatrix[1,1]+ confusionMatrix[1,2] + confusionMatrix[2,1])
Fmeasure <- 2*precision*recall/(precision+recall)
precisionDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[1,2]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,1] + confusionMatrixDefault[2,2])))
recallDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[2,1]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,2])))
accuracyDefault <- (confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1])/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1]+ confusionMatrixDefault[1,2] + confusionMatrixDefault[2,1])
FmeasureDefault <- 2*precisionDefault*recallDefault/(precisionDefault+recallDefault)

#statistics for model
accuracy <- cbind(accuracy,accuracyDefault)
colnames(accuracy) <- c("implement","default")
precision <- cbind(precision,precisionDefault)
colnames(precision) <- c("implement","default")
recall <- cbind(recall,recallDefault)
colnames(recall) <- c("implement","default")
Fmeasure <- cbind(Fmeasure,FmeasureDefault)
colnames(Fmeasure) <- c("implement","default")

#print stats
accuracy
precision
recall
Fmeasure