################################
# # naiveBayes with discrete features
################################
##clear the memory
rm(list = ls())


################################
# loading essential package
################################
require(cvTools)
require(MASS)
require(e1071)
######################################################################################
# loading the datasets wine and transforming the datasets into  with 2 class 
#######################################################################################
#transform the datasets into binomial form
wine <- read.table('D:/CS584/KUN_MEI_ass2/data/wine.txt',sep=',')
data <- wine [1:130,]


######################################################################################
# transforming the data into discrete form. In general, for different feature, we 
# are using different slice/groups
#######################################################################################
data$V2 <- cut(data$V2, breaks = c(0,11, 13,Inf), labels = c(1:3))
data$V3 <- cut(data$V3, breaks = c(0,1, 3, 5, Inf), labels = c(1:4))
data$V4 <- cut(data$V4, breaks = c(0,3, Inf), labels = c(1:2))
data$V5 <- cut(data$V5, breaks = c(0,10,15,20, Inf), labels = c(1:4))
data$V6 <- cut(data$V6, breaks = c(0,75,85,95, Inf), labels = c(1:4))
data$V7 <- cut(data$V7, breaks = c(0,1,2, Inf), labels = c(1:3))
data$V8 <- cut(data$V8, breaks = c(0,1,2, Inf), labels = c(1:3))
data$V9 <- cut(data$V9, breaks = c(0,0.15,0.25, Inf), labels = c(1:3))
data$V10 <- cut(data$V10, breaks = c(0,1.5,2.5, Inf), labels = c(1:3))
data$V11 <- cut(data$V11, breaks = c(0,11,12,13, Inf), labels = c(1:4))
data$V12 <- cut(data$V12, breaks = c(0,0.8,1,1.5, Inf), labels = c(1:4))
data$V13 <- cut(data$V13, breaks = c(0,2,3, Inf), labels = c(1:3))
data$V14 <- cut(data$V14, breaks = c(0,500, 800, 1100, 1400, Inf), labels = c(1:5))

#################
##reloading and recombing the data
###############
attach(data)
winedata <- as.data.frame(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14))
detach(data)

################################
##compute the prior probilibty 
################################
m1 <- nrow(winedata[which(winedata$V1=='1'),])
m2 <- nrow(winedata[which(winedata$V1=='2'),])
m <- nrow(winedata)
prior <- c(m1/m,m2/m)

################################
##initialization confusion matrice
################################
confusionMatrixDefault <- matrix(0,2,2) # confusion matrix computing by R package 
confusionMatrix <- matrix(0,2,2) #computed by implementing functions


#####################
## main loop
####################
#10 folds cross-validation
k <- 10
folds <- cvFolds(nrow(winedata), K = k, type = "interleaved")
for (i in 1:k){
  testdata  <- subset(winedata, folds$which == i)
  traindata <- subset(winedata, folds$which != i)
  probabilitySumOfClass1<-0 #Initilization 
  probabilitySumOfClass2<-0 #Initilization 
  mTrain<-nrow(traindata)
  for (j in 1:mTrain){ ## compute the naiveBayes postior
    
      if (traindata[j,1]==1){
        for (k in 2:14){
        probabilitySumOfClass1<-probabilitySumOfClass1+traindata[j,k]
        }
      }else{
        for (k in 2:14){
        probabilitySumOfClass2<-probabilitySumOfClass2+traindata[j,k]
        }
      }

  }
  
  #parameter alpha
  alphaClass1<-colMeans(traindata[which(traindata$V1=='1'),2:14])
  alphaClass1<-nrow(traindata[which(traindata$V1=='1'),])/probabilitySumOfClass1*alphaClass1
  alphaClass2<-colMeans(traindata[which(traindata$V1=='2'),2:14])
  alphaClass2<-nrow(traindata[which(traindata$V1=='2'),])/probabilitySumOfClass2*alphaClass2
  
  mTest <- nrow(testdata) #testdata number
  yhatEstimate<-vector() #predict class label
  for (i1 in 1:mTest){  ##  use test data to predict.
    g1 <-1
    g2 <-1
    temp <-0  
    for (j1 in 2:14){
      temp<-temp+testdata[i1,j1]
    }
    for (j2 in 2:14){
      g1<-g1*choose(temp,testdata[i1,j2])*(alphaClass1[j2-1]^testdata[i1,j2])*((1-alphaClass1[j2-1])^(temp-testdata[i1,j2]))
      g2<-g2*choose(temp,testdata[i1,j2])*(alphaClass2[j2-1]^testdata[i1,j2])*((1-alphaClass2[j2-1])^(temp-testdata[i1,j2]))
    }
    dx<-g1-g2
    if (dx>0){
      yhatEstimate <- c(yhatEstimate,1)
    }else{
      yhatEstimate <- c(yhatEstimate,2)
    }  
    
  }
  ## computing the yhat using the premade R package naiveBayes
  model <- naiveBayes(as.factor(V1) ~ ., data = traindata)
  yhat <- predict(model, testdata[,-1])
  
  #compute confusion matrix
  confusionMatrixDefault <- confusionMatrixDefault + table(yhat,testdata[,1]) 
  confusionMatrix <- confusionMatrix + table(yhatEstimate,testdata[,1])  
}  

#print confusion matrix of default and implemented
confusionMatrixDefault
confusionMatrix

##evaluate performance of algorithm
precision <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2]),confusionMatrix[2,2]/(confusionMatrix[2,1] + confusionMatrix[2,2])))
rownames(precision) <- c("class1","class2")
recall <- as.matrix(c(confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[2,1]),confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[1,2])))
rownames(recall) <- c("class1","class2")
accuracy <- (confusionMatrix[2,2] + confusionMatrix[1,1])/(confusionMatrix[2,2] + confusionMatrix[1,1]+ confusionMatrix[1,2] + confusionMatrix[2,1])
Fmeasure <- 2*precision*recall/(precision+recall)

#compute stats
precisionDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[1,2]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,1] + confusionMatrixDefault[2,2])))
recallDefault <- as.matrix(c(confusionMatrixDefault[1,1]/(confusionMatrixDefault[1,1] + confusionMatrixDefault[2,1]),confusionMatrixDefault[2,2]/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,2])))
accuracyDefault <- (confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1])/(confusionMatrixDefault[2,2] + confusionMatrixDefault[1,1]+ confusionMatrixDefault[1,2] + confusionMatrixDefault[2,1])
FmeasureDefault <- 2*precision*recall/(precision+recall)

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


