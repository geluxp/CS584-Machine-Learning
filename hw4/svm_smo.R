

setClass("svmResult",
         representation=representation(
           X="matrix",
           y="numeric",
           kernelFunction="character",
           w="matrix",
           b="numeric",
           alphas="numeric"
         )
)

svmTrain <- function(X, Y, C, kernelFunction="linearKernel",
                     tol=1e-4, max.iter=20, verbose=FALSE) {

  
  ## data parameter
  m <- nrow(X)
  #n <- ncol(X)
  
  ## labels
  ## Map 0 to -1
  Y[Y==0] <- -1
  
  # variables
  alphas <- rep(0, m)
  b <- 0
  E <- rep(0, m)
  iter <- eta <- L <- H <- 0
  
  ## transformations of original data to map into new space
  if(kernelFunction == "linearKernel") {
    K <- matrix(0, ncol=m, nrow=m)
    K <- X %*% t(X)
  } else if (kernelFunction == "gaussianKernel") {
    K <- matrix(0, ncol=m, nrow=m)
    for (i in 1:m) {
      for (j in i:m) {
        K[i,j] <- gaussianKernel(X[i,], X[j,])
        K[j,i] <- K[i,j]
      }
    }
  } else if (kernelFunction =="polynominalKernel"){
    K <- matrix(0, ncol=m, nrow=m)
    for (i in 1:m) {
      for (j in i:m) {
        K[i,j] <- polyKernel(X[i,], X[j,])
        
      }
    }
  }
  
  while (iter < max.iter) {
    num_changed_alphas <- 0
    for (i in 1:m){
      E[i] <- b + sum(alphas * Y * K[,i]) - Y[i]
      if( (Y[i]*E[i] < -tol & alphas[i] < C) || (Y[i] >  tol & alphas[i] > 0) ) {
        ## if the error E[i] is large
        ## the alpha corresponding to this data instance can be optimized
        
        j <- ceiling(m * runif(1))
        while (j == i) { # make sure i != j
          j <- ceiling(m * runif(1))
        }
        
        E[j] <- b + sum(alphas * Y * K[,j]) - Y[j]
        
        ## save old alphas
        alpha.i.old <- alphas[i]
        alpha.j.old <- alphas[j]
        
        if (Y[i] == Y[j]) {
          L <- max(0, alphas[j] + alphas[i] -C)
          H <- min(C, alphas[j] + alphas[i])
        } else {
          L <- max(0, alphas[j] - alphas[i])
          H <- min(C, C + alphas[j] - alphas[i])
        }
        
        if (L == H) {
          ## continue to next i
          next
        }
        
        ## compute eta
        eta <- 2 * K[i,j] - K[i,i] - K[j,j]
        if (eta >= 0) {
          ## continue to next i
          next
        }
        
        ## compute and clip new value for alpha j
        alphas[j] <- alphas[j] - Y[j] * (E[i] - E[j])/eta
        ## clip
        alphas[j] = min(H, alphas[j])
        alphas[j] = max(L, alphas[j])
        
        ## check if change in alpha is significant
        if(abs(alphas[j] - alpha.j.old) < tol) {
          alphas[j] <- alpha.j.old
          next
        }
        
        ## determine value for alpha i
        alphas[i] <- alphas[i] + Y[i] * Y[j] * (alpha.j.old - alphas[j])
        
        ## compute b1 and b2
        b1 <- b - E[i] +
          - Y[i] * (alphas[i] - alpha.i.old) * K[i,j] +
          - Y[j] * (alphas[j] - alpha.j.old) * K[i,j]
        b2 <- b - E[j] +
          - Y[i] * (alphas[i] - alpha.i.old) * K[i,j] +
          - Y[j] * (alphas[j] - alpha.j.old) * K[i,j]
        
        ## compute b
        if ( alphas[i] > 0 & alphas[i] < C) {
          b <- b1
        } else if (alphas[j] > 0 & alphas[j] < C) {
          b <- b2
        } else {
          b <- (b1+b2)/2
        }
        
        num_changed_alphas <- num_changed_alphas + 1
      }
    }
    
    if (num_changed_alphas == 0) {
      iter <- iter + 1
    } else {
      iter <-  0
    }
    if (verbose) {
      setTxtProgressBar(pb, iter)
    }
  }
  
  if(verbose) {
    close(pb)
    print("done")
  }
  
  idx <- alphas > 0
  new("svmResult",
      X=X[idx,],
      y=Y[idx],
      kernelFunction=kernelFunction,
      b=b,
      alphas=alphas[idx],
      w=t(alphas * Y) %*% X
  )
}


svmPredict <- function(model, X) {
  ## returns a vector of predictions using a trained SVM model (svmTrain).
  ## X is a m x n matrix where there each example is a row.
  ## model is a svm model returned from svmTrain.
  
  ## output pred is a vector of length m of predictions of {0, 1} values.
  
  
  
  m <- nrow(X)
  p <- rep(0, m)
  pred <- rep(0, m)
  kernelFunction <- model["kernelFunction"]
  if (kernelFunction == "linearKernel") {
    p <- X %*%t( model["w"]) + model["b"]
  } else if (kernelFunction == "gaussianKernel") {
    
    ##    X1 <- rowSums(X^2)
    ##    X2 <- rowSums(model$X^2)
    ##    K <- X1 + X2 - 2* X %*% t(model$X)
    ##    K <- gaussianKernel(1,0) ^K
    ##    K <- model$y * K
    ##    K <- model$alphas * K
    ##    p <- rowSums(K)
    
    alphas <- model["alphas"]
    for (i in 1:m) {
      prediction <- 0
      for (j in 1:nrow(model["X"])) {
        prediction <- prediction +
          alphas[j] * model["y"][j] *
          gaussianKernel(X[i,], model["X"][j,])
      }
      p[i] <- prediction + model["b"]
    }
    
  } else if (kernelFunction=="polynominalKernel"){
    alphas <- model["alphas"]
    for (i in 1:m) {
      prediction <- 0
      for (j in 1:nrow(model["X"])) {
        prediction <- prediction +
          alphas[j] * model["y"][j] *
          polyKernel(X[i,], model["X"][j,])
      }
      p[i] <- prediction + model["b"]
    }
  }
  pred[p >= 0] <- 1
  pred[p < 0] <- 0
  return(pred)
}


gaussianKernel <- function(x1,x2, sigma=0.1) {
  ## Gaussian kernel is a similarity function that
  ## measures the "distance" between a pair of examples.
  sim <- exp(-sum((x1-x2)^2)/(2*sigma^2))
  return(sim)
}

## equivalent with meshgrid in octave
meshgrid <- function(a, b) {
  list(
    x <- outer(b*0, a, FUN="+"),
    y <- outer(b, a*0, FUN="+")
  )
}

polykernel<-function(X){
  sim<-(X%*%t(X)+1)^3
  return(sim)
}


setMethod(
  f="[",
  signature=signature(x="svmResult", i="character"),
  definition=function(x,i,j...) {
    if(i=="w")
      return(x@w)
    if(i=="b")
      return(x@b)
    if(i=="alphas")
      return(x@alphas)
    if(i=="kernelFunction")
      return(x@kernelFunction)
    if(i=="X")
      return(x@X)
    if(i=="y")
      return(x@y)
  }
)



svmPlot_linear=function(data,model,tt="separable"){
  
  w=model["w"]
  b=model["b"]
  
  
  xp=seq(min(data[,1]),max(data[,1]),length=100)
  yp=-(w[1]*xp+b)/w[2]
  
  plot(data[,1],data[,2],col=ifelse(data[,3]==1,"red","green"),xlab="x",ylab="y",main="linear SVM result", sub=tt)
  lines(xp,yp)
}



polyKernel<-function(x1,x2){
  sim<-(t(x1)%*%x2+1)^3
  return(sim)
}


svmPlot_nonlinear<-function(model, X, y,,type="nonlinear" title="", xlab="", ylab=""){
  colnames(X)=c("v1","v2")
  d <- data.frame(X, y=as.factor(y))
  
  V <- colnames(X)
  p <- ggplot(data=d, aes_string(x=V[1], y=V[2])) +
    geom_point(aes(colour=y))
  
  
  w <- model["w"]
  b <- model["b"]
  if(type == "linear") {
    p <- p+geom_abline(intercept = -b/w[2],
                       slope = -w[1]/w[2],
                       colour = "yellow")
  } else {
    xr <- seq(min(X[,1]), max(X[,1]), length.out=100)
    yr <- seq(min(X[,2]), max(X[,2]), length.out=100)
    mg <- meshgrid(xr, yr)
    X1 <- mg[[1]]
    X2 <- mg[[2]]
    
    vals <- matrix(0, ncol=ncol(X1), nrow=nrow(X1))
    
    for (i in 1:ncol(X1)){
      thisX <- cbind(X1[,i], X2[,i])
      vals[,i] <- svmPredict(model, thisX)
    }
    
    vm <- data.frame(x=as.vector(X1),
                     y= as.vector(X2),
                     z=as.vector(vals))
    
    p <- p+geom_contour(data=vm, aes(x=x,y=y,z=z))
    
  }
  
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(title)
  
  print(p)
}










################run the data#####################
##linear_eparable
dataSim_linear1=simData_linear(sample_size=100,distance=6)
dataSim_linear2=simData_linear(sample_size=100,distance=4)
colnames(dataSim_linear)<-c("x","y","label")   

plot(dataSim_linear1[,1],dataSim_linear1[,2],col=ifelse(dataSim_linear1[,3]==1,"red","green"),xlab="x",ylab="y",main="Simdataset")

plot(dataSim_linear2[,1],dataSim_linear2[,2],col=ifelse(dataSim_linear2[,3]==1,"red","green"),xlab="x",ylab="y",main="Simdataset")
###separable
dataSim_linear=dataSim_linear1
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])
model=svmTrain(X,y,10000) ##hardmargin=10000
svmPlot_linear(dataSim_linear,model,tt="C=10000,datasets=separable")
model=svmTrain(X,y,10)
svmPlot_linear(dataSim_linear,model,tt="C=10,datasets=separable")
model=svmTrain(X,y,1)
svmPlot_linear(dataSim_linear,model,tt="C=1,datasets=separable")

###non separable

dataSim_linear=dataSim_linear2
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])
model=svmTrain(X,y,10000) ##hardmargin=10000
svmPlot_linear(dataSim_linear,model,tt="C=10000,datasets=non separable")
model=svmTrain(X,y,10)
svmPlot_linear(dataSim_linear,model,tt="C=10,datasets=non separable")
model=svmTrain(X,y,1)
svmPlot_linear(dataSim_linear,model,tt="C=1,datasets=non separable")


#########################
#################
#####################
########gaussian kernel#######

dataSim_linear=dataSim_linear1###separable
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])

model=svmTrain(X,y,1,kernelFunction="gaussianKernel")

svmPlot_nonlinear(model,X,y,type="nonlinear",title="dataset_sepearable,guassianKernel",xlab="v1",ylab="v2")


dataSim_linear=dataSim_linear2##non separable
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])

model1=svmTrain(X,y,1,kernelFunction="gaussianKernel")

svmPlot_nonlinear(model1,X,y,type="nonlinear",title="dataset_nonsepearable,guassianKernel",xlab="v1",ylab="v2")
#######################
#######polynominal kernel########
#######################
dataSim_linear=dataSim_linear1###separable
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])


model=svmTrain(X,y,1,kernelFunction="polynominalKernel")
svmPlot_nonlinear(model,X,y,type="nonlinear",title="dataset_sepearable,polynominalKernel",xlab="v1",ylab="v2")


###################################################
dataSim_linear=dataSim_linear2###non separable
X=as.matrix(dataSim_linear[,1:2])
y=as.matrix(dataSim_linear[,3])


model=svmTrain(X,y,1,kernelFunction="polynominalKernel")
svmPlot_nonlinear(model,X,y,type="nonlinear",title="dataset_non sepearable,polynominalKernel",xlab="v1",ylab="v2")

#################
###the first external datasets is the iris data
dataset1=iris[1:100,]
external.dataset1=vector()
datasett=round(dataset1[,1])###we are doing this grounding to make the feature become ordinal
for (i in 1:100){
  if (dataset1[i,5]=="versicolor")
    external.dataset1[i]=0
  else 
    external.dataset1[i]=1
}

external.dataset11=data.frame(datasett,dataset1[,c(-1,-5)],external.dataset1)###we 
external.dataset11[11,1]=-999
##make the dataset has missing features


#####now begin our training#####
missing.data=which(external.dataset11<0)
###we delete the missing value to make the dataset become a hilbert space
data1=external.dataset11[-(missing.data),]



#######cross validation########
k <- 10 #10 folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved") 
cm=matrix(0,2,2)
#using CVTools
for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata<- subset(data1, folds$which != i) 
  X.train=as.matrix(traindata[,1:2])
     y.train=as.matrix(traindata[,5])
  model=svmTrain(X.train,y.train,10,kernelFunction="gaussianKernel")
  X.test=as.matrix(testdata[,1:2])
  y.test=as.matrix(testdata[,5])
  predict.model=svmPredict(model,X.test)
  cm=table(predict.model,y.test)+cm
}
cm


k <- 10 #10 folds
folds <- cvFolds(nrow(data1), K = k, type = "interleaved") 
cm=matrix(0,2,2)
#using CVTools
for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata<- subset(data1, folds$which != i) 
  X.train=as.matrix(traindata[,1:2])
  y.train=as.matrix(traindata[,5])
  model=svmTrain(X.train,y.train,10,kernelFunction="polynominalKernel")
  X.test=as.matrix(testdata[,1:2])
  y.test=as.matrix(testdata[,5])
  predict.model=svmPredict(model,X.test)
  cm=table(predict.model,y.test)+cm
}
cm

############################################
###the second external datasets is the  data from andrew ng
#############################################
ex2data2 <- read.csv("D:/CS584/KUN_MEI_ass4/data/ex2data2.txt", header=FALSE)
data2<-as.data.frame(ex2data2)
data2<-as.matrix(data2)

k <- 10 #10 folds
folds <- cvFolds(nrow(data2), K = k, type = "interleaved") 
cm=matrix(0,2,2)
#using CVTools
for(i in 1:k){
  testdata  <- subset(data2, folds$which == i)
  traindata<- subset(data2, folds$which != i) 
  X.train=as.matrix(traindata[,1:2])
  y.train=as.matrix(traindata[,3])
  model=svmTrain(X.train,y.train,10,kernelFunction="gaussianKernel")
  X.test=as.matrix(testdata[,1:2])
  y.test=as.matrix(testdata[,3])
  predict.model=svmPredict(model,X.test)
  if(nrow(table(predict.model,y.test))==1)
     {cm[1,]=table(predict.model,y.test)+cm[1,]
  }else  
    cm=table(predict.model,y.test)+cm
}
cm

k <- 10 #10 folds
folds <- cvFolds(nrow(data2), K = k, type = "interleaved") 
cm=matrix(0,2,2)
#using CVTools
for(i in 1:k){
  testdata  <- subset(data2, folds$which == i)
  traindata<- subset(data2, folds$which != i) 
  X.train=as.matrix(traindata[,1:2])
  y.train=as.matrix(traindata[,3])
  model=svmTrain(X.train,y.train,10,kernelFunction="polynominalKernel")
  X.test=as.matrix(testdata[,1:2])
  y.test=as.matrix(testdata[,3])
  predict.model=svmPredict(model,X.test)
  cm=table(predict.model,y.test)+cm
}
cm




##delete some tuples from irisdata
data3=data1[-c(1:20),]

k <- 10 #10 folds
folds <- cvFolds(nrow(data3), K = k, type = "interleaved") 
cm=matrix(0,2,2)
#using CVTools
for(i in 1:k){
  testdata  <- subset(data3, folds$which == i)
  traindata<- subset(data3, folds$which != i) 
  X.train=as.matrix(traindata[,1:2])
  y.train=as.matrix(traindata[,5])
  model=svmTrain(X.train,y.train,1,kernelFunction="gaussianKernel")
  X.test=as.matrix(testdata[,1:2])
  y.test=as.matrix(testdata[,5])
  predict.model=svmPredict(model,X.test)
  cm=table(predict.model,y.test)+cm
}
cm

