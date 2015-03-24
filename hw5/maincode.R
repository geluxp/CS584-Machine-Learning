require(R.matlab)
require(matlab)
require(psych)
##########
###PCA#####
##########

iris=iris[,1:4]
iris=as.matrix(iris)
wine=read.table("D:/KUN_MEI_ASS5/data/wine.txt",,sep=',')
wine=as.matrix(wine)
wine=wine[1:50,2:ncol(wine)]



###PCA method and plot 2D plot
PCA2d<-function(data)
{ 
  data.sd=scale(data)
 data.cor=cor(data.sd)
 data.eigen=eigen(data.cor)
###since we only need the first 2 dimensions to rubuild the method.
  transformmatrix=data.eigen$vectors[,1:2]
  data.result=data.sd%*%(transformmatrix)
  data.result=cbind(data.result,1)
label1=1*rep(1,50)
label2=0*rep(1,50)
data.result[,3]=c(label1,label2)
plot(data.result[,1],data.result[,2],col=ifelse(data.result[,3]==1,"red","green"),xlab="x1",ylab="x2",main="dataset rebuild using PCA")
}

faa=function(X,p)
{
  ####pre processing#####
  d=nrow(X)
  n=ncol(X)
  mu=rowMeans(X)
  
  X=X-mu
  
  
  
  tol = 1e-3;
  converged = FALSE
  llh = -10000
  
  ## initialize guessing parameters
  W = matrix(runif(d*p,0,1),nrow=d,ncol=p)
  invpsi=1/matrix(runif(d,0,1),nrow=d,ncol=1)
  
  ### precompute quantities
  I = eye(p);
  normX = rowSums(X^2)
  
  
  
  U=matrix(0,nrow=d,ncol=p)
  for (i in 1:ncol(W))
    U[,i]=W[,i]*sqrt(invpsi)
  
  
  M = t(U)%*%U+I  ### M = W'*inv(Psi)*W+I
  R = chol(M)
  invM= solve(R,solve(t(R),I))
  
  temp=matrix(0,nrow=d,ncol=p)
  for (i in 1:ncol(W))
    temp[,i]=W[,i]*(invpsi)
  
  
  WinvPsiX = t(temp)%*%X   ##    % WinvPsiX = W'*inv(Psi)*X
  
  
  
  while (!converged)
  {   # E step
    Ez = invM%*%WinvPsiX
    Ezz = n*invM+Ez%*%t(Ez)
    
    
    R = chol(Ezz);  
    XEz = X%*%t(Ez)
    
    # M step
    W=(XEz%*%solve(R))%*%solve(t(R))
    invpsi = n/(normX-rowSums(W*XEz))
    ##end    
    
    ## compute quantities needed
    for (i in 1:ncol(W))
      U[,i]=W[,i]*sqrt(invpsi)
    
    M = t(U)%*%U+I  ### M = W'*inv(Psi)*W+I
    R = chol(M)
    invM= solve(R,solve(t(R),I))
    
    for (i in 1:ncol(W))
      temp[,i]=W[,i]*(invpsi)
    WinvPsiX = t(temp)%*%X   ##    % WinvPsiX = W'*inv(Psi)*X
    
    ##end
    
    ## likelihood
    last = llh
    logdetC = 2*sum(log(diag(R)))-sum(log(invpsi))               ###log(det(C))
    
    trinvCS = (t(normX)%*%invpsi-sum(sum((solve(t(R),WinvPsiX)^2))))/n  ### trace(inv(C)*S)
    llh = -n*(d*log(2*pi)+logdetC+trinvCS)/2
    ### end
    converged = (abs(llh-last) < tol*abs(llh))  # check likelihood for convergence
  }
  psi = 1/invpsi
  result=list()
  result$W=W
  result$psi=psi
  return(result)
  
  return(result)
}

FA2d<-function(data)
 { data.result=faa(data,2)
   data.result=cbind(data.result,1)
   label1=1*rep(1,50)
   label2=0*rep(1,50)
   data.result[,3]=c(label1,label2)
   plot(data.result[,1],data.result[,2],col=ifelse(data.result[,3]==1,"red","green"),xlab="x1",ylab="x2",main="dataset rebuild using EM.FA")
}


PCA.analyze<-function(data)
{
  data.sd=scale(data)
  data.cor=cor(data.sd)
  data.eigen=eigen(data.cor)
  
  data.values=data.eigen$values
  plot(c(1:nrow(data.cor)),data.values, type="b")
  return(data.values)
}


  
FA.anaylze<-function(data)
{ 
  W=faa(wine,13)$W
  W.cor=cor(W)
  eigen=eigen(W.cor)$values
  sum.eigen=sum(eigen)
  eigen.norm=vector()
  for (i in 1:length(eigen))
  {eigen.norm[i]=eigen[i]/sum.eigen}
  plot(c(1:length(eigen.norm)),eigen.norm,type="b")
  return(eigen.norm)
} 
  
  data.value=PCA.analyze(iris)  
  data.value1=FA.anaylze(iris)
  
plot(c(1:length(data.value)),data.value,type="b",col="red")
par(new=TRUE)
plot(c(1:length(data.value1)),data.value1,type="b")

data.value=PCA.analyze(wine)  
data.value1=FA.anaylze(wine) 
  
plot(c(1:length(data.value)),data.value,type="b",col="red")
par(new=TRUE)
plot(c(1:length(data.value1)),data.value1,type="b")



automatic_detect=function(data)
{data.sd=scale(data)
 data.cor=cor(data.sd)
 data.eigen=eigen(data.cor)
 ###since we only need the first 2 dimensions to rubuild the method.
 data.values=data.eigen$values
 cumuvalue=0
 number.factor=0
 while (cumuvalue<0.8*(sum(data.values)))
  {number.factor=number.factor+1
   cumuvalue=cumuvalue+data.values[number.factor]
 }
 
 return(number.factor)
}























