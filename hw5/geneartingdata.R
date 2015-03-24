
mvrnorm <-function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE, EISPACK = FALSE)
  {
    p <- length(mu)
    eS <- eigen(Sigma, symmetric = TRUE, EISPACK = EISPACK)
    ev <- eS$values
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n, byrow = TRUE)
    if(empirical) {
      X <- scale(X, TRUE, FALSE) # remove means
      X <- X %*% svd(X, nu = 0)$v # rotate to PCs
      X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
    nm <- names(mu)
    if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if(n == 1) drop(X) else t(X)
  }
Generating<-function()
{
  Sigma <- matrix(c(0.2,0.1,0.1,0.2),2,2)
  
  temp1=mvrnorm(n=50, rep(0, 2), Sigma)
  temp2=mvrnorm(n=50, rep(0, 2), Sigma)
  temp2[,2]=temp2[,2]+2
  temp2[,1]=temp2[,1]+2
  label1=1*rep(1,nrow(temp1))
  label2=0*rep(1,nrow(temp2))
  data=matrix(rep(0,3*100),nrow=100,ncol=3)  
  data[,1]=c(temp1[,1],temp2[,1]) 
  data[,2]=c(temp1[,2],temp2[,2])
  data[,3]=c(label1,label2)
  
  data
}

dataSim=Generating()
colnames(dataSim)<-c("x1","x2","label")   
plot(dataSim[,1],dataSim[,2],col=ifelse(dataSim[,3]==1,"red","green"),xlab="x1",ylab="x2",main="Simdataset")


X=dataSim[,1:2]

###polynominal mapping###
X.poly=matrix(rep(0,3*100),nrow=100,ncol=5)
X.poly[,1:2]=X
X.poly[,3]=X[,1]*X[,2]
X.poly[,4]=X[,1]^2
X.poly[,5]=X[,2]^2

##linear mapping####
X.linear=matrix(rep(0,3*100),nrow=100,ncol=5) 
X.linear[,1:2]=X
X.linear[,3]=X[,1]*0.3+X[,2]*0.7
X.linear[,4]=X[,1]*0.6+X[,2]*0.4
X.linear[,5]=X[,1]*0.8+X[,2]*0.2





















