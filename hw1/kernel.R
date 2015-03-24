##clear the memory
rm(list = ls())

data1<-read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set1.dat")
data<-data1



x<-data[,1:2]


y<-data[,3]
d<-2 #number of features of x
z<-x
rx<-z
N=length(y)
rn=length(y)     
x1<-rx
yhat<-matrix(0,ncol=1,nrow=N)

kerf<-function(z)
{exp(-rowSums(z*z)/sqrt(0.05))}
             
       
  for (i in 1:N){    
    xx<-abs(x-x1[(matrix(0,nrow=rn,ncol=1)+i),])
    z<-kerf(xx)
    yhat[i]<-sum((z*y))/sum(z)
  }



MSETrain<-sum((yhat-y)^2)/N