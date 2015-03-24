#else if (kernelFunction =="polynominalKernel"){
# K <- matrix(0, ncol=m, nrow=m)
# K<-polykernel(X)

#else if (kernelFunction =="polynominalKernel"){
# K <- matrix(0, ncol=m, nrow=m)
# K<-polykernel(X)
simData_linear=function(sample_size,distance)##if distance=4 it is non-separable  ;if distance=6,it is separable
{aa1=runif(sample_size/2)
 aa2=runif(sample_size/2)
 x1=aa1
 x2=aa1
 y1=x1*5
 y2=x1*5+distance
 x1=x1+aa2
 x2=x2+aa2
 x.1=vector()
 x.2=vector()
 label1=1*rep(1,length(x1))
 label2=0*rep(1,length(x2))
 data=matrix(rep(0,3*sample_size),nrow=sample_size,ncol=3)  
 data[,1]=c(x1,x2)  
 data[,2]=c(y1,y2)  
 data[,3]=c(label1,label2)
 
 data
}

dataSim_linear=simData_linear(sample_size=100,distance=6)
colnames(dataSim_linear)<-c("x","y","label")   
plot(dataSim_linear[,1],dataSim_linear[,2],col=ifelse(dataSim_linear[,3]==1,"red","green"),xlab="x",ylab="y",main="Simdataset")