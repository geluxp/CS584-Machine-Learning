############
##nnMain
##########
require(RSNNS)
require(nnet)
require(cvTools)
require(R.matlab)
require(matlab)
#################################ex3data1 from Andrew Ng coursera course mahine learning############################################################

data=readMat("D:/KUN_MEI_ASS3/DATA/ex4data1.mat")

##manupulating data
rawX=as.matrix(as.data.frame(data[1]))
rawy=as.matrix(as.data.frame(data[2]))
X=rawX[c(1:1500),]
y=rawy[c(1:1500),]

m=size(X,1)

for (i in 1:500)
{y[i]=0}

output1=t(y)
input=X

output=eye(1500,3)
## change the output1 from 1 dimisional to 3 dimisonal
for(i in 1:1500)
  {  if (y[i]==0)
      { output[i,]=c(1,0,0)
    } else if (y[i]==1)
      {output[i,]=c(0,1,0)
    } else if (y[i]==2)
      {output[i,]=c(0,0,1)}    
}
#####################################################################################################
input_layer_size=400
############################################
hidden_layer_size = 25 ###number of hidden layer
######################################
num_labels = 3
innum=400
midnum=25
outnum=3

#####################################################
###########################dataset from the www.ilovematlab.com #####################################
###################################################################
c1=readMat("D:/KUN_MEI_ASS3/CODE/data1.mat")
c2=readMat("D:/KUN_MEI_ASS3/CODE/data2.mat")
c3=readMat("D:/KUN_MEI_ASS3/CODE/data3.mat")


data=matrix(NaN,nrow=1500,ncol=25)
data[1:500,]=as.matrix(as.data.frame(c1[1]))
data[501:1000,]=as.matrix(as.data.frame(c2[1]))
data[1001:1500,]=as.matrix(as.data.frame(c3[1]))

input=data[,2:25]
y=data[,1]
output=matrix(NaN,nrow=1500,ncol=3)
for(i in 1:1500)
{  if (y[i]==1)
{ output[i,]=c(1,0,0)
} else if (y[i]==2)
{output[i,]=c(0,1,0)
} else if (y[i]==3)
{output[i,]=c(0,0,1)}    
}

input_layer_size=24
############################################
hidden_layer_size = 25 ###number of hidden layer
######################################
num_labels = 3

innum=24
midnum=10
outnum=3
######################################################################
#######################################################
###############################################
########################learning rate#####################################
xite=0.1
alfa=0.1####################
#########################################################
#######################



myNormalize <- function (target) {
  (target - min(target))/(max(target) - min(target))
}







nnMain<-function(input1,output,xite,alfa,innum,midnum)
{

input<-myNormalize(input1)


############################################


##################




### neuron network initialization######################  midnum=25

outnum=3


w1=matrix(runif(midnum*innum,-1,1),midnum,innum)
b1=matrix(runif(midnum,-1,1),midnum,1)
w2=matrix(runif(midnum*outnum,-1,1),midnum,outnum)
b2=matrix(runif(outnum,-1,1),outnum,1)


w2_1=w2;w2_2=w2_1;
w1_1=w1;w1_2=w1_1;
b1_1=b1;b1_2=b1_1;
b2_1=b2;b2_2=b2_1;

dw1=matrix(0,ncol=midnum,nrow=innum)
db1=matrix(0,nrow=midnum)

###################################training part###########################
E=vector()
I=vector()
Iout=vector()
yhatt=vector()
ytruee=vector()
k <- 10 #10 folds
folds <- cvFolds(nrow(output), K = k, type = "interleaved")  #using CVTools
confusionMatrix <- matrix(0,3,3) 
 for (tt in 1:k)
{
input_test  <- subset(input, folds$which == tt)
input_train <- subset(input, folds$which != tt) 
output_test<-subset(output, folds$which == tt)
output_train <- subset(output, folds$which !=tt) 
#####################
nn=sample(1:1500, size = 1500)
mm=sample(1:1500, size = 150)
input_train=input[nn,];
output_train=output[nn,];
input_test=input[mm,]
output_test=output[mm,]
for (ii in 1:1)
      {    E[ii]=0
        for (i in 1:1350)
         {  ###network predict output
            x=input_train[i,]
            ##hidden layer output
            for (j in 1:midnum)
            {  I[j]=input_train[i,]%*%w1[j,]+b1[j]
               Iout[j]=1/(1+exp(-I[j]))
            }
            ##output layer output
            yn=t(w2)%*%Iout+b2
            
            ## value adjustion
            e=output_train[i,]-yn
            E[ii]=E[ii]+sum(abs(e))
            ## calculating the value changing ratio
            dw2=e%*%Iout
            db2=t(e)
            FI=vector()
            
            for (j in 1:midnum)
            { S=1/(1+exp(-I[j]))
              FI[j]=S*(1-S)
            }
            
            for (k in 1:innum)
            {   for (j in 1: midnum)
                 {dw1[k,j]=FI[j]%*%x[k]%*%( e[1]%*%w2[j,1]+ e[2]%*% w2[j,2]+ e[3]%*% w2[j,3])
                  db1[j]=FI[j]*( e[1]%*%w2[j,1]+ e[2]%*% w2[j,2]+ e[3]%*% w2[j,3])
                 }
             }
            
            w1=w1_1+xite*t(dw1)+alfa*(w1_1-w1_2);
            b1=b1_1+xite*(db1)+alfa*(b1_1-b1_2);
            w2=w2_1+xite*t(dw2)+alfa*(w2_1-w2_2);
            b2=b2_1+xite*t(db2)+alfa*(b2_1-b2_2);
            
            w1_2=w1_1;w1_1=w1;
            w2_2=w2_1;w2_1=w2;
            b1_2=b1_1;b1_1=b1;
            b2_2=b2_1;b2_1=b2;
        }
}
            fore=matrix(0,150,3)
##############################doing prediction#############################################       
for (ii in 1:1)
     { for (i in 1:150)
               {for (j in 1:midnum)
                    {I[j]=input_test[i,]%*%w1[j,]+b1[j]
                                                Iout[j]=1/(1+exp(-I[j]))
                                               }
                    fore[i,]=t(w2)%*%Iout+b2
                    }
          }


yhat=vector()
ytrue=vector()
for (i in 1:150){
yhat[i]=which.max(fore[i,])
ytrue[i]=which.max(output_test[i,])}


yhatt[((tt-1)*150+1):((tt)*150)]=yhat
ytruee[((tt-1)*150+1):((tt)*150)]=ytrue


}
confusionMatrix <- confusionMatrix + table(yhatt,ytruee)
return(confusionMatrix)
}



nnread<-function(input1,output,xite,alfa,innum,midnum)
{a=nnet(input,y,size=25,rang=0.1,decay=5e-4,maxit=10000))}
 a.predict=predict(a,input)
 table(a.predict,y)
test=nnMain(input,output,xite,alfa,innum,midnum)
precious=sum(diag(test))/sum(rowSums(test))
precious
