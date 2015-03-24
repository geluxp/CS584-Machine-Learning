require(tseries)
require(TTR)
require(quantmod)
require(randomForest) 
require(DMwR)
require(nnet)

startdate="2005-01-02"
enddate="2010-01-10"
GSPC <- as.xts(get.hist.quote("^GSPC",start=startdate, end=enddate,quote=c("Open", "High", "Low", "Close","Volume")))
barChart(GSPC)
getSymbols('DGS10',src='FRED')
getSymbols('DAAA',src='FRED')
############Type of pre_processing data###########################
simpledata=GSPC #data without pre_processing
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) { 
  v <- apply(HLC(quotes),1,mean) 
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes)) 
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x) 
  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin])) 
  if (is.xts(quotes)) 
    xts(x,time(quotes)) 
  else x }
T.simple=T.ind(simpledata)
simple.data=specifyModel(T.simple~simpledata)
simple.data.model=modelData(simple.data)
model.simple <- nnet(T.simple~.,linout = F,size = 10, decay = 0.01,maxit = 1000,trace = F,data = simple.data.model)
model.logistic=multinom(Tform.train~., data=stock)

####################
data<-prepro_data(type=1,GSPC,DGS10,DAAA)  
# the default setting of this function is as follow: 
#startdate="2012-01-02"  to load the spx(standard and poor data) from 2012-01-02 to present 
#type=2 load the data without financial index (DGS10) and (DAAA)

#################split the data into different training and test data##########

##########technique "moving window
####tagret data##


Tform.raw=data[,1]
Tform.raw=as.matrix(Tform.raw)
Tform=vector()

###
##T.signal function is used to turn the continuoesly value T.ind into a nominal value sets{'buy','hold','sell'} 
#in order to make real world decision to help the investor buy/sell stock


Tform<-T.signal(Tform.raw)


#####we will do the testing in two ways:AAAAAAAAAA
###first we just roughly splitt the period into two parts: the training datasets are the first 80% days,
### the testing datasets are the subsequential 20% trading days
###second, we would use a validation method specified for time series data ,which is called "moving window"
##method
###
###method 1 : normal roughly testing
dummy.test<-data[floor(length(Tform)*0.8):length(Tform),-1]
result.matrix=stockpredict(data,Tform,method="nn",GSPC,dummy.test)
accu=sum(diag(result.matrix))/sum(result.matrix)
result.matrix=stockpredict(data,Tform,method="logistic",GSPC,dummy.test)
accu=sum(diag(result.matrix))/sum(result.matrix)
###
############
###method 2 : moving window testing
window=1000
steplength=500

window.iteration=floor((length(Tform)-window)/steplength)# calculating how many iteration would be using this "moving window" method
confusionmatrix=matrix(0,3,3)
 for ( i in   1:window.iteration)
   { 
       start=1+(i-1)*steplength
       end=(i-1)*steplength+window
       Tform.iteration=Tform[start:end]
       data.iteration=data[start:end,]
       test.dumm.iterationt=data.iteration[floor(length(Tform.iteration)*0.8):length(Tform.iteration),-1]
       result.matrix=stockpredict(data.iteration,Tform.iteration,method="nn",GSPC,test.dumm.iteration)
       confusionmatrix=confusionmatrix+result.matrix
      
    }
confusionmatrix
accu=sum(diag(confusionmatrix))/sum(confusionmatrix)