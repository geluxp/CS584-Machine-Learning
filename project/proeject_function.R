require(tseries)
require(TTR)
require(quantmod)
require(randomForest) 
require(DMwR)
require(nnet)


#############
##this R file include all the major large functions that we will use in this project
#KUN MEI AND JUNZHE ZHENG 11.18.2014
##################


################techinal index##########################


## function use to transfer those index into a target function T.index, which is a indicator to predict wheter the stock
#would have a decreasing (T.ind<0) or increasing trend(T.ind>0)
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) { 
  v <- apply(HLC(quotes),1,mean) 
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes)) 
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x) 
  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin])) 
  if (is.xts(quotes)) 
    xts(x,time(quotes)) 
  else x }
#####################
### we use function in the pacakge qunatmod to simplify calculating those stock technical index
### THEY ARE
# ATR(average true range) SMI(Stochastic Momentum Indicato)
# ADX( Directional Movement Index) Aroon(Aroon Oscillator) 
# BB(Bollinger Bands) Chaikinvol(chaikinvolatility 
# CLV(Chaiken Volatility ) EMV( Arm's Ease of Movement)
# MACD(Moving Average Convergence / Divergence) MFI( Money Flow Index)
# SAR(Parabolic Stop and Reversal),volat(stock volatility)
myATR <- function(x) ATR(HLC(x))[,'atr'] 
mySMI <- function(x) SMI(HLC(x))[,'SMI'] 
myADX <- function(x) ADX(HLC(x))[,'ADX'] 
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator 
myBB <- function(x) BBands(HLC(x))[,'pctB'] 
myChaikinVol <- function(x) 
  Delt(chaikinVolatility(x[,c("High","Low")]))[,1] 
myCLV <- function(x) EMA(CLV(HLC(x)))[,1] 
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2] 
myMACD <- function(x) MACD(Cl(x))[,2] 
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"]) 
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1] 
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]
###################end of this part######################################



#################
##################
####main function#################
#################

##############
prepro_data<-function(type=2,GSPC,DGS10,DAAA)
{
 
  ###############get the financial data##########################

  
  
  
  
  
  
  
  ##################bulidng up model################################
  if (type==1){
    ####To facilitate this relatively unique data issue, 
    #quantmod dynamically creates data objects for use within the modelling process, 
    #creating a model frame internally after going through a series of steps to identify the sources of data required 
    #- loading if necessary. specifyModel is the workhorse function to handle all the data issues, and it's
    #help file should be read to fully understand what is happening internally. For our purposes here,
    #it is enough to know that one can specify ANY data within the call to specifyModel, and quantmod will 
    #handle to lookup and data aggregation for you. Of course the data has to be locatable and unique, 
    #but that was probably suspected.
    #we use this function to merge them into the same size and date scale.
    data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                                 myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) 
                               + myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) + 
                                 CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                                 myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + 
                                 RSI(Cl(GSPC)) + mySAR(GSPC) + runMean(Cl(GSPC)) + 
                                 runSD(Cl(GSPC))+DGS10+DAAA) ##adding DGS 
    ####fetch the preprocessed data############## 
    data=modelData(data.model)
    
  } else if(type==2) {
    ####To facilitate this relatively unique data issue, 
    #quantmod dynamically creates data objects for use within the modelling process, 
    #creating a model frame internally after going through a series of steps to identify the sources of data required 
    #- loading if necessary. specifyModel is the workhorse function to handle all the data issues, and it's
    #help file should be read to fully understand what is happening internally. For our purposes here,
    #it is enough to know that one can specify ANY data within the call to specifyModel, and quantmod will 
    #handle to lookup and data aggregation for you. Of course the data has to be locatable and unique, 
    #but that was probably suspected.
    #we use this function to merge them into the same size and date scale.
    data.model.without<- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                                        myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) 
                                      + myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) + 
                                        CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                                        myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + 
                                        RSI(Cl(GSPC)) + mySAR(GSPC) + runMean(Cl(GSPC)) + 
                                        runSD(Cl(GSPC))) 
    ####fetch the preprocessed data##############
    ###use modelData to get a visualable datasets.
    data=modelData(data.model.without)
  } else (print("please enter a correct type "))## warining messge if you are typing a invalid type.
  
  return(data)
}

###we use this function to test the performance of this prediction system in the real world investment.
#### we assume we all buy/sell the stock at the beginning of the market.
test.return<-function(data.return,Tform.test)
{
  basefund=100000
  baseshare=0
  for (i in 1:length(Tform.test)){
    if (Tform.test[i]=='buy'){        #let's buy some stocks
      if (basefund!=0){
        
        baseshare=basefund/data.return[i,2]
        basefund=0
      }
    }else  if (Tform.test[i]=='sell'){
      if (baseshare!=0) {             ## sell our stocks
        basefund=baseshare*data.return[i,2]
        baseshare=0
      }
    }  
  }
  
  if (basefund==0)
  {basefund=baseshare*data.return[i,2]}
  
  rate=(basefund/100000)-1
  
  index.rate=(data.return[nrow(data.return),4]-data.return[1,1])/data.return[1,1]
  
  
  ### the return rate means the profit rate we could get from which we follow the signal to buy/sell stocks.
  ### the corrspongding index move indicates the SPX index performance in corresponding period
  ### the adjusted rate means the adjusted rate towards the  SPX index performance,if we get a postive value, it means
  ## we beat the market,otherwise we fail the market.
  cat("\nthe return rate is\n",rate,"\nthe corresponding index move is\n",  index.rate ,"\nthe adjusted retrun rate is\n",rate-index.rate)
  
}

stockpredict<-function(data,Tform,method="nn",GSPC,dummy.test)
{

  Tdata.raw=data[,-1]
  Tdata.raw=as.matrix(Tdata.raw)
  if (TRUE){
    Tform.train=Tform[1:floor(length(Tform)*0.8)]
    Tform.test=Tform[floor(length(Tform)*0.8):length(Tform)]
    
    Tdata.train<-Tdata.raw[1:(length(Tform)*0.8),]
    Tdata.train.norm <- scale(Tdata.train) # scalethe datasets
    stock<-data.frame(Tform.train,Tdata.train.norm)
    
    ##doing nnet and logistic regreesion training 
    model.nn.simple <- nnet(Tsimple~.,linout = F,size = 10, decay = 0.01,maxit = 1000,trace = F,data = stock)
    model.logistic=multinom(Tform.train~., data=stock)
    
    ##get the testing dataset 
    Tdata.test<-Tdata.raw[floor(length(Tform)*0.8):length(Tform),]
    Tdata.test.norm <- scale(Tdata.test) #scale the datesets
    stock.test<-data.frame(Tform.test,Tdata.test.norm)
    
    nn.preds <- predict(model.nn, stock.test,type='class')
    logistic.preds<-predict(model.logistic, stock.test, type='class')
    nn.table=table(nn.preds,Tform.test)
    logistic.table=table(logistic.preds,Tform.test)
    
    ##########################################
    
    
   
    model.return<-specifyModel(GSPC ~ dummy.test)
    data.return<-modelData(model.return)
    data.return<-as.matrix(data.return)
    
    ####investment based the predicting system on the subseqential days######
    
    cat("\n predicting using neroun netowrk\n ")
    test.return(data.return, nn.preds)
    cat("\n predicting using logistic regression\n")
    test.return(data.return,logistic.preds)
    
    
    
  }
  
  if(method=="nn")
    return(nn.table)
  else return(logistic.table)
 # return(confusionmatrix)
}


T.signal<-function(Tform.raw){
  Tform=vector()
  for (i in 1:(length(Tform.raw)-1))
  {if(Tform.raw[i]<(-0.1))
    Tform[i]='sell'
   else if(Tform.raw[i]>=(-0.1)&&Tform.raw[i]<=(0.1))
     Tform[i]='hold'
   else {Tform[i]='buy'
   }
  }
  return(Tform)
}