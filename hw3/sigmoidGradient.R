sigmoidGradient<-function(z){
  g=zeros(size(z))
  
  g=exp(-z)/ ((1+exp(-z))^2)}