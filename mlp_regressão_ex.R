library(RSNNS)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

set.seed(1)  
e<-rnorm(500,sd=100)
x<-seq(1:500)

y<-2*x+e
th <- 5

model_mlp<-mlp(x,y,size= c(3,3),maxit=200,learnFunc="Rprop",linOut=TRUE)
plot.nnet(model_mlp)

predictions_mlp<-predict(model_mlp,t(t(x)))   # now gives 500 unique predictions
plot(predictions_mlp,type="l")  
lines(y,col="4")
