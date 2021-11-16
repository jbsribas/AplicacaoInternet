library(RSNNS)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
set.seed(1)  

url <- "C:/Users/jessica/Documents/teste3.csv"
dados <- read.table(file=url,header=TRUE,sep=";")
dados
summary(dados)
#x <- cbind(dados$Open,dados$High,dados$Low,dados$Close)
x <- cbind(dados$x1,dados$x2,dados$x3,dados$x4,dados$x5,dados$x6,dados$x7,dados$x8,dados$x9,dados$x10)
y <- c(dados$y)

xn <- (x -(mean(x))/(sd(x)))
yn <-  (y-(mean(y))/(4*sd(y)))


plot(y,type="l")


th <- c(8,8,8)
max <- 500
valor <- splitForTrainingAndTest(xn, yn, ratio=0.25)
valor <- normTrainingAndTestSet(valor)

model<-mlp(valor$inputsTrain,valor$targetsTrain,size= th,maxit= max,learnFunc="Rprop",linOut=TRUE)
#plot.nnet(model)

weightMatrix(model)
extractNetInfo(model)
par(mfrow=c(2,2))
plotIterativeError(model)

predictions<-predict(model,valor$inputsTest)   # now gives 500 unique predictions
plot(predictions,type="l")  
lines(valor$targetsTest,col="4")

plotRegressionError(valor$targetsTest,predictions)
