library(RSNNS)
library(nnet)
library(devtools)
library(cvTools)
url <- "C:/Users/jessica/Documents/petr4_tratado2.csv"
dados <- read.table(file=url,header=TRUE,sep=";")
dados
summary(dados)

x <- cbind(dados$Open,dados$High,dados$Low,dados$Close)
#xomin <- pmin(x[,1])
#xomax <- pmax(x[,1])
y <- c(dados$Med)

eta <- 0.1
htam <- 5
max <- 100
xn <- x - mean(x) / sd(x)
yn <-  y - mean(y)/ (4*sd(y))
tjanela <- 11
tx <- nrow(x)
contorno <- tx/tjanela
valor <- tx * 0.75
cont <- 0
for(i in 0:contorno){
  
  if(i%%4 == 0){
    testex <- x[cont:(cont+11), ]
    testey <- y[cont:(cont+11)]
  }
  else{
    treinox <- x[cont:(cont+11), ]
    treinoy <- y[cont:(cont+11)]
  }
  cont <- cont+11
}


model<-mlp(xn[1:valor, ], yn[1:valor], size=htam ,maxit=max ,learnFunc=eta)
plot.nnet(model)

weightMatrix(model)
extractNetInfo(model)
par(mfrow=c(2,2))
plotIterativeError(model)


predictions_mlp<-predict(model,val$inputsTest)   # now gives 500 unique predictions
plot(predictions_mlp,type="l")  
lines(y,col="4")




