library(RSNNS)
library(nnet)
library(devtools)
library(cvTools)
library(reshape)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
set.seed(1)  

url <- "C:/Users/jessica/Documents/Doutorado/Redes Neurais/media1_C.csv"
dados <- read.table(file=url,header=FALSE,sep=";")
dados
summary(dados)
as.numeric(levels(dados))

x1 <- c()
x2 <- c()
x3 <- c()
x4 <- c()
x5 <- c()
x6 <- c()
x7 <- c()
x8 <- c()
x9 <- c()
x10 <- c()
y <- c()
linnha <- 3929/11
j <- 0

for(i in 0:((3929/11)-1)){
  x1[i] <- as.numeric(dados[j, ])
  x2[i]<- as.numeric(dados[j+1, ])
  x3[i]<- as.numeric(dados[j+3, ])
  x4[i]<- as.numeric(dados[j+4, ])
  x5[i]<- as.numeric(dados[j+5, ])
  x6[i]<- as.numeric(dados[j+6, ])
  x7[i]<- as.numeric(dados[j+7, ])
  x8[i]<- as.numeric(dados[j+8, ])
  x9[i]<- as.numeric(dados[j+9, ])
  x10[i]<- as.numeric(dados[j+10, ])
  y[i]<- as.numeric(dados[j+11, ])
  j<- j+11
}

x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

xn <-  (x-(mean(x))/(sd(x)))
yn <-  (y-(mean(y))/(4*sd(y)))

xn <- normalizeData(x, type = "norm")
yn <- normalizeData(y, type = "norm")

valor <- splitForTrainingAndTest(xn, yn, ratio=0.25)

th <- c(8,8,8)
max <- 500


model<-mlp(valor$inputsTrain,valor$targetsTrain,size= th,maxit= max,learnFunc="Std_Backpropagation",linOut=TRUE)
plot.nnet(model) #plot do layout fa rede com as entradas e saida

weightMatrix(model)
extractNetInfo(model)
par(mfrow=c(2,2))
plotIterativeError(model)

predictions<-predict(model,valor$inputsTest)   # now gives 500 unique predictions


plot(valor$targetsTest,type="l",col="6") # plot do valor desejado
lines(predictions) #plot da linha estimada


m = nrow(predictions)

rmse <- sqrt(1/m*sum((denormalizeData(valor$targetsTest,getNormParameters(yn)) - denormalizeData(predictions,getNormParameters(yn)))^2))

mse <- 1/m*sum((denormalizeData(valor$targetsTest,getNormParameters(yn)) - denormalizeData(predictions,getNormParameters(yn)))^2)

mae<- 1/m*sum(abs((denormalizeData(valor$targetsTest,getNormParameters(yn)) - denormalizeData(predictions,getNormParameters(yn)))))