library(RSNNS)
library(nnet)
library(devtools)
library(cvTools)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
set.seed(1)  

url <- "C:/Users/jessica/Documents/media1_C.csv"
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
  x1[i] <- as.numeric(as.character(dados[j, ]))
  x2[i]<- as.numeric(as.character(dados[j+1, ]))
  x3[i]<- as.numeric(as.character(dados[j+3, ]))
  x4[i]<- as.numeric(as.character(dados[j+4, ]))
  x5[i]<- as.numeric(as.character(dados[j+5, ]))
  x6[i]<- as.numeric(as.character(dados[j+6, ]))
  x7[i]<- as.numeric(as.character(dados[j+7, ]))
  x8[i]<- as.numeric(as.character(dados[j+8, ]))
  x9[i]<- as.numeric(as.character(dados[j+9, ]))
  x10[i]<- as.numeric(as.character(dados[j+10, ]))
  y[i]<- as.numeric(as.character(dados[j+11, ]))
  j<- j+11
}

x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

xn <-  (x-(mean(x))/(sd(x)))
yn <-  (y-(mean(y))/(4*sd(y)))

xn <- normalizeData(x, type = "norm")
yn <- normalizeData(y, type = "norm")

th <- c(8,8,8)
max <- 500


j = 0;
k = 0;
treinox <- c()
treinoy <- c()
testex <-c()
testey <-c()

for(i in 0:((3929/11)-1)){
  if(i%%4==0){
    testex[j, ] <- xn[i, ]
    testey[j] <- yn[i]
    j<- j+1
    
  }else{
    treinox[k, ]<- xn[i, ]
    treinoy[k]<- yn[i]
    k <- k+1
  }
}
model2<-mlp(treinox,treinoy,size= th,maxit= max,learnFunc="Rprop",linOut=TRUE)
#plot.nnet(model)

aweightMatrix(model2)
extractNetInfo(model2)
par(mfrow=c(2,2))
plotIterativeError(model2)

predictions<-predict(model2,testex)   # now gives 500 unique predictions
plot(predictions,type="l")  
lines(testey,col="4")

plot(testey,type="l")
lines(testey,col="4")

#plotRegressionError(valor$targetsTrain,valor$targetsTrain)
#plotRegressionError(valor$targetsTest,predictions)
#plotROC(predictions,valor$targetsTest)

m = nrow(predictions)

rmse2 <- sqrt(1/m*sum((testey - predictions)^2))

mse2 <- 1/m*sum((testey - predictions)^2)

mae2<- 1/m*sum(abs(testey - predictions))
