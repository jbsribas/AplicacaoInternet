library(RSNNS)
library(nnet)
library(devtools)
library(cvTools)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
set.seed(1)  

url <- "C:/Users/jessica/Documents/media1_C.csv"
dados2 <- read.table(file=url,header=TRUE,sep=";")
dados2
summary(dados2)
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
treino <- c()
teste <-c()
linnha <- (3929/11)-1
j <- 0

for(i in 0:((3929/11)-1)){
  x1[i] <- dados2[j, ]
  x2[i]<- dados2[j+1, ]
  x3[i]<- dados2[j+3, ]
  x4[i]<- dados2[j+4, ]
  x5[i]<- dados2[j+5, ]
  x6[i]<- dados2[j+6, ]
  x7[i]<- dados2[j+7, ]
  x8[i]<- dados2[j+8, ]
  x9[i]<- dados2[j+9, ]
  x10[i]<- dados2[j+10, ]
  y[i]<- dados2[j+11, ]
  j<- j+11
}
novoDado <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,y)
j = 0;
k = 0;

for(i in 0:((3929/11)-1)){
  if(i%%4==0){
    teste[j] <- novoDado[i, ]
    j<- j+1
      
  }else{
    treino[k]<- novoDado[i, ]
    k <- k+1
      }
}
