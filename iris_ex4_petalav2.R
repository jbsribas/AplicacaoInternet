#PERCEPTRON DE ROSEMBLATT
library(cvTools)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
data(iris)
head(iris)
summary(iris)

iris3 <- iris[iris$Species != "versicolor", ]
summary(iris3)
    
#entradas
X <- cbind(iris3[ ,3],iris3[ ,4],1)

#iris3$Species=="setosa"

#saida
y <- c(iris3[ ,5] )

for (j in 1:100){
  if(y[j] == 1){
    y[j]<- 1
  }else{
    y[j] <- 0
  }
  }

#inicialização pesos
w <- c(0,0,0.7)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.4
atingiu <- 0


erro <- 0
erroAnterior <- 0
e <- numeric(100)
eps_custo <- numeric(100)
#vr <- string(100)


#treinamento
#for(epoca in 1: 100){
  
#for(i in 1:100){
  v <- sum(X[1,]*w)
   if (v >1){
       y_calc <- 1
   }else {
     y_calc <-0}

  table (y[1],y_calc)
   
  
  
  if ( v > 1){
   y_calc <- 1
  }else{
     y_calc <- 0
  }
  #vr[i] <- teste
  
  erroAnterior <- erro
  erro <- ((y[i]-v)^2)/2
  e[i] <- erro
  
  if((abs(erroAnterior - erro)) > 0.05 & (atingiu < 1)){
    delta <- (eta*(y[i]-v)*X[i,])
    w <- w + delta	
    atingiu <- 0
    
  }  else {
    atingiu <- 1
    
  }
  
}
eps_custo[epoca] <- sum(e)

par(cex=1.5,las=1)
with(iris,plot(Petal.Length, Petal.Width,type='n',xlab='Comprimento da petala',ylab='Largura da Petala'))
with(iris[which(iris$Species=='setosa'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='virginica'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))

#x_1 <- abs(w[3]/w[1])
#x_2 <- abs(w[3]/w[2])

#segments(x_1,0,0,x_2)

}


plot(eps_custo, type="l")



