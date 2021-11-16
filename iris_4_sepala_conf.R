#PERCEPTRON DE ROSEMBLATT
library(randomForest)
library(cvTools)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
data(iris)
head(iris)
summary(iris)

iris3 <- iris[iris$Species != "versicolor", ]

#entradas
X <- cbind(iri3[ ,1],iris[ ,2],1)

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
w <- c(0.8,0.3,0.7)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.1
atingiu <- 0


erro <- 0
erroAnterior <- 0
e <- numeric(100)
vr <- string(100)

forestIris <- randomForest(Species ~ Sepal.Width + Sepal.Length, data = iris ,prox = TRUE)
#forestIris
getTree(forestIris, k = 2)

#treinamento
for(i in 1:100){
v <- sum(X[1,]*w)
iris3[i,1] <- (X[i,1]*w[1])
iris3[i,2] <- ((X[i,2]*w[2])+(X[i,3] *w[3]))
summary(iris3)
teste <- predict(forestIris,iris3[i, ],type="response",norm.votes=TRUE)

if (teste == "setosa"){
  y_calc <- 1
}else{
  y_calc <- 0
}
vr[i] <- teste

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

paraG <- cbind(X,100)

plot(paraG, type="n")
lines(e, col="2")




### Avaliando a discriminação produzida pela árvore.
par(cex=1.5,las=1)
with(iris,plot(Petal.Length, Petal.Width,type='n',xlab='Comprimento da pétala',ylab='Largura da pétala'))
with(iris[which(iris$Species=='setosa'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='red'))
with(iris[which(iris$Species=='virginica'),],points(jitter(Petal.Length), jitter(Petal.Width),pch=20,col='green'))
