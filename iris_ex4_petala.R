#PERCEPTRON DE ROSEMBLATT
library(randomForest)
library(cvTools)

#url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris2/iris2.data"

#iris2 <- read.table(file=url,header=FALSE,sep=",")
head(iris2)
summary(iris2)
    
#entradas
X <- cbind(iris2[ ,3],iris2[ ,4],1)
#cvFolds((nrow(iris2), K = 5, type = "random")


#saida
y <- c(iris2[ ,5] )#não sei a saida esperada

#inicialização pesos
w <- c(0,0,1)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.1
atingiu <- 0


erro <- 0
erroAnterior <- 0
e <- numeric(100)
vr <- numeric(100)



#treinamento
for(i in 1:100){
  v <- sum(X[i,]*w)
  vr[i] <- v
  #if (v <= 2){
   # y_calc <- 1
  #}else{
  
     # y_calc <- 0
  #}
  forestIris <- randomForest(V5 ~ V3 + V4, data = iris2,prox = TRUE)
  forestIris
  getTree(forestIris, k = 2)
  
  
  
  table(predict(forestIris), iris2$V5)
  prop.table(table(predict(forestIris), iris2$V5))
  
  erroAnterior <- erro
  erro <- ((y[i]-v)^2)/2
  e[i] <- erro
  
  if((abs(erroAnterior - erro)) > 0.05 & (atingiu < 1)){
    delta <- (eta*(y[i]-v)*X[i,])
    w <- w + delta	
    atingiu <- 0
  }
  else {
    atingiu <- 1
    #e[i] <- 0
    
  }
  
  
}

paraG <- cbind(e,100)

plot(paraG, type="n")
lines(e, col="2")


#head(iris2)

### Avaliando a discriminação produzida pela árvore.
#par(cex=1.5,las=1)
#with(iris2,plot(V3, V4,type='n',xlab='Comprimento da pétala',ylab='Largura da pétala'))
#with(iris2[which(iris2$V5=='iris2-setosa'),],points(jitter(V3), jitter(V4),pch=20,col='red'))
#with(iris2[which(iris2$V5=='iris2-virginica'),],points(jitter(V3), jitter(V4),pch=20,col='green'))


#}