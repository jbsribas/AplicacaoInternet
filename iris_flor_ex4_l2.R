#PERCEPTRON DE ROSEMBLATT



url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
head(iris)
summary(iris)



#entradas
X <- cbind(iris[V1,],iris[V2,],1)




#saida
y <- c(0,0,0,1) #não sei a saida esperada
#y <- c(0,1,1,1) #or
#y <- c(0,1,1,0) #xor

#inicialização pesos
w <- c(0,0,1)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.1


#for(j in 1:50){
#treinamento
for(i in 1:150){
  v <- sum(X[i,]*w)
  if (iris[i,'V5'] == "Iris-setosa"){
    y_calc <- 1
  }else{
    if (iris[i,'V5'] =="Iris-virginica")
          y_calc <- 0
  }
  erro <- y[i]-y_calc
  delta <- (eta*erro*x[i,])
  w <- w + delta	
  
}

head(iris)
#iris[2:4,3]=NA
### Avaliando a discriminação produzida pela árvore.
par(cex=1.5,las=1)
with(iris,plot(V1, V2,type='n',xlab='Comprimento da sepal',ylab='Largura da sepal'))
with(iris[which(iris$V5=='Iris-setosa'),],points(jitter(V1), jitter(V2),pch=20,col='red'))
with(iris[which(iris$V5=='Iris-virginica'),],points(jitter(V1), jitter(V2),pch=20,col='green'))

#}