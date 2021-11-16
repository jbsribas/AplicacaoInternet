#PERCEPTRON DE ROSEMBLATT

#	  x1,x2,bias

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")

#entradas
 X <- rbind(iris[,'V1'],iris[,'V2'],1)

#saida
y <- c(0,0,0,1) #não sei a saida esperada
#y <- c(0,1,1,1) #or
#y <- c(0,1,1,0) #xor

#inicialização pesos
w <- c(0,0,0,1)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.1
plot(X, type="n")

#for(j in 1:50){
  #treinamento
  for(i in 1:150){
    v <- sum(X[i,]*w)
      if ((iris[i,'V5'] == "Iris-setosa")|(iris[i,'V5'] =="Iris-virginica")){
                    y_calc <- 1
      }else{
                y_calc <- 0
    }
    erro <- y[i]-y_calc
    delta <- (eta*erro*X[i,])
    w <- w + delta	
  
  #points(x1[i],x1[i],col="2")
  #points(x2[i],x2[i],col="2")
  points(X[i],y_calc,col="2")
  
  }
 # x_1 <- -(w[3]/w[1])
  #x_2 <- -(w[3]/w[2])
  
  segments(x_1,0,0,x_2)
#}


