#PERCEPTRON DE ROSEMBLATT

#	  x1,x2,bias
x1 <- c(0.5,0.5,1)
x2 <- c(0.5,1,1)
x3 <- c(1,0.5,1)
x4 <- c(1,1,1)

#entradas
X <- rbind(x1,x2,x3,x4)

#saida
y <- c(0,0,0,1) #and
#y <- c(0,1,1,1) #or
#y <- c(0,1,1,0) #xor

#inicialização pesos
w <- c(-1,0,0)
#w <- c(0,0,-1) #para or

#taxa de aprendizagem
eta <- 0.1
plot(X, type="n")

#for(j in 1:50){
  #treinamento
  for(i in 1:4){
    v <- sum(X[i,]*w)
    if(v>0){
      y_calc <- 1
    }else{
      y_calc <- 0
    }
    erro <- y[i]-y_calc
    delta <- (eta*erro*X[i,])
    w <- w + delta	
  }
  
  points(x1[1],x1[2],col="2")
  points(x2[1],x2[2],col="2")
  points(x3[1],x3[2],col="2")
  points(x4[1],x4[2],col="2")
  
  x_1 <- -(w[3]/w[1])
  x_2 <- -(w[3]/w[2])
  
  segments(x_1,0,0,x_2)
#}


