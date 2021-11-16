#  PERCEPTRON DE ROSEMBLATT

# criacao dos padrões de treinamento
#     xi1,xi2,bias
x1 <- c(0,0,1)
x2 <- c(0,1,1)
x3 <- c(1,0,1)
x4 <- c(1,1,1)

# entradas
X <- rbind(x1,x2,x3,x4) 
#saidas
Y <- c(0,0,0,1)

# inicializacao dos pesos
W <- c(0,0,1)

# taxa de aprendizada
eta <- 0.1

# treinamento
for(i in 1:4){
  v <- sum(X[i,]*W)
  if(v>0){
    y_calc <- 1
  }else{
    y_calc <- 0
  }
  erro <- Y[i]-y_calc
  delta <- (eta*erro*X[i,])
  W <- W + delta
}

plot(X,type="n")
points(x1[1],x1[2],col="2")
points(x2[1],x2[2],col="2")
points(x3[1],x3[2],col="2")
points(x4[1],x4[2],col="1")

x_1 <- -(W[3] / W[1])
x_2 <- -(W[3] / W[2])
segments(x_1,0,0,x_2)




