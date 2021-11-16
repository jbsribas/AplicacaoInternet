#Lista4
# PERCEPTRON DE ROSEMBLATT
# Preparação dos dados da base Iris
# URL Remota
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
#Caminho local, quando não é possível acessas a internet pelo R
iris <- read.table(file=url,header=FALSE,sep=",")

# Visualizar dados
#View(iris)

#Nomear colunas
colnames(iris) <- c("sepal_length","sepal_width", "petal_length","petal_width","class")

setosa <- iris[iris[,"class"] == "Iris-setosa",] #Separação dos dados de setosa
setosa$class <- 0

virginica <- iris[iris[,"class"] == "Iris-virginica",] #separação dos dados de virginica
virginica$class <- 1

#Item A)
Atributos_A <- rbind(setosa[, c("sepal_length","sepal_width" ,"class")],virginica[, c("sepal_length","sepal_width" ,"class")])
Atributos_A <- Atributos_A[sample(1:nrow(Atributos_A),length(1:nrow(Atributos_A))),1:ncol(Atributos_A)]
#View(Atributos_A)

#Item B)
Atributos_B <- rbind(setosa[, c("petal_length","petal_width" ,"class")],virginica[, c("petal_length","petal_width" ,"class")])
Atributos_B <- Atributos_B[sample(1:nrow(Atributos_B),length(1:nrow(Atributos_B))),1:ncol(Atributos_B)]
#View(Atributos_B)
#Entradas



#Define qual base será usada, trocar A por B
X <- Atributos_B

#Fim da preparação dos dados base IRIS

#Criação dos datasets
# KPastas
X_Treino     <- X[1:25,]
#Saida  para o treino
Y <- c(X_Treino$class)
X_Treino$class=1 # setando o Bias bias

X_Teste      <- X[26:50,]
X_Validacao  <- X[51:75,]
X_Validacao2 <- X[76:100,]

#Inicializacao dos Pesos
W <- c(0,0,-1)

#taxa de aprendizagem
eta <- 0.4

#Máximo de épocas
MaxEpocas = 10

par(mfrow=c(2,2))
plot(X_Treino[,1],X_Treino[,2],type="n")

#Matriz Confuzão Treino
MatrixConfTreino <- cbind(c(0,0),c(0,0))

for(j in 1:MaxEpocas) {
  Treino_Tam=nrow(X_Treino)
  #treinamento
  for(i in 1:Treino_Tam){
    V <- sum(X_Treino[i,]*W)
    
    if(V>1){
      y_calc <- 1
    } else {
      y_calc <- 0
    }
    
    erro <- Y[i] - y_calc

    if(j==MaxEpocas){
      MatrixConfTreino[y_calc +1, Y[i]+1] <- MatrixConfTreino[y_calc+1,Y[i]+1] +1
    }
    
    delta <- (eta * erro * X_Treino[i,])
    W <- W + delta
    points(X_Treino[i, 1], X_Treino[i, 2], col=2 + Y[i])
  }
  
  
}

x_1 <- -(W[3] / W[1])
x_2 <- -(W[3] / W[2])

segments(x_1[1,1], 0, 0, x_2[1,1])

plot(X_Treino[,1],X_Treino[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

V=0
y_calc = 0

tam_amostra <- nrow(X_Teste)
Y_amostra <- X_Teste[,3]
X_Teste[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfTest <- cbind(c(0,0),c(0,0))
for(t in 1:tam_amostra){
  V <- sum(X_Teste[t,]*W)
  
  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }

  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfTest[y_calc +1,Y_amostra[t]+1] <- MatrixConfTest[y_calc+1,Y_amostra[t]+1] +1
  
  if(V == Y_amostra[t]){
    points(X_Teste[t, 1], X_Teste[t, 2], col=2, pch=6)
  } else {
    points(X_Teste[t, 1], X_Teste[t, 2], col=3, pch=6)
  }
}


plot(X_Treino[,1],X_Treino[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

V=0
y_calc=0

tam_amostra <- nrow(X_Validacao)
Y_amostra <- X_Validacao[,3]
X_Validacao[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfValid1 <- cbind(c(0,0),c(0,0))
for(t in 1:tam_amostra){
  V <- sum(X_Validacao[t,]*W)

  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }
  
  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfValid1[y_calc +1,Y_amostra[t]+1] <- MatrixConfValid1[y_calc+1,Y_amostra[t]+1] +1

  if(V == Y_amostra[t]){
    points(X_Validacao[t, 1], X_Validacao[t, 2], col=2, pch=8)
  } else {
    points(X_Validacao[t, 1], X_Validacao[t, 2], col=3, pch=8)
  }
}

V=0


plot(X_Treino[,1],X_Treino[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

tam_amostra <- nrow(X_Validacao2)
Y_amostra <- X_Validacao2[,3]
X_Validacao2[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfValid2 <- cbind(c(0,0),c(0,0))
for(t in 1:tam_amostra){
  V <- sum(X_Validacao2[t,]*W)
  
  
  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }
  
  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfValid2[y_calc +1,Y_amostra[t]+1] <- MatrixConfValid2[y_calc+1,Y_amostra[t]+1] +1
  
  if(V == Y_amostra[t]){
    points(X_Validacao2[t, 1], X_Validacao2[t, 2], col=2, pch=11)
  } else {
    points(X_Validacao2[t, 1], X_Validacao2[t, 2], col=3, pch=11)
  }
}

AcuraceaTreino <- (MatrixConfTreino[1,1] + MatrixConfTreino[2,2]) / nrow(X_Treino)
AcuraceaTest <- (MatrixConfTest[1,1] + MatrixConfTest[2,2]) / nrow(X_Teste)
AcuraceaValid1 <- (MatrixConfValid1[1,1] + MatrixConfValid1[2,2]) / nrow(X_Validacao)
AcuraceaValid2 <- (MatrixConfValid2[1,1] + MatrixConfValid2[2,2]) / nrow(X_Validacao2)
AcuraceaMedia <- (AcuraceaTreino  + AcuraceaTest + AcuraceaValid1  + AcuraceaValid2 )/4

MatrixConfTreino
MatrixConfTest
MatrixConfValid1
MatrixConfValid2
AcuraceaTreino
AcuraceaTest
AcuraceaValid1  
AcuraceaValid2  
AcuraceaMedia  
