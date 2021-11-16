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
sepal <- rbind(setosa[, c("sepal_length","sepal_width" ,"class")],virginica[, c("sepal_length","sepal_width" ,"class")])

sepal <- sepal[sample(1:nrow(sepal),length(1:nrow(sepal))),1:ncol(sepal)]
#View(sepal)

#Item B)
petala <- rbind(setosa[, c("petal_length","petal_width" ,"class")],virginica[, c("petal_length","petal_width" ,"class")])
petala <- petala[sample(1:nrow(petala),length(1:nrow(petala))),1:ncol(petala)]
#View(petala)
#Entradas



#Define qual base será usada, trocar A por B
X <- petala

#Fim da preparação dos dados base IRIS

#Criação dos datasets
# KPastas
X_treino1     <- X[1:25,]
#Saida  para o treino
Y <- c(X_treino1$class)
X_treino1$class=1 # setando o Bias bias

X_teste1  <- X[26:50,]
X_treino2 <- X[51:75,]
X_teste2 <- X[76:100,]

#Inicializacao dos Pesos
W <- c(0,0,-1)

#taxa de aprendizagem
eta <- 0.4
atingiu <- 0

#Máximo de épocas
MaxEpocas = 10

par(mfrow=c(2,2))
#plot(X_treino1[,1],X_treino1[,2],type="n")

#Matriz Confuzão Treino
MatrixConfTreino1 <- cbind(c(0,0),c(0,0))

erro <- 0
erroAnterior <- 0
e <- numeric(100)
eps_custo <- numeric(100)

for(j in 1:MaxEpocas) {
  Treino_Tam=nrow(X_treino1)
  #treinamento
  for(i in 1:Treino_Tam){
    V <- sum(X_treino1[i,]*W)
    
    if(V > 1){
      y_calc <- 1
    } else {
      y_calc <- 0
    }
    
    erroAnterior <- erro
    erro <- ((Y[i]-V)^2)/2
    e[i] <- erro

    if(j==MaxEpocas){
      MatrixConfTreino1[v +1, Y[i]+1] <- MatrixConfTreino1[v+1,Y[i]+1] +1
    }
    
    if((abs(erroAnterior - erro)) > 0.05 & (atingiu <= 1)){
      delta <- (eta*(Y[i]-V)* X_treino1[i,])
      W <- W + delta	
      atingiu <- 0
      
    }  else {
      atingiu <- 1
      
    }
    
    #delta <- (eta * erro * X_treino1[i,])
    #W <- W + delta
    points(X_treino1[i, 1], X_treino1[i, 2], col=2 + Y[i])
  }
  
  eps_custo[j] <- sum(e) 
}

#plot(eps_custo, type="l")

x_1 <- -(W[3] / W[1])
x_2 <- -(W[3] / W[2])

segments(x_1, 0, 0, x_2)

plot(X_treino1[,1],X_treino1[,2],type="n")
segments(x_1, 0, 0, x_2)

V=0
y_calc = 0

tam_amostra <- nrow(X_treino2)
Y_amostra <- X_treino1[,3]
X_treino2[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfTreino2 <- cbind(c(0,0),c(0,0))

for(t in 1:tam_amostra){
  V <- sum(X_treino2[t,]*W)
  
  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }

  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfTreino2[y_calc +1,Y_amostra[t]+1] <- MatrixConfTreino2[y_calc+1,Y_amostra[t]+1] +1
  
    points(X_treino2[t, 1], X_treino2[t, 2], col= (2 + y_calc), pch=6)
  
}


plot(X_treino2[,1],X_treino2[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

V=0
y_calc=0

tam_amostra <- nrow(X_teste1)
Y_amostra <- X_treino1[,3]
X_teste1[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfTeste1 <- cbind(c(0,0),c(0,0))
for(t in 1:tam_amostra){
  V <- sum(X_teste1[t,]*W)

  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }
  
  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfTeste1[y_calc +1,Y_amostra[t]+1] <- MatrixConfTeste1[y_calc+1,Y_amostra[t]+1] +1

  
    points(X_teste1[t, 1], X_teste1[t, 2], col=(2 + y_calc), pch=8)
  
}

V=0

plot(X_teste1[,1],X_teste1[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

tam_amostra <- nrow(X_teste2)
Y_amostra <- X_treino1[,3]
X_teste2[,3] <- 1
V_Calc=cbind(c(1:25),c(1:25),c(1:25))
MatrixConfTeste2 <- cbind(c(0,0),c(0,0))

for(t in 1:tam_amostra){
  V <- sum(X_teste2[t,]*W)
  
  
  if(V>1){
    y_calc <- 1
  } else {
    y_calc <- 0
  }
  
  V_Calc[t,1] <- V
  V_Calc[t,2] <- y_calc
  V_Calc[t,3] <- Y_amostra[t]
  
  MatrixConfTeste2[y_calc +1,Y_amostra[t]+1] <- MatrixConfTeste2[y_calc+1,Y_amostra[t]+1] +1
      points(X_teste2[t, 1], X_teste2[t, 2], col= (2+y_calc), pch=1)
  
}

plot(X_teste2[,1],X_teste2[,2],type="n")
segments(x_1[1,1], 0, 0, x_2[1,1])

AcuraceaTreino1 <- (MatrixConfTreino1[1,1] + MatrixConfTreino1[2,2]) / nrow(X_treino1)
AcuraceaTreino2 <- (MatrixConfTreino2[1,1] + MatrixConfTreino2[2,2]) / nrow(X_treino2)
AcuraceaTeste1 <- (MatrixConfTeste1[1,1] + MatrixConfTeste1[2,2]) / nrow(X_teste1)
AcuraceaTeste2<- (MatrixConfTeste2[1,1] + MatrixConfTeste2[2,2]) / nrow(X_teste2)
AcuraceaMedia <- (AcuraceaTreino1 + AcuraceaTreino2  + AcuraceaTeste1 + AcuraceaTeste2)/4 

MatrixConfTreino1
MatrixConfTreino2
MatrixConfTeste1
MatrixConfTreino2
AcuraceaTreino1
AcuraceaTreino2
AcuraceaTeste1  
AcuraceaTeste2 
AcuraceaMedia  
