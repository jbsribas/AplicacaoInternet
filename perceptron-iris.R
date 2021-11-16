
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")

head(iris)
iris[,'V1']
iris[,'V2']
iris[,'V5']

# Entradas - ATRIBUTOS
X <- rbind(iris[,'V1'],iris[,'V2'],iris[,'V5'])

# Saídas
Y <- c(0,0,0,1)

# Inicialização dos pesos
W <- c(0,0,1)

# Taxa de aprendizado
eta <- 0.2

# Criando um vetor para o resultado temporário
saida_final <- c(0,0,0,0)

#virginia
vir <- c(0,0,0)

#setosa
s <- c(0,0,0)

# Criação dos padrões de treinamento
# LOOP - Treinamento
#for(j in 1:150) {
# Loop de i até 4
for(i in 1:150){
	v <- sum(X[i,]*W)
	if(v=0) {
		y_calc <- 1
		saida_final[i] <- 1
	} else {
		y_calc <- 0
		saida_final[i] <- 0
	}

	if (X[i,2] == 3.0) {
		plot(X,type="n")
		points(X[3,i],X[3,i],col="2")
		x_1 <- - (W[3] / W[1])
		x_2 <- - (W[3] / W[2])
		segments(x_1,0,0,x_2)
	}

	# Calculando o erro
	erro <- Y[i]-y_calc
	delta <- (eta*erro*X[i,])
	W <- W + delta
	
	#if(saida_final == Y) {
	#	break()
	#}
}

