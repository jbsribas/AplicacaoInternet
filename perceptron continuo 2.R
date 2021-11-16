#PERCEPTRON DE ROSEMBLATT

a <- 1.5
b <- 2
x <- c(0:100)
y<- a*x+b 
X <- cbind(x,1)
paraG <- cbind(x,y)

#saida
Y <- cbind(y)


#inicialização pesos w1 e wbias
 w <- c(0,1) #


#taxa de aprendizagem
eta <- 0.1
plot(paraG, type="n")

for(j in 1:50){
#treinamento
for(i in 1:100){
   v <- sum(X[i]*w)

   # erro médio quadratico
   erro <- ((Y[i]-v)^2)/2
   
   #volta para a função de ativação
   if(v>0){
      y_calc <- 1
   }else{
	y_calc <- 0
   }
   #erro <- y[i]-y_calc
   # View(erro)
   # verificação de erro para parada
   delta <- (eta*erro*X[i,])
   w <- w + delta	
   points(X[i,1],Y[i],col="2")
}


#x_1 <- -(w[2]/w[1])
#x_2 <- -(w[3]/w[2])

#segments(x_1,0)
#segments(x_1,0,0,x_2)

}