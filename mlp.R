library(Rcpp)
library(RSNNS)
#LISTA 5
# Criação Bases de Dados
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
#Caminho local, quando não é possível acessas a internet pelo R
iris <- read.table(file=url,header=FALSE,sep=",")

# Visualizar dados
#View(iris)

#Nomear colunas
colnames(iris) <- c("sepal_length","sepal_width", "petal_length","petal_width","class")

setosa <- iris[iris[,"class"] == "Iris-setosa",] #Separação dos dados de setosa
setosa[,"class"] <- 0
virginica <- iris[iris[,"class"] == "Iris-virginica",] #separação dos dados de virginica
virginica[,"class"] <- 1


#Item A)
sepal <- rbind(setosa[, c("sepal_length","sepal_width" ,"class")],virginica[, c("sepal_length","sepal_width" ,"class")])
#shuffle dados
sepal <- sepal[sample(1:nrow(iris),length(1:nrow(sepal))),1:ncol(sepal)]
colnames(sepal) <- c("length","width","class")

#View(sepal)

#Item B)
petala <- rbind(setosa[, c("petal_length","petal_width" ,"class")],virginica[, c("petal_length","petal_width" ,"class")])
#shuffle dados
petala <- petala[sample(1:nrow(petala),length(1:nrow(petala))),1:ncol(petala)]
colnames(petala) <- c("length","width","class")

#View(petala)

par(mfrow=c(3,3))

#plot(sepal[,"length"], sepal[,"width"],sub = "Sepal.")
#plot(petala[,"length"], petala[,"width"],sub = "Petala.")

petalaase = petala

Atr_Targets <- decodeClassLabels(petalaase[,3])

# Criar base de treino de 20%
Base <- splitForTrainingAndTest(petalaase, Atr_Targets, ratio=1)

Base <- normTrainingAndTestSet(Base)

# sIZE = 7, eta = 0,09, Epocas = 50
model <- mlp(Base$inputsTrain, Base$targetsTrain, size=10, learnFuncParams=c(0.1),
             maxit=100)

summary(model)
model

weightMatrix(model)
extractNetInfo(model)

plotIterativeError(model)
predictions <- predict(model,Base$inputsTest)
plotRegressionError(predictions[,2], Base$targetsTest[,2])

#plot(sepal[,"length"], sepal[,"width"],sub = "Sepal.")
plot(petala[,"length"], petala[,"width"],sub = "Petala.")

t_conf_treino <- confusionMatrix(Base$targetsTrain,fitted.values(model))
t_conf_test <- confusionMatrix(Base$targetsTest,predictions)
plotROC(fitted.values(model)[,2], Base$targetsTrain[,2])
plotROC(predictions[,2], Base$targetsTest[,2])
#confusion matrix with 402040-method
t_conf_treino2 <- confusionMatrix(Base$targetsTrain, encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))


#ctable <- as.table(matrix(c(42, 6, 8, 28), nrow = 2, byrow = TRUE))
#fourfoldplot(t_conf_treino, color = c("#CC6666", "#99CC99"),
 #            conf.level = 0, margin = 1, main = "Confusion Matrix Treino")


#ctable <- as.table(matrix(c(42, 6, 8, 28), nrow = 2, byrow = TRUE))
#fourfoldplot(t_conf_test, color = c("#CC6666", "#99CC99"),
            # conf.level = 0, margin = 1, main = "Confusion Matrix Teste")


#ctable <- as.table(matrix(c(42, 6, 8, 28), nrow = 2, byrow = TRUE))
#fourfoldplot(t_conf_treino2, color = c("#CC6666", "#99CC99"),
            # conf.level = 0, margin = 1, main = "Confusion Matrix Treino2")


AcuraceaTreino = (t_conf_treino[1,1] + t_conf_treino[2,2]) / nrow(Base$inputsTrain)

AcuraceaTeste = (t_conf_test[1,1] + t_conf_test[2,2]) / nrow(Base$inputsTest)

AcuraceaTotal = (t_conf_treino[1,1] + t_conf_treino[2,2] + t_conf_test[1,1] + t_conf_test[2,2]) / (nrow(Base$inputsTrain) + nrow(Base$inputsTest))

AcuraceaTreino
AcuraceaTeste
AcuraceaTotal
