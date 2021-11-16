library(Rcpp)
library(RSNNS)
#LISTA 4
# Criação Bases de Dados
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
#Caminho local, quando não é possível acessas a internet pelo R
iris <- read.table(file=url,header=FALSE,sep=",")

# Visualizar dados
View(iris)

#Nomear colunas
colnames(iris) <- c("sepal_length","sepal_width", "petal_length","petal_width","class")

setosa <- iris[iris[,"class"] == "Iris-setosa",] #Separação dos dados de setosa
setosa[,"class"] <- 0
virginica <- iris[iris[,"class"] == "Iris-virginica",] #separação dos dados de virginica
virginica[,"class"] <- 1



#Item A)
Atributos_A <- rbind(setosa[, c("sepal_length","sepal_width" ,"class")],virginica[, c("sepal_length","sepal_width" ,"class")])
#shuffle dados
Atributos_A <- Atributos_A[sample(1:nrow(Atributos_A),length(1:nrow(Atributos_A))),1:ncol(Atributos_A)]
View(Atributos_A)

#Item B)
Atributos_B <- rbind(setosa[, c("petal_length","petal_width" ,"class")],virginica[, c("petal_length","petal_width" ,"class")])
#shuffle dados
Atributos_B <- Atributos_B[sample(1:nrow(Atributos_B),length(1:nrow(Atributos_B))),1:ncol(Atributos_B)]
View(Atributos_B)


par(mfrow=c(2,2))

plot(Atributos_A[,"sepal_length"], Atributos_A[,"sepal_width"],sub = "Distribuição Atributos Sepalas.")
plot(Atributos_B[,"petal_length"], Atributos_B[,"petal_width"],sub = "Distribuição Atributos Petalas.")


Atributos_ATargets <- decodeClassLabels(Atributos_A[,3])

BaseA <- splitForTrainingAndTest(Atributos_A, Atributos_ATargets, ratio=0.15)

BaseA <- normTrainingAndTestSet(BaseA)

# sIZE = 6
modelA <- mlp(BaseA$inputsTrain, BaseA$targetsTrain, size=6, learnFuncParams=c(0.1),
             maxit=50, inputsTest=BaseA$inputsTest, targetsTest=BaseA$targetsTest)

summary(modelA)
modelA

weightMatrix(modelA)
extractNetInfo(modelA)

plotIterativeError(modelA)
predictionsA <- predict(modelA,BaseA$inputsTest)
plotRegressionError(predictionsA[,2], BaseA$targetsTest[,2])
confusionMatrix(BaseA$targetsTrain,fitted.values(modelA))
confusionMatrix(BaseA$targetsTest,predictionsA)
plotROC(fitted.values(modelA)[,2], BaseA$targetsTrain[,2])
plotROC(predictionsA[,2], BaseA$targetsTest[,2])
#confusion matrix with 402040-method
confusionMatrix(BaseA$targetsTrain, encodeClassLabels(fitted.values(modelA),
                                                     method="402040", l=0.4, h=0.6))





Atributos_BTargets <- decodeClassLabels(Atributos_B[,3])

BaseB <- splitForTrainingAndTest(Atributos_B, Atributos_BTargets, ratio=0.15)

BaseB <- normTrainingAndTestSet(BaseB)

modelB <- mlp(BaseB$inputsTrain, BaseB$targetsTrain, size=5, learnFuncParams=c(0.1),
              maxit=50, inputsTest=BaseB$inputsTest, targetsTest=BaseB$targetsTest)

summary(modelB)
modelB

weightMatrix(modelB)
extractNetInfo(modelB)
plotIterativeError(modelB)
predictionsB <- predict(modelB,BaseB$inputsTest)
plotRegressionError(predictionsB[,2], BaseB$targetsTest[,2])
confusionMatrix(BaseB$targetsTrain,fitted.values(modelB))
confusionMatrix(BaseB$targetsTest,predictionsB)
plotROC(fitted.values(modelB)[,2], BaseB$targetsTrain[,2])
plotROC(predictionsB[,2], BaseB$targetsTest[,2])
#confusion matrix with 402040-method
confusionMatrix(BaseB$targetsTrain, encodeClassLabels(fitted.values(modelB),
                                                      method="402040", l=0.4, h=0.6))
















