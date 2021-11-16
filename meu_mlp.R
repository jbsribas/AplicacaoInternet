library(RSNNS)
library(cvTools)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")

data(iris)
head(iris)
summary(iris)

iris3 <- iris[iris$Species != "versicolor", ]
summary(iris3)

X <- cbind(iris3[ ,3],iris3[ ,4],1)
y <- c(iris3[ ,5] )

th <- 1    #number of units in the hidden
it <- 100 #maximum of iterations to learn
iniF <- "Randomize_Weights"  
eta <- 0.1
irisValues <- iris3[,1:2]
irisTargets <- decodeClassLabels(iris3[,5])
#irisTargets <- decodeClassLabels(iris3[,5], valTrue=0.9, valFalse=0.1)
iris3 <- splitForTrainingAndTest(irisValues, irisTargets, ratio=1)
iris3 <- normTrainingAndTestSet(iris3)
model <- mlp(iris3$inputsTrain, iris3$targetsTrain, size=th, learnFuncParams=eta,
             maxit=it)
summary(model)
model
weightMatrix(model)
extractNetInfo(model)
par(mfrow=c(2,2))
plotIterativeError(model)
predictions <- predict(model,iris3$inputsTest)
plotRegressionError(predictions[,2], iris3$targetsTest[,2])
confusionMatrix(iris3$targetsTrain,fitted.values(model))
confusionMatrix(iris3$targetsTest,predictions)

plotROC(fitted.values(model)[,2], iris3$targetsTrain[,2])
plotROC(predictions[,2], iris3$targetsTest[,2])
#confusion matrix with 402040-method
confusionMatrix(iris3$targetsTrain, encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))
