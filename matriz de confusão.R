library(randomForest)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
data(iris)
head(iris)


forestIris <- randomForest(Species ~ Petal.Width + Petal.Length, data = iris ,prox = TRUE)

forestIris

getTree(forestIris, k = 2)


table(predict(forestIris), iris$Species)
prop.table(table(predict(forestIris), iris$Species))
## essa linha é para fazer os centros
##iris.p <- classCenter(iris[, c(3, 4)], iris$Species, forestIris$prox)

plot(iris[, 3], iris[, 4], pch = 21, xlab = names(iris)[3], ylab = names(iris)[4],
     bg = c("red", "blue")[as.numeric(factor(iris$Species))], main = "petala iris matriz confusao")
points(iris.p[, 1], iris.p[, 2], pch = 21, cex = 2, bg = c("red", "blue"))

forestIris1 <- randomForest(Species ~ Petal.Width + Petal.Length, data = iris,
                            prox = TRUE, ntree = 50)
forestIris2 <- randomForest(Species ~ Petal.Width + Petal.Length, data = iris,
                            prox = TRUE, ntree = 50)

forestIris1
