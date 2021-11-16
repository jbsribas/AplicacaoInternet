library(randomForest)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")

iris3 <- iris[iris$Species != "versicolor", ]
data(iris3)
head(iris3)

forestIris <- randomForest(Species ~ Petal.Width + Petal.Length, data = iris3 ,prox = TRUE)

forestIris
plot (forestIris, main = "erro petala")

#, mtry=3
#, ntree=100
getTree(forestIris, k = 2)

table(predict(forestIris), iris2$V5)
prop.table(table(predict(forestIris), iris2$V5))
plot (forestIris, main = "erro petala")

#iris.p <- classCenter(iris[, c(3, 4)], iris$V5, forestIris$prox)

plot(iris2[, 3], iris2[, 4], pch = 21, xlab = names(iris2)[3], ylab = names(iris2)[4],
     bg = c("red", "blue")[as.numeric(factor(iris2$V5))], main = "iris matriz de confusão")
points(iris2.p[, 1], iris2.p[, 2], pch = 21, cex = 2, bg = c("red", "blue")