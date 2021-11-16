library(randomForest)

data(iris2)
head(iris2)


forestIris <- randomForest(V5 ~ V1 + V2, data = iris2,prox = TRUE)
forestIris



getTree(forestIris, k = 2)


table(predict(forestIris), iris2$V5)
prop.table(table(predict(forestIris), iris2$V5))
plot (forestIris, main = "erro sepal")

      

#iris.p <- classCenter(iris[, c(3, 4)], iris$V5, forestIris$prox)

#plot(iris2[, 1], iris2[, 2], pch = 21, xlab = names(iris2)[3], ylab = names(iris2)[4],
#     bg = c("red", "blue")[as.numeric(factor(iris2$V5))], main = "iris matriz de confusão sepal")
#points(iris2.p[, 1], iris2.p[, 2], pch = 21, cex = 2, bg = c("red", "blue")