library(kohonen)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")

data(iris)
iris[,5]<- as.numeric(iris$Species)
set.seed(7)
training <- sample(nrow(iris), 120)
iris[training, ]
scale(iris[training, ])
Xtraining <- scale(iris[training, ])
Xtest <- scale(iris[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
som.iris <- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))
summary(som.iris)
som.iris$codes
som.iris$unit.classif
som.iris$distances

plot(som.iris, type="changes")

#Node Counts
plot(som.iris, type="count")

#Codes / Weight vectors
plot(som.iris, type="codes")

plot(som.iris, type = "property", property =
       som.iris$codes[,2], main=names(som.iris$data)[2])

# Neighbour distance plot
plot(som.iris, type="dist.neighbours")

#esse não foi
plot(som.iris, type="mapping",
     labels = iris.classes, col = iris.classes+1,
     main = "mapping plot")




som.prediction <- predict(som.iris, newdata = Xtest,
                          trainX = Xtraining,
                          trainY = factor(iris.classes[training]))
table(iris.classes[-training], som.prediction$prediction)
