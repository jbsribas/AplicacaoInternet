library(cvTools)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
data(iris)
head(iris)

iris3 <- iris[iris$Species != "versicolor", ]
summary(iris3)

set.seed(1234) # set seed for reproducibility

folds <- cvFolds(nrow(iris3), K = 4)

folds

pasta1 <- folds$subsets[folds$which==1]
pasta_tr1 <- folds$subsets[folds$which!=1]

#apresenta a pasta_tr1 para treinar e a pasra 1 para validar

 for (i in 1:4)
   folds





