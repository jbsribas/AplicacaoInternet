### Primeiro contato.
#help(iris)
#head(iris)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
summary(iris)
by(iris,iris$Species,summary)


### Ajustando uma árvore.

head(iris)
#iris[2:4,3]=NA
### Avaliando a discriminação produzida pela árvore.
par(cex=1.5,las=1)
with(iris,plot(V3, V4,type='n',xlab='Comprimento da pétala',ylab='Largura da pétala'))
with(iris[which(iris$V5=='Iris-setosa'),],points(jitter(V3), jitter(V4),pch=20,col='red'))
with(iris[which(iris$V5=='Iris-virginica'),],points(jitter(V3), jitter(V4),pch=20,col='green'))
