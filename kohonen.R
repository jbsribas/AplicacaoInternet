library(kohonen)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(file=url,header=FALSE,sep=",")
data(iris)

som_grid <- somgrid(xdim=10,ydim=10,topo="hexagonal")
#som_model <- som(data,grid=som_grid,rlen=100,alpha=c(0.05,0.01))
som_model <- som(iris,grid=som_grid,rlen=100,alpha=c(0.05,0.01))               
som_model                 