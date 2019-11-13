library(cluster) 

head(iris)

#Atribuindo as colunas 1-4 (features) para a variavel x, e a classe para a variavel y
x = iris[,-5]
y = iris$Species

kc <- kmeans(x,3,nstart = 100)


#mostrando os resultados, utilizando apenas duas dimensoes (features) Sepal.Length" e "Sepal.Width
plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)


#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html




#------Calculo da Silhouetta - sugestao de numero de clusters

silhouette_score <- function(k){
  km <- kmeans(x, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(x))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Numero de clusters', ylab='Media - Silhouetta', frame=FALSE)

#-----------------------