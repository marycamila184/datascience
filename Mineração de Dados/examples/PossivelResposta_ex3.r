
#Questao 1
#o metodo padrao de linkage eh o COMPLETE LINKAGE
distancia <- dist(iris[, 1:4])
clusters_complete <- hclust(distancia)
plot(clusters_complete)

clusters_ward <- hclust(distancia,method = "ward.D2")
plot(clusters_ward)


#visualmente ja temos a indicacao de que Ward proporciona uma melhor separabilidade.

#utilizando o cophenetic correlation coeficient: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cophenetic.html
cophe_ward <- cophenetic(clusters_ward)
cophe_complete <- cophenetic(clusters_complete)

cor(cophe_ward,distancia)
#saida: 0.87
cor(cophe_complete,distancia)
#saida: 0.72

#confirmamos a indicacao de que Ward e' mais indicado.


#questao 2
library(cluster) 
data <- read.csv("dadoEx2.csv",header = F)

silhouette_score <- function(k){
  km <- kmeans(data, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Numero de clusters', ylab='Media - Silhouetta', frame=FALSE)

#Pelo plot da silhouete podemos perceber que a indicacao e' de 3 grupos