#o metodo padrao de linkage eh o COMPLETE LINKAGE
clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)

#De acordo com o dendograma um bom corte eh na altura 3, o que da um numero de 3 grupos

#o cutree separa 3 grupos
clusterCut <- cutree(clusters, 3)

#metodo de linkage average
clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)

#Separando em tres grupos, com o cutree
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

#plot dendograma com labels
grupos <- hclust(dist(iris[, 3:4]),method = "complete")
plot(grupos,iris[,5])

#Separando em tres grupos, com o cutree
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)