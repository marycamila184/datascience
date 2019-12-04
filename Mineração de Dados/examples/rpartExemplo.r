#part usa o CART para a producao da arvore de decisao
library(rpart)
#pacote para o plot de uma arvore de decisao com aspectos visuais melhor
library(rpart.plot)

#dados sobre 3 tipos de plantas iris
data("iris")

#garante em que todas as execucoes sera produzido o mesmo resultado. Ou seja, vamos selecionar
#sempre a mesma amostra para treino. Comente para ver a variacao
set.seed(2)
#seleciona 100 numeros aleatorios entre o numero de observacoes (150) do dataset
rand <- sample(nrow(iris),100)

#O total de observacoes e' 150: 2/3 para treino e 1/3 para teste
treino <- iris[rand,]
validacao <- iris[-rand,]

#O "." significa que esta' considerando todos os parametros restantes na formula. Metodo = "class" indicando que queremos um classificador. 
modelo <- rpart(Species ~ ., data=treino, method = "class")

#Plot simples, pode nao ser a melhor opcao visualmente falando
#rpart.plot(modelo)

#Plot com parametros personalizados. Modificando os parametros podemos produzir 
#uma visualizacao mais interessante
rpart.plot(modelo, type=3,extra=100)

#Faz a predicao nos dados de teste que separamos
predicao <- predict(modelo, validacao, type="class")

str(validacao)
#cria uma matriz de confusao para verficar a "qualidade" do modelo criado
table(validacao[,5], predicao)



######  Computando metricas avaliativas  - Opcional #######
#transformando a tabela em matriz
matrizConf = as.matrix(table(validacao[,5], predicao))
matrizConf

#variaveis basicas
n = sum(matrizConf) # numero de instancias
nc = nrow(matrizConf) # numero de classes
diag = diag(matrizConf) # numero de instancias classificadas corretamente 
rowsums = apply(matrizConf, 1, sum) # numero de instancias por classe 
colsums = apply(matrizConf, 2, sum) # numero de predicao por classe

#mais detalhes sobre essas metricas: https://en.wikipedia.org/wiki/Confusion_matrix
accuracia = sum(diag) / n 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
data.frame(macroPrecision, macroRecall, macroF1)


#----------------------usando o pacote CARET
library(caret)

#define o tipo de divisao e treino
controleTreino<- trainControl(method="cv", number=10, savePredictions = TRUE)

#Usa a definicao do treino para treinar e predizer com o metodo escolhido
model<- train(Species~., data=iris, trControl=controleTreino, method="rpart")

#visualiza as previsoes
model$pred

#Matriz de confusao
confusionMatrix(model)