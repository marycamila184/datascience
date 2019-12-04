#part usa o CART para a producao da arvore de decisao
library(rpart)
#pacote para o plot de uma arvore de decisao com aspectos visuais melhor
library(rpart.plot)

#dados sobre os personagens do filme titanic
load("titanic.raw.rdata")

set.seed(2)
rand <- sample(nrow(titanic.raw),1100)

treino <- titanic.raw[rand,]
validacao <- titanic.raw[-rand,]

modelo <- rpart(Survived ~ ., data=treino, method = "class")

#Faz a predicao nos dados de teste que separamos
predicao <- predict(modelo, validacao, type="class")

table(validacao[,4], predicao)



######  Computando metricas avaliativas  #######
#transformando a tabela em matriz
matrizConf = as.matrix(table(validacao[,4], predicao))
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
model<- train(Survived~., data=titanic.raw, trControl=controleTreino, method="rpart")

#visualiza as previsoes
model$pred

#Matriz de confusao
confusionMatrix(model)


library("neuralnet")
model<- train(Survived~., data=titanic.raw, trControl=controleTreino, method="mlp")
confusionMatrix(model)


#------------- Sem o pacote Caret
library("neuralnet")
library("Metrics")

rand <- sample(nrow(titanic.raw),1300)
treino <- titanic.raw[rand,]
validacao <- titanic.raw[-rand,]

#codifica variaveis categoricas em numericas
treino_m <- model.matrix( 
  ~Survived+Class+Sex +Age, 
  data = treino 
)

model <- neuralnet(SurvivedYes~Class2nd+Class3rd+ClassCrew+AgeChild+SexMale, data=treino_m,hidden = c(4,2))
plot(model)

#codifica variaveis categoricas em numericas
valida_m <- model.matrix( 
  ~Survived+Class+Sex +Age, 
  data = validacao 
)

valida_m <- subset(valida_m, select = c("Class2nd","Class3rd","ClassCrew","AgeChild","SexMale"))      
model.results <- compute(model, valida_m)


results <- data.frame(verdade = validacao$Survived, predicao = ifelse(model.results$net.result>0.5,"Yes","No"))
      
table(results)
accuracy(results$verdade,results$predicao)