library(caret)
library("neuralnet")

#define o tipo de divisao e treino
controleTreino<- trainControl(method="cv", number=10, savePredictions = TRUE)

#Usa a definicao do treino para treinar e predizer com o metodo escolhido
model<- train(Species~., data=iris, trControl=controleTreino, method="mlp")
confusionMatrix(model)
