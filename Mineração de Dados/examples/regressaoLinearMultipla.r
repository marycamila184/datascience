library(caret)
library(glmnet)

set.seed(123) 

#Regressao linear multipla. Exemplo slides - advertising
advertisingData <- read.csv("Advertising.csv")

rand <- sample(nrow(advertisingData),150)
treino <- advertisingData[rand,]
teste <- advertisingData[-rand,]


modelo <- lm(Sales~TV+Radio+Newspaper, data = advertisingData )
summary(modelo)


cor(advertisingData)

#avaliacao da regressao

predictionLinear = predict(modelo,teste)

# Metricas de desempenho
data.frame(
  RMSE = RMSE(predictionLinear, teste$Sales),
  Rsquare = R2(predictionLinear, teste$Sales)
)