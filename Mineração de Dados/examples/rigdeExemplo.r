#---------ridge exemplo
set.seed(123) 

advertisingData <- read.csv("Advertising.csv")

rand <- sample(nrow(advertisingData),150)
treino <- advertisingData[rand,]
teste <- advertisingData[-rand,]

y <- treino[, "Sales"]
x <- data.matrix(treino[, c("TV", "Radio", "Newspaper")])  

#Usa o cross-validation glmnet
cv <- cv.glmnet(x, y, alpha = 0)

#Usa o melhor valor de lambda
modelRidge <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)

coef(modelRidge)

y_teste <- teste[, "Sales"]
x_teste <- data.matrix(teste[, c("TV", "Radio", "Newspaper")])  

predictionsRidge <- predict(modelRidge, newx = x_teste)


# Metricas de desempenho
data.frame(
  RMSE = RMSE(predictionsRidge, teste$Sales),
  Rsquare = R2(predictionsRidge, teste$Sales)
)
