#Objetivo: criar um modelo para prever se uma negligencia vai ocorrer em 10 anos com base em LTI e age (atributos do dataset)

set.seed(1234567890)

library("neuralnet")
library("Metrics")

#Dataset: income (renda anual), age, loan (valor do emprestimo) e LTI (loan/income)

dataset <- read.csv("creditset.csv")
head(dataset)

rand <- sample(nrow(dataset),1300)
treino <- dataset[rand,]
validacao <- dataset[-rand,]

creditnet <- neuralnet(default10yr ~ LTI + age, treino, hidden = c(4), lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1, stepmax=1e6)
plot(creditnet, rep = "best")

#----Etapa de validacao----

#prepara o dataset de teste com os atributos de interesse
temp_test <- subset(validacao, select = c("LTI", "age"))

#utiliza o modelo gerado com os dados de teste
creditnet.results <- compute(creditnet, temp_test)

#Verificando a acuracia
#Criando um data.frame com os rotulos de treino e os valores previstos
results <- data.frame(verdade = validacao$default10yr, predicao = creditnet.results$net.result)

#arredonda os valores para nao terem casas decimais
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)

table(roundedresultsdf$verdade,roundedresultsdf$predicao)

accuracy(roundedresultsdf$verdade,roundedresultsdf$predicao)