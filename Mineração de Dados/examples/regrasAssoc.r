library(arules)
#Necessario instalar o pacote Matrix, caso nao tenha: TOOLS>INSTALL PACKAGES

load("titanic.raw.rdata")

head(titanic.raw)

# acha regras de associacao sem personalizar parametros (todos default)
regras = apriori(titanic.raw)
inspect(regras) 

#lhs e rhs indicam partes da regra: lhs = left-hand-side, rhs=right-hand-side
#configurando rhs=c("Survived=No", "Survived=Yes") a parte direita da regra so vai ter essas opcoes
regras <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
inspect(regras)

#ordena regras pelo valor do lift
regras.sorted <- sort(regras, by="lift")
inspect(regras.sorted)


#Necessario instalar o pacote arulesViz, caso nao tenha: TOOLS>INSTALL PACKAGES 
library(arulesViz)
plot(regras)