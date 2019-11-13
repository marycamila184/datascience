#Exercicio 1 slides de regressao linear simples
dias <- c(1,2,4,8,16,25)
tempo <- c(0.65,0.79,1.36,2.26,3.59,5.36)

mediaDias <- mean(dias)
mediaTempo <- mean(tempo)

b1 <- sum((dias-mediaDias)*(tempo-mediaTempo))/sum((dias-mediaDias)^2)
b0 <- mediaTempo - (b1)*(mediaDias)

tempoPrevisao <- b0+b1*dias

SSE <- sum((tempo-tempoPrevisao)^2)
TSS <-sum((tempo- mediaTempo)^2)
R2 <- (TSS-SSE)/TSS
#R2 eh equivalente ao coeficiente de correlacao (r) ao quadrado (r2)
cor(dias,tempo)^2

n<-6
Se <- sqrt(SSE/(n-2))
#desvio padrao de b0
Sb0 <- Se*sqrt(1/n+ (mediaDias^2/ (sum(dias^2) -n*(mediaDias^2))))

#desvio padrao de b1
Sb1 <- Se / (sqrt(sum(dias^2)-n*mediaDias^2))

#intervalo de confianca de 90% alfa=0.1
#t[0.95,4] = 2.132
#bi +- 2.132* Sbi
#Para b0
c(b0 -2.132* Sb0, b0 +2.132* Sb0)

#Para b1
c(b1 -2.132* Sb1, b1 +2.132* Sb1)


#---------Usando funcoes do R para fazer os mesmos calculos
modelo <- lm(tempo~dias)
summary(modelo)
confint(modelo,level = 0.90)

plot(dias,tempo)
abline(modelo$coefficients)
