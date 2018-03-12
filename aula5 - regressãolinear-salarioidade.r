#Padronizando as cores
install.packages("RColorBrewer")
library(RColorBrewer)
cores <- brewer.pal(5, "Dark2")

#Gerando a massa de dados
setwd("C:\\Users\\Fabio Navarro\\Google Drive\\_DATA_SCIENCE\\_Curso BIG DATA DATA SCIENCE DIEGO NOGARE\3-RegressaoLinear")
arquivo <- "dados.csv"

dados <- read.csv(arquivo, header=TRUE, sep=",")

colnames(dados) <- c("anos","salario")

head(dados)

#Plotando os dados
plot(dados,col=cores[1], pch=20
     , main="Anos trabalhados Vs. Salario"
     , xlab="Anos"
     , ylab="Salario em R$ (vezes 1000)"
     , lwd = 4)

#Criando a regressão
Regressao = lm( dados$salario ~ dados$anos )
abline(Regressao)

#Valores dos Coeficientes
Regressao$coefficients[1] 
Regressao$coefficients[2]

#Calculando o Coeficiente de Determinação - aquele que mede o percentual 
#da variação de Y (variável depende) que é explicado pela variação de X (variável independente)
CoefDet = lm(dados$salario ~dados$anos)
summary(CoefDet)$r.squared
### 0.9450339
## OU SEJA, R² = 94% dos casos de variação de Y é explicado pela variação de X. 


#Calculando o valor de Y com base no de X
AnosTrabalhados = 8
Y = Regressao$coefficients[1] + (AnosTrabalhados * Regressao$coefficients[2])

#Apresentando o ponto estimado no grafico
points(AnosTrabalhados, Y, col=cores[3], lwd=4, pch=20)
lines(AnosTrabalhados, Y, type="h", col=cores[3])
segments(0, Y, AnosTrabalhados, Y, col=cores[3])

Y