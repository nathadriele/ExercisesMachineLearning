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

#Criando a regress�o
Regressao = lm( dados$salario ~ dados$anos )
abline(Regressao)

#Valores dos Coeficientes
Regressao$coefficients[1] 
Regressao$coefficients[2]

#Calculando o Coeficiente de Determina��o - aquele que mede o percentual 
#da varia��o de Y (vari�vel depende) que � explicado pela varia��o de X (vari�vel independente)
CoefDet = lm(dados$salario ~dados$anos)
summary(CoefDet)$r.squared
### 0.9450339
## OU SEJA, R� = 94% dos casos de varia��o de Y � explicado pela varia��o de X. 


#Calculando o valor de Y com base no de X
AnosTrabalhados = 8
Y = Regressao$coefficients[1] + (AnosTrabalhados * Regressao$coefficients[2])

#Apresentando o ponto estimado no grafico
points(AnosTrabalhados, Y, col=cores[3], lwd=4, pch=20)
lines(AnosTrabalhados, Y, type="h", col=cores[3])
segments(0, Y, AnosTrabalhados, Y, col=cores[3])

Y