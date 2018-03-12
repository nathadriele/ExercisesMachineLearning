##Regressão Logistica

#1. Importar Dataset
dataset <- read.csv('social_network_ads.csv')
View(dataset)
dataset = dataset [, 3:5]

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Escala
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[, 1:2])

#Model de Regressão Logística
classifier = glm(formula = Purchased ~., family = binomial, data = training_set)

summary(classifier)

#Retiro a coluna 3 pois é esta que quero predizer
#Predição
probalidade = predict(classifier, type = 'response', newdata = test_set[-3])

#Como os resultados de prbabilidade n estao definidos em 0 e 1 verificamos
premonicao = ifelse(probalidade > 0.5, 1, 0)
View(premonicao)

#
#Matriz de confusão
#quero somente a coluna 3 para comparar o test_set
cm = table(test_set[, 3], premonicao > 0.5)
cm





