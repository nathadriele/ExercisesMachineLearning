#Load dataset
iris <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"),header = FALSE)

#Mostra as 6 primeiras linhas
head(iris)

#Nomear as colunas
names(iris) <- c("Sepal.Legth", "Sepal.Width", "Petal.Legth", "Petal.Width", "Species")
iris

#Instalar pacote de gráfico
install.packages("ggvis")
library(ggvis)
iris %>% ggvis(~Sepal.Legth, ~Sepal.Width, fill=~Species) %>% layer_points()
iris %>% ggvis(~Petal.Legth, ~Petal.Width, fill=~Species) %>% layer_points()

cor(iris$Petal.Legth, iris$Petal.Width)

x = levels(iris$Species)

#Print correlação Setosa
print(x[1])
cor(iris[iris$Species==x[1], 1:4])

#Print versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2], 1:4])

#Print versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[3], 1:4])

#Retornar a estrutura do dataset
str(iris)

#Divisão das especies
table(iris$Species)

#Modelo KNN
install.packages("class")
library(class)

#Preparando os dados
#1. Normalização caso necessário
#2. Test set

#1.Normalização
summary(iris)

#2.Training Set
set.seed(12345)
ind <- sample(2, nrow(iris), replace=TRUE, prob = c(0.67, 0.33))

#Criar de fato o Training-set
iris.Training <- iris[ind==1, 1:4]
iris.Training

iris.Test <- iris[ind==2, 1:4]
iris.Test

any(grepl("class", installed.packages()))

#Preparação dos dados
#1. Normalização
#2. TrainingSet
#3. TestSet

#1. Normalização
summary(iris)

#2. TrainingSet

set.seed(7667)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.67, 0.33))
#No comando acima estamos usando a função sample() para "embaralhar" dentre 2 vetores
#(train e test) o dataset iris inteiro (nrow(iris)) sendo que a cada sorteio de embaralhamento"
#este seja sempre renovado(não vicie) (TRUE) e faça a distribuição em 0.67 para Train e 0.33 para Test.

#Compose trainingSet
iris.training <- iris[ind==1, 1:4]

#Compose trainingSet
iris.test <- iris[ind==2, 1:4]

#Para usar o KNN no R devemos separar o trainingSet e o TestSet ambos com e sem as labels 
#(variavel alvo -> Species)

#Compose iris training labels
iris.trainingLabels <- iris[ind==1, 5]
print(iris.testLabels)

#Compose iris training labels
iris.testLabels <- iris[ind==2, 5]
print(iris.testLabels)

#Agora sim o KNN, queremos encontrar os k vizinhos mais proximos
iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainingLabels, k=3)

#Aqui estamos criando o modelo e definindo os datasets tanto de treino quanto de teste e definido
#que a coluna alvo "cl" está no dataset iris.trainLabels e pegando 3 vizinhos

#Inspect 'iris_pred'
iris_pred
#O resultado é um vetor de fatores com a predição para cada linha do dataset de teste.
#Não inserimos o dataset de labels do dataset de test(iris.testLabels), pois este sera usado
#para verificar se o nosso modelo esta fazendo uma boa predição ou não, ou seja iremos confrontar 
#nossos acertos ou não do modelo com o dataset de testLabels

#Put 'iris.testLabels'in a data frame
irisTestLAbels <- data.frame(iris.testLabels)

#merge 'iris_pred' ad 'iris.testLabels'
merge <- data.frame(iris_pred, iris.testLabels)

#Vamos dar nome ás colunas já na variavel "merge"
names(merge) <- c("Predictedd Species", "Observed Species")

#Inspect 'Merge'
merge

install.packages("gmodels")
library(gmodels)
#Usamos para verificar em detalhes 2 variaveis
#Tabela de contigencia ou tabulação cruzada

#Neste caso, queremos entender como as classes do nosso test "iris.testLabels" 
#relaciona-se, segundo nosso modelo, com iris_pred:
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)

##Desta maneira conseguimos ver que nosso modelo esta muito bom e nao precisa ser refeito.
#Outra maneira de verificar nosso modelo, matriz de confusão.
table(iris.testLabels, iris_pred)
confusionMatrix(iris.testLabels, iris_pred)


