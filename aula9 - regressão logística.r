##Regressão Logística - Classificadores

#1. Acessar o dataset e carregá-lo em uma variável.
# Importing the dataset e retirar colunas que não possuem valor para o modelo
dataset = read.csv('C:\\Users\\Fabio Navarro\\Google Drive\\___1UNIVEM\\2017 UNIVEM\\2 semestre\\6 sem Sistemas de Apoio a Decisão\\Datasets\\Logistic_Regression\\Social_Network_Ads.csv')
dataset = dataset[, 3:5]


# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
#retiro a coluna 3 pois é esta que quero predizer usando meu modelo de regressao logistica.
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])  

#Como os resultados de prob_pred não estão definidos em 0 ou 1 fazemos uma verificação para tal.
y_pred = ifelse(prob_pred > 0.5, 1, 0)

##Analise prob_pred e y_pred e depois compare com seu test_set para verificar se seu modelo está ok.

# Making the Confusion Matrix - fazendo uma análise do campo Purchased no test_set com meu modelo y_pred
# quero somente a coluna 3 para comparar com o test_set com o modelo.
cm = table(test_set[, 3], y_pred > 0.5)

#    FALSE TRUE
# 0    57    7
# 1    10   26

# 57 eram falsos (não comprariam o carro) e o modelo definiu como falso -> Verdadeiro Negativo - OK
# 26 eram verdadeiros (comprariam o carro) e o modelo definiu como verdadeiro -> Verdadeiro Positivo - OK 

##Importante saber da matriz de confusão é que temos 57 + 26 = 83 das 100 são predições corretas 
##e 17 são predições incorretas.

# Visualising the Training set results
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))