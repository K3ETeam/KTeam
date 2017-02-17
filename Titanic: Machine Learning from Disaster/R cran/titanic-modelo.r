################################
#  Competición https://www.kaggle.com/c/titanic
################################
library(rattle)
library(RColorBrewer)
library(ggplot2)

library(grid)
library(gridExtra)

library(rpart)
library(rpart.plot)
library(randomForest)

###
# Datasets
###

# Carga de datos
train_url <- "../data/train.csv"
train <- read.csv(train_url)
  
test_url <- "../data/test.csv"
test <- read.csv(test_url)

# Unimos los dos conjuntos para tratar los datos y despues los separaremos de nuevo
full <- rbind(train[,!(names(train) %in% c("Survived"))], test);

#############################
#	Preparamos los datos para que no existan valores vacios en los campos
#	> nrow(full[is.na(full$Fare),]) 	[1] 1
#	> nrow(full[is.na(full$Age),]) 		[1] 263
#	> nrow(full[full$Embarked == "",])	[1] 2
#############################

#Utilizamos rpart para predecir los datos de Embark
rpart_tree_embark <- rpart(Embarked ~ Pclass + SibSp + Parch + Fare, data = train, method = "class")
predict_embark <- predict(rpart_tree_embark, newdata = full[full$Embarked == "",], type = "class")
full$Embarked[strtoi(labels(predict_embark))] <- predict_embark

#Utilizamos rpart para predecir los datos de Fare
aux <- train[!is.na(train$Fare),]
rf_fare <- randomForest(Fare ~ Pclass + Sex + SibSp + Parch + Embarked, data = aux, importance=TRUE, ntree = 1000)
aux <- full[is.na(full$Fare),]
prediction <- predict(rf_fare, aux)
full[labels(prediction),]$Fare <- prediction

#Utilizamos un proceso itererativo para predecir la edad. 
train_age <- train		#copia de train

for(pclass in c(1,2,3)){
	aux <- train_age[!is.na(train_age$Age),]
	rf_age <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Embarked, data = aux, importance=TRUE, ntree = 1000)

	aux <- train_age[is.na(train_age$Age) & train_age$Pclass == pclass,]
	prediction <- predict(rf_age, aux)
	train_age[labels(prediction),]$Age <- prediction
	train_age$Age <- round(train_age$Age)
}

# Una vez entrenado aplicamos la predicción al conjunto full
rf_age <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Embarked, data = train_age, importance=TRUE, ntree = 1000)

aux <- full[is.na(full$Age),]
prediction <- predict(rf_age, aux)
full[labels(prediction),]$Age <- prediction
full$Age <- round(full$Age)

#############################
#	Creacion de variables a partir de las ya existente.
#	· Titulos
#	· Prefijo de Cabina
#	· Prefijo de Tikect
#	· FSize
#############################

ticket <- function(dataset){
	ticket <- gsub(" \\d+|^\\d+$|[:/.:]", "", dataset$Ticket)
	ticket[ticket == ""] <- "Empty"
	ticket <- factor(ticket, labels = labels(table(ticket))[[1]])
	return(ticket)
}

cabin <- function(dataset){
	cabin <- substr(dataset$Cabin,1,1)
	cabin[cabin == ""] <- "Unknown"
	cabin <- factor(cabin, labels = labels(table(cabin))[[1]])
	return(cabin)
}

titles <- function(dataset){
	titles <- gsub('(.*, )|(\\..*)', '', dataset$Name)

	titles[titles == 'Mlle'] <- 'Miss' 
	titles[titles == 'Ms'] <- 'Miss'
	titles[titles == 'Mme'] <- 'Mrs'

	other <- c('Jonkheer', 'the Countess', 'Don', 'Dona', 'Capt', 'Col', 'Sir', 'Lady', 'Major')
	titles[titles %in% other] <- 'Other' 

	titles <- factor(titles, labels = labels(table(titles))[[1]])

	return(titles)
}


full$Title <- titles(full)
full$CabinPrefix <- cabin(full)
full$TicketPrefix <- ticket(full)

full$FSize <- full$Parch + full$SibSp + 1

################################################
#	Separamos el conjunto de entrenamiento y el de pruebas
################################################

train_aux <- full[train$PassengerId,]
train_aux$Survived <- train$Survived
test_aux <- full[test$PassengerId,]

################################################
#	Graficas
################################################

# Edad del conjunto de entrenamiento inicial frente al final
grid.arrange(
	ggplot(train, aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(.~Sex),
	ggplot(train_aux, aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(.~Sex),
ncol = 2)

# Visualizacion de outliers para Fare, FSize, Age
boxplot(train_aux$Age)
ggplot(train_aux, aes(factor(Survived), Age)) + geom_boxplot() + facet_grid(.~Sex)

boxplot(train_aux$Fare)
ggplot(train_aux, aes(factor(Survived), Fare)) + geom_boxplot() + facet_grid(.~Sex)

boxplot(train_aux$FSize)
ggplot(train_aux, aes(factor(Survived), FSize)) + geom_boxplot() + facet_grid(.~Sex)

################################################
#	Otras medidas (Aún no se como interpretar o si son interpretables)
#	http://stats.stackexchange.com/questions/108007/correlations-with-categorical-variables
################################################

# Correlación
cor(train_aux$Survived, train_aux$Age)
cor(train_aux$Survived, train_aux$Fare)
cor(train_aux$Survived, train_aux$FSize)

cor(train_aux$Age, train_aux$Fare)
cor(train_aux$Age, train_aux$FSize)

cor(train_aux$FSize, train_aux$Fare)

# Estudio de varianza 
summary(aov(Survived ~ Title, train_aux))
summary(aov(Survived ~ CabinPrefix, train_aux))
summary(aov(Survived ~ Sex, train_aux))

# Test chi quadrado
chisq.test(train_aux$Title, train_aux$Sex)
chisq.test(train_aux$Title, train_aux$CabinPrefix)
chisq.test(train_aux$Sex, train_aux$CabinPrefix)

################################################
#	Experimentamos sacando modelos!!!!
################################################

randomForest(as.factor(Survived) ~ Pclass + Fare + Sex + Age + Title + CabinPrefix + TicketPrefix + Embarked + FSize + Parch + SibSp, data = train_aux, importance=TRUE, ntree = 750, mtry = 3, nodesize = 5)

#        OOB estimate of  error rate: 17.06%
#Confusion matrix:
#    0   1 class.error
#0 532  17  0.03096539
#1 135 207  0.39473684

randomForest(as.factor(Survived) ~ Pclass + Fare + Title, data = train_aux, importance=TRUE, ntree = 1000, mtry = 3, nodesize = 5)

#        OOB estimate of  error rate: 15.71%
#Confusion matrix:
#    0   1 class.error
#0 488  61   0.1111111
#1  79 263   0.2309942

# Para los de 2º clase

randomForest(as.factor(Survived) ~ Fare + Title, data = train_aux[train_aux$Pclass ==2,], importance=TRUE, ntree = 1000)
#        OOB estimate of  error rate: 7.61%
#Confusion matrix:
#   0  1 class.error
#0 91  6  0.06185567
#1  8 79  0.09195402

