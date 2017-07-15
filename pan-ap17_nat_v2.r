#install.packages("qdap")
#install.packages("XML")
#install.packages("tm")
#install.packages("splitstackshape")
#install.packages("caret")

#install.packages("kernlab")
#install.packages("klaR")
#install.packages("party")
#install.packages("randomForest")
install.packages("e1071")

library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)

library(kernlab)
library(klaR)
library(party)
library(randomForest)
library(e1071)


GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE, accents = TRUE) {
	setwd(path)
	
	# cargar todos los tuits para generar un vocabulario
	files = list.files(pattern="*.xml")
	
	corpus.raw <- NULL
	i <- 0
	for (file in files) {
		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
		i <- i + 1
		if (verbose) print(paste(i, " ", file))	
	}

	corpus.preprocessed <- corpus.raw
	
	if (lowcase) {
		if (verbose) print("Tolower...")
		corpus.preprocessed <- tolower(corpus.preprocessed)
	}
  
  #Remove accents
	if (accents) {
	  if (verbose) print("Remove the accents...")
	  corpus.preprocessed <- chartr("áéíóúã", "aeioua", corpus.preprocessed)
	}
	
	if (punctuations) {
		if (verbose) print("Removing punctuations...")		
		corpus.preprocessed <- removePunctuation(corpus.preprocessed)
	}

	if (numbers) {
		if (verbose) print("Removing numbers...")
		corpus.preprocessed <- removeNumbers(corpus.preprocessed)
	}

	if (whitespaces) {
		if (verbose) print("Stripping whitestpaces...")
		corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
	}

	if (swlang!="")	{
		if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
		corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
	}
	
	if (swlist!="") {
		if (verbose) print("Removing provided stopwords...")
		corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
	}
  
 # if(oneletter){
 #    if (verbose) print(paste("Removing words only letters"))
 #    if (corpus.preprocessed)
 #    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
 #  }

	if (verbose) print("Generating frequency terms")
	
	corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
	if (verbose) plot(corpus.frequentterms)
	
	return (corpus.frequentterms)
}


GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE, accents = TRUE) {
	setwd(path)

	truth <- read.csv("truth.txt", sep=":", header=FALSE)
	truth <- truth[,c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

	i <- 0
	bow <- NULL
	files = list.files(pattern="*.xml")
	for (file in files) {
		author <- gsub(".xml", "", file)
		variety <- truth[truth$author==author,"variety"]
		gender <- truth[truth$author==author,"gender"]

		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
		

		if (lowcase) {
			txtdata <- tolower(txtdata)
		}
    
    if (accents) {
      txtdata <- chartr("áéíóúã", "aeioua", txtdata)
    }

		if (punctuations) {
			txtdata <- removePunctuation(txtdata)
		}

		if (numbers) {
			txtdata <- removeNumbers(txtdata)
		}

		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
		freq <- freq_terms(txtdata, n)
		for (word in vocabulary$WORD) {
			thefreq <- 0
			if (length(freq[freq$WORD==word,"FREQ"])>0) {
				thefreq <- freq[freq$WORD==word,"FREQ"]
			} 
			line <- paste(line, ",", thefreq, sep="")
		}
    
    if (class=="variety") {
			line <- paste(variety, ",", line, sep="")
		} else {
			line <- paste(gender, ",", line, sep="")
		}
    
#		if (class=="variety") {
#			line <- paste(line, ",", variety, sep="")
#		} else {
#			line <- paste(line, ",", gender, sep="")
#		}

		bow <- rbind(bow, line)

		i <- i + 1

		if (verbose) {
			if (class=="variety") {
				print(paste(i, author, variety))
			} else {
				print(paste(i, author, gender))
			}
		}
	}

	return (bow)
}

#ELENA: Diversas bolsas de palabrabras generadas durante la práctica
#número utilizado para hacer la exploración de las palabras más usadas
#n <- 100
#1000 = número de palabras utilizado para los primeros cálculos
#n <- 1000
#2000 = número de palabras utilizado para los segundos cálculos
n <- 2000
path_training <- "C:/Users/elena/MasterBigData/Practica TextMining/20170609/training"	# Your training path
path_test <- "C:/Users/elena/MasterBigData/Practica TextMining/20170609/test"			# Your test path


#Generación del vocabulario inicial sin Stopwords
#vocabulary <- GenerateVocabulary(path_training, n, swlang="es")

#Generación del vocabulario
vocabulary <- GenerateVocabulary(path_training, n, swlang="es", swlist=c("youtube", "video", "q", "si", "x", "jajaja", "trump", "d", "mas"))

#Generación de los datasets de Training y Test para Sexo (gen) y Variedad (var)
bow_training_gen <- GenerateBoW(path_training, vocabulary, n, class="gender")
bow_test_gen <- GenerateBoW(path_test, vocabulary, n, class="gender")

bow_training_var <- GenerateBoW(path_training, vocabulary, n, class="variety")
bow_test_var <- GenerateBoW(path_test, vocabulary, n, class="variety")


# split para tenerlo como vectores
training_gen <- cSplit(bow_training_gen, "V1", ",")
test_gen <- cSplit(bow_test_gen, "V1", ",")

training_var <- cSplit(bow_training_var, "V1", ",")
test_var <- cSplit(bow_test_var, "V1", ",")

# Se elimina la columna del autor
training_gen <- training_gen[,-2]
training_var <- training_var[,-2]

# creamos la columna class, que es la última del vector quecontiene el nombre de la clase
names(training_gen)[1] <- "class"
names(training_var)[1] <- "class"


truth_gen  <- unlist(test_gen[,1:1])
truth_var  <- unlist(test_var[,1:1])
# Se elimina la columna del autor
test_gen <- test_gen[,-2]
test_var <- test_var[,-2]

# Aprendiendo y evaluando con validación cruzada
train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 
model_SVM_gen <- train( class~., data= training_gen, trControl = train_control, method = "svmLinear")
print(model_SVM_gen)
model_SVM_var <- train( class~., data= training_var, trControl = train_control, method = "svmLinear")
print(model_SVM_var)

# Aprendiendo con todo el training
#train_control <- trainControl(method="none")
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")

# Evaluando con el test
pred_SVM_gen <- predict(model_SVM_gen, test_gen)
confusionMatrix(pred_SVM_gen, truth_gen)

pred_SVM_var <- predict(model_SVM_var, test_var)
confusionMatrix(pred_SVM_var, truth_var)



# Aprendiendo con Naive Bayes
train_control <- trainControl(method="cv")
model_NB_gen <- train( class~., data= training_gen, trControl = train_control, method = "nb")
model_NB_var <- train( class~., data= training_var, trControl = train_control, method = "nb")

# Evaluando con el test Naive Bayes
pred_NB_gen <- predict(model_NB_gen, test_gen)
confusionMatrix(pred_NB_gen, truth_gen)
pred_NB_var <- predict(model_NB_var, test_var)
confusionMatrix(pred_NB_var, truth_var)


# Aprendiendo con randomForest
train_control <- trainControl(method="none")
model_RF_gen <- train( class~., data= training_gen, trControl = train_control, method = "rf")
model_RF_var <- train( class~., data= training_var, trControl = train_control, method = "rf")

# Evaluando con el test randomForest
pred_RF_gen <- predict(model_RF_gen, test_gen)#confusionMatrix(pred_RF_gen, truth_gen)
pred_RF_var <- predict(model_RF_var, test_var)
confusionMatrix(pred_RF_var, truth_var)


