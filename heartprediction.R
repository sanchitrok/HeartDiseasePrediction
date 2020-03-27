# Heart Prediction
install.packages('e1071')
library(e1071)
library (caret)
library(dplyr)
# Importing the dataset
dataset = read.csv('heart.csv')
summary(dataset)
dataset$ï..age<-dataset$age
str(dataset)
dataset$sex<-as.factor(dataset$sex)
dataset$cp<-as.factor(dataset$cp)
dataset$fbs<-as.factor(dataset$fbs)
dataset$restecg<-as.factor(dataset$restecg)
dataset$exang<-as.factor(dataset$exang)
dataset$slope<-as.factor(dataset$slope)
dataset$ca<-as.factor(dataset$ca)
dataset$thal<-as.factor(dataset$thal)

# Encoding the target feature as factor
dataset$target = factor(dataset$target, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$target, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -14],
             test = test_set[, -14],
             cl = training_set[, 14],
             k = 5,
             prob = TRUE)

y_pred
# Making the Confusion Matrix
cm = table(test_set[, 14], y_pred)
confusionMatrix(cm)
#Naive Bayes
classifier = naiveBayes(x = training_set[-14],
                        y = training_set$target)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-14])

# Making the Confusion Matrix
cm = table(test_set[, 14], y_pred)
confusionMatrix(cm)



