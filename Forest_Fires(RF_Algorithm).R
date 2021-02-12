#Predicting the Algerian forest fires (Bejaia region) using the Random Forest Classification

#Setting the working directory
setwd('C:/Users/DELL/Desktop/Project_Files/Algerian_Forest_Fires')

#Importing the data and viewing the first few rows
dataset = readxl::read_excel('Algerian_Forest_Fires.xlsx')
head(dataset)

#Converting the dataset into a data frame
dataset = as.data.frame(dataset)

#Removing the unwanted 'Days' column from the dataset
dataset = dataset[-1]

#Converting 'Classes' into a factor and encoding them
dataset$Classes = factor(dataset$Classes, levels = c('not fire', 'fire'), labels = c(0,1))
View(dataset)

#Splitting the data into training and test sets
library(caTools)
split = sample.split(dataset$Classes, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Using the Random Forest classification algorithm from the 'randomForest' package
library(randomForest)
classifier = randomForest(data = training_set, x = training_set[-11], y = training_set$Classes, ntree = 10)

#Predicting the classifier for the 'test_set'
y_pred = predict(classifier, newdata = test_set, type = 'response')

#Checking model performance on the 'test_set'
cm = table(test_set[,11], y_pred)
cm


#Another method for checking model performance by using the K-Fold Cross Validation Method 
#'First loading the 'caret' package and performing the technique
library(caret)
folds = createFolds(dataset$Classes, k = 20)
cv = lapply(folds, function(x){
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = randomForest(data = training_fold, x = training_fold[-11], y = training_fold$Classes, ntree = 10)
  y_pred = predict(classifier, newdata = test_fold, type = 'response')
  cm = table(test_fold[,11], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(acfcuracy)
})

#Checking the 20 accuracies
cv

#Finding the mean (and final) model accuracy
round(mean(as.numeric(cv))*100, 2)



#In the above steps, we took all the variables into the model (after excluding the 'Days' column)
#'Now, we take only the significant variables by comparing each of them with the 'Classes'
#'We do this using the Chi-Square Test and iterate over the variables using a For loop

for(i in 1:10){
  print(names(dataset[i]))
  print(chisq.test(dataset[i], dataset$Classes))
}

#'Temperature', 'Rain', 'ISI', 'FWI' are the only variables which are significant

#'We subset the dataset into these columns and look at them
data = dataset[c(1,4,8,10,11)]
View(data)

#We go through the K-Fold Cross Validation Method to check the new model accuracy
fold = createFolds(data$Classes, k = 20)
ab = lapply(fold, function(x){
  training_fold1 = data[-x, ]
  test_fold1 = data[x, ]
  classifier1 = randomForest(x = training_fold1[-5], y = training_fold1$Classes, data = training_fold1, ntree = 10)
  y_pred1 = predict(classifier1, newdata = test_fold1, type = 'response')
  cm1 = table(test_fold1[,5], y_pred1)
  accuracy1 = (cm1[1,1] + cm1[2,2]) / (cm1[1,1] + cm1[2,2] + cm1[1,2] + cm1[2,1])
  return(accuracy1)
})

ab

#Calculating the final accuracy
round(mean(as.numeric(ab))*100,2)

#The accuracy increases but only slightly, but this is a good model anyway with an accuracy of more than 90% consistently
