
#Install and Load the following packages
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
library("caret")
library("e1071")
library("rpart")
library("rpart.plot")


#Load the dataset customer_churn
data = read.csv("customer_churn.csv")
data[1:5,]

summary(data)
#convert the outcome column to be a factor - tell R that this column is categorical
#important step for binary classification
data$Churn <- as.factor(data$Churn)

#OBJECTIVE: build classification models to predict whether a customer will churn
#or not


#First, we need to randomly split our dataset into training and testing dataset
#Use the createDataPartition function
#y identifies our class (outcome) variable; in this case, it is the Churn variable
#p is the % of datapoints we want in the training data; 
#list = FALSE is to specify that our outcome is not a list

#createDataPartition is going to randomly pick 70% of our existing observations
#and put them into the vector train_rows

set.seed(123)
train_rows <- createDataPartition(y = data$Churn, p =0.7, list = FALSE)

#Create the training data using the randomly picked observations from before
data_train <- data[train_rows,]

#Create the test data, by taking all the remaining observations that 
#were not included in data_train
data_test <- data[-train_rows, ]

#Next, assess whether to normalize data
#In this case, let us use standardization
#First, let us create a copy of the training and testing data

data_train_stand <- data_train
data_test_stand <- data_test

#Think about which variables we want to standardize
#remember: we do not standardize the outcome (class) variable
#we will standardize variables from 1 to 17

#We apply the function scale to the data_train_stand and data_test_stand
data_train_stand[,1:17] <- apply(data_train_stand[,1:17], MARGIN = 2, FUN = scale)
data_test_stand[,1:17] <- apply(data_test_stand[,1:17], MARGIN = 2, FUN = scale)

#K-NN
#Let us train a k-NN model on the standardized, training data
#Use the function train(), specify the method knn, and specify the class variable (Y)
#and attributes you want to use for prediction. If you want to use all the attributes
#simply specify Churn~.

fitKNN <- train(data= data_train_stand, method = "knn", Churn~.)
fitKNN

#We can set the values of k we would like the method to use
grid = expand.grid(k = c(5,7,9,13,15))

fitKNN <- train(data= data_train_stand, method = "knn", Churn~., 
                trControl = trainControl(search="grid"), tuneGrid=grid)
fitKNN

#Plot how accuracy of the model changes with number of k
#Note: since the model is trained on a random set of data, we may get slightly different results 
plot(fitKNN, ylab = "Accuracy")

#Use the k-NN model we just trained to predict the classes of the observations in the 
#testing data
#in other words, we are going to use the model we just built above
#and try to use it to predict the class (outcome) of the observations in the testing data
#note that since we also know the "true" value for the class variable for the observations
#in the testing data, we can then compare the predictions obtained with the model with the
#real value to evaluate how good our model is
knn_predictions <- predict(fitKNN, data_test_stand)

#Create the confusionMatrix to evaluate the performance of the model
#we use the function confusionMatrix and input the predictions we want to use to create the matrix

confusionMatrix(knn_predictions, data_test_stand$Churn)

#if we use mode = "prec_recall" we will get precision, recall and F1 for the class that we select

confusionMatrix(knn_predictions, data_test_stand$Churn, mode = "prec_recall", positive = "True")
#we don't usually run this (line below) for the not positive class
confusionMatrix(knn_predictions, data_test_stand$Churn, mode = "prec_recall", positive = "False")


#Training a Decision Tree
#We still use the function train(), specify as method rpart (recursive partitioning)
#Also, we do not need to use the standardized data with decision trees

fitDT <- train(data = data_train, method = "rpart", Churn~.)

#describe the tree in English
fitDT$finalModel
#Get the plot of the tree - 2 types of visualizations below
rpart.plot(fitDT$finalModel)
#prp(fitDT$finalModel, box.palette = "Reds", tweak = 1.2)

#get the predictions from the tree
DT_predictions <- predict(fitDT$finalModel, newdata = data_test, type = "class")

#Create the confusionMatrix and get recall, precision for both classes

confusionMatrix(DT_predictions, data_test$Churn, mode = "prec_recall", positive = "True")
confusionMatrix(DT_predictions, data_test$Churn, mode = "prec_recall", positive = "False")

#Rather than getting the class prediction, we can get the probabilities.
#that is, we can get what is the probability that a certain observation belongs to one class or the other
#by default, a cut-off of 50% is used
#Nevertheless, we can change the cutoff value depending on the context
#Example: in this case, we want to be sure to not miss any potential Churn
#In other words, we want to avoid classifying a customer as "False Churn" if there is a decent
#probability that is a "True Churn". 
#We can increase the cut-off for False, so that the algorithm will classify a customer 
#as False Churn only if the probability is really high (or equivalently, decrease the cut-off for True,
#so that a customer will be classified as True Churn even if the probability is less than 50%)

#First, get the predicted probabilities
#we still use the function predict, but now we specify type = "prob"
#also, we save the results in a dataframe
DT_prob<- as.data.frame(predict(fitDT$finalModel, newdata = data_test, type = "prob"))

#next, we create a new column in the dataframe just created that is going to assign to each
#observation "False" or "True" based on the cutoff
DT_prob$pred_class <- ifelse(DT_prob$False > 0.85, "False", "True")


#equivalently, we can decrease cut-off for True
DT_prob$pred_class2 <- ifelse(DT_prob$True > 0.15, "True", "False")

#transform the new created column into a class variable by using the as.factor()
DT_prob$pred_class2<- as.factor(DT_prob$pred_class2)

#create again the confusion matrix 
confusionMatrix(DT_prob$pred_class2, data_test$Churn, mode = "prec_recall", positive = "True")

confusionMatrix(DT_prob$pred_class, data_test$Churn, mode = "prec_recall", positive = "False")



