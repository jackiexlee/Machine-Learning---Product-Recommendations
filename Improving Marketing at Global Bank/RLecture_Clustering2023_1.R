
#https://www.kaggle.com/datasets/arjunbhasin2013/ccdata?select=CC+GENERAL.csv
library(stats)
library(standardize)
library(factoextra)
library(ggplot2)
library(plyr)

#Load the data 
data <- read.csv("CreditCardData3.csv")
#get summary
summary(data)


#Data normalization
#Option 1: Normalize with the min-max method
#Create the function normalize
#The function takes as input a vector (or column) of numbers and performs the min-max normalization

min_normalize = function(x){
  return ((x - min(x))/(max(x) - min(x)))}

#Create a copy of the dataset, so we can keep the original data
data_norm <- data

#Next, apply the normalize function to our dataset, using apply()
#We need to specify which attributes we want to normalize.
#We exclude the CUST ID, so we will use column from 2 to 8
#the option MARGIN specifies whether we want to apply the function by row or by column. 
#The = 2 means by column
#the option FUN is used to specify which function we want to apply to the data

data_norm[, 2:8] <- apply(data_norm[, 2:8], MARGIN = 2, FUN = min_normalize)

#Option2: If instead we want to use standardization
#create a copy of the data
data_stand <- data

#apply standardization on chosen attributes
#scale() function performs standardization for us
data_stand[, 2:8]<- apply(data_stand[, 2:8], MARGIN = 2, FUN = scale)



#---------------------------------------------------------#
#K-MEANS CLUSTERING
#Next, let us implement k-means, using the function kmeans()
#We need to input normalized data
#and we need to specify how many clusters we want
#NOTE: remember that k-means selects initial random points to start the process
#If we want kmeans to run for a number of different random starting point, we can use 
#the option nstart
#In the example below, R will try 15 different sets of random starting points 
#and then select the one with the lowest within cluster variation.

#Since the points are picked at random, every time we run
#the command it may change; 
#if you do not want that, because you may want to be able to replicate 
#the same analysis, use set.seed

set.seed(123)
k1 = kmeans(data_norm[, 2:8], centers = 4, nstart = 15)

#Look at a summary of the result
str(k1)

#check cluster centroids
k1$centers

#Add a column to the original data to indicate in which cluster each observation is
data$k_clust <- k1$cluster

#Use the original dataset to do summary statistics like we did before
ddply(data, .(k_clust), summarize, Balance=round(mean(BALANCE),2), Cash_Adv_Trx=round(mean(CASH_ADVANCE_TRX),2), 
      Cash_Adv_Amount = round(mean(CASH_ADVANCE),2),
      Install = round(mean(INSTALLMENTS_PURCHASES),2),
      Purchase_Trx = round(mean(PURCHASES_TRX),2),
      CreditLim = round(mean(CREDIT_LIMIT),2))


#EVALUATING CLUSTERS/CHOOSING NUMBER CLUSTERS

#Let us see how we can compute the WSS and BSS and create the Elbow Plot
#Create two empty vectors to store the results for the WSS and BSS
WSS_curve <- c()
BSS_curve <- c()

#Create a for-loop, that implements the k-means clustering for how many times we would like,
#each time increasing the number of clusters requested
#during each iteration, we also ask R to compute the WSS and BSS and store the results
#into WSS_curve and BSS_curve

#in this example, we run k-means from 1 to 20
#and with nstart = 5
for (n in 1:20) {
  k = kmeans(data_norm[,2:8], centers = n, nstart = 5)
  wss = k$tot.withinss
  bss = k$betweenss
  WSS_curve[n] <- wss
  BSS_curve[n] <-bss}

#Finally, we can plot both in the same graph
plot(1:20, WSS_curve, type = "b", col = "red", ylab = "WSS and BSS", xlab = "K", ylim=c(0,50)) 
lines(1:20, BSS_curve,type="o",col="blue")

#CAN WE PLOT THE CLUSTERS?
#Plotting the clusters would be easy if we had 2 or 3 dimensions.
#What can we do when we have more? We can use the package fviz_cluster.
#If the data contains more than 2 dimensions, the package fviz_cluster will perform something
#called principal component analysis (PCA) and plot the data points according to 
#the first two principal components that explain the majority of the variance.


#specify the clustering solution, the data used
fviz_cluster(k1, geom = "point", data=data_norm[, 2:8]) + ggtitle("Clusters")

#We could also decide to pick 2 dimensions and plot the clusters for these two
fviz_cluster(k1, geom = "point", data=data_norm[, 2:8], choose.vars = c("BALANCE", "CASH_ADVANCE_TRX")) + 
  ggtitle("Clusters")

#We can also run kmeans for different number of k 
#and plot the results in a grid
library(gridExtra)

k2 <- kmeans(data_norm[, 2:8], centers = 2, nstart = 15)
k3 <- kmeans(data_norm[, 2:8], centers = 3, nstart = 15)
k4 <- kmeans(data_norm[, 2:8], centers = 4, nstart = 15)
k5 <- kmeans(data_norm[, 2:8], centers = 5, nstart = 15)

p2 <- fviz_cluster(k2, geom = "point", data = data_norm[, 2:8]) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point", data = data_norm[, 2:8]) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = data_norm[, 2:8]) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = data_norm[, 2:8]) + ggtitle("k = 5")

grid.arrange(p2, p3, p4, p5, nrow = 2)


