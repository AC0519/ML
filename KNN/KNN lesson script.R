#We are using the Caravan data set inside the ISLR package.  This data is a bunch of variables that eventually lead to the final factor of if they purchsed insurance from this company or not.  

#Get the data
install.packages("ISLR")
library(ISLR)

#remove NA values
any(is.na(Caravan))

#When dealing with KNN it is important to standardize the variables.  Since KNN predicts the value of a new observation based on how close it is to known variables, the scale is very important. 

#Check variance of features
var(Caravan[,1])
var(Caravan[,2])

#Standardize the x variables except for purchase.
purchase <- Caravan$Purchase

standardized.Caravan <- scale(Caravan[,-86])
var(standardized.Caravan[,1:5])

#Train/Test Split
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]


###################

#KNN Model

library(class)
set.seed(101)

predicted.purchase <- knn(train.data,test.data,train.purchase, k=9)

missclass.error <- mean(test.purchase != predicted.purchase)

missclass.error


#Choosing a K value

predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase, k=i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

error.rate

#Visualize K elbow
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)

error.df

ggplot(error.df,aes(k.values,error.rate)) + geom_point() + geom_line(lty='dotted',color='red')
 























