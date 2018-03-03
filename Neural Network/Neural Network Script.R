#We are going to build a neural network to predict median value of housing from the Boston data set

#import data from mass package 
library(MASS)

data <- Boston

#Whenever you are training a neural network it is good practice to normalize your data.

#Find the max and min values of each of the columns
maxs <- apply(data,2,max)
mins <- apply(data,2,min)

#use the max and min values to scale "normalize" the data.  Using the center arguement means each data point in the corresponding column has the minimum value of that column subtracted from it.  The scale arguemnent means that every data point is then divided by the maximum value minus the minimum value.
scaled.data <- scale(data,center = mins, scale = maxs-mins)
#the above line turns everything into a matrix but I need a data frame.  So:
scaled <- as.data.frame(scaled.data)

#turn data into a training/test sets
library(caTools)

split <- sample.split(scaled$medv, SplitRatio = .7)

train <- subset(scaled, split==T)
test <- subset(scaled, split==F)

#######Build and train the model
library(neuralnet)

#this formula is strange in that it does not take the y ~. arguement as a way to bring in all of the columns.  You would have to type all of the ones that you want individually.  To avoid that, use the trick below. 
n <- names(train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f,data = train,hidden = c(5,3),linear.output = T) #If you were performing a classification problem with the neural net the liner.output would be set to false but since I want continuos values returned, it is set to true.

plot(nn)

######create predictions with the model

predicted.nn.values <- compute(nn,test[1:13])

#Since I scaled and normalized the data I need to undo those operations to return valid results 
true.predictions <- predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

#this needs to be done to the test data as well
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)

error.df <- data.frame(test.r,true.predictions)


#plot the results
library(ggplot2)


ggplot(error.df,aes(test.r,true.predictions)) + geom_point() +stat_smooth()







