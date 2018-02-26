library(ISLR)
head(iris)

any(is.na(iris))

#Standardize data

var(iris[,1])
var(iris[,2])

standardiris <- scale(iris[,1:4])

var(standardiris)

final.data <- cbind(standardiris,iris[5])

#Train/Test Split

set.seed(101)
library(caTools)
 sample <- sample.split(final.data$Species, SplitRatio = .7)
 
 train <- subset(final.data, sample == T)
 test <- subset(final.data, sample==F)
 
#KNN
library(class)
 
predict.species <- knn(train[1:4], test[1:4], train$Species, k=3)

misclass <- mean(test$Species != predict.species)
misclass

### Find best K 

predict.species <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predict.species <- knn(train[1:4], test[1:4], train$Species, k=i)
  error.rate[i] <- mean(test$Species != predict.species)
}

error.rate


# Plot error.rate against k's
kvalues <- 1:20
error.df <- data.frame(error.rate,kvalues)


library(ggplot2)

ggplot(error.df,aes(kvalues,error.rate)) + geom_point() + geom_line(lty='dotted',color='red')


  
  
  
  
  
  
  
  
  
  









