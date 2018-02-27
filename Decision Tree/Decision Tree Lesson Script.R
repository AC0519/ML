#Using the kyphosis data set that is inherent to R
#install and call "rpart" and "rpart.plot" packages
library(rpart)
library(rpart.plot)

tree <- rpart(Kyphosis ~.,method='class', data=kyphosis)
printcp(tree)
prp(tree)

#Random Forests improve predictive accuracy by generating a large number of bootstrap trees based on random samples of the variables, classifying a case using each tree in this new forest, and deciding a final predicted outcome by combining the results across all of the trees.  In classification it is a majority vote of the trees

#Install and call randomForest package
library(randomForest)

rf.model <- randomForest(Kyphosis ~.,data = kyphosis)
rf.model
rf.model$predicted
rf.model$confusion
