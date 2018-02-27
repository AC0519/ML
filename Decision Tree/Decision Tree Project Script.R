#Using the College dataframe contained in ISLR package
library(ISLR)

######EDA
library(ggplot2)

ggplot(College, aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private), size=4,alpha=.5)

ggplot(College, aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50,alpha=.5)

ggplot(College, aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50,alpha=.5)

#The plot above showed a graduation rate of greater than 100 percent.  I want to find out what the name of the college is and then fix the error
subset(College,Grad.Rate > 100)

College['Cazenovia College','Grad.Rate'] <- 100

#####Train/Test Split
library(caTools)
set.seed(101)
sample <- sample.split(College$Private, SplitRatio = .7)
train.college <- subset(College, sample==T)
test.college <- subset(College, sample==F)

library(rpart)

tree <- rpart(Private ~., method = 'class',data = train.college)

tree.predictions <- predict(tree, test.college)

#turn tree.preditions into a single column with a yes or no result using a function

tree.predictions <- as.data.frame(tree.predictions)

joiner <- function(x){
  if (x >=.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.predictions$Private <- sapply(tree.predictions$Yes,joiner)
head(tree.predictions)

table(tree.predictions$Private,test.college$Private)

###Plot decision tree
library(rpart.plot)
prp(tree)


#########################
#Using Random Forests on this data

library(randomForest)

rf.model <- randomForest(Private~.,data=train.college,importance=T)

rf.model$confusion
rf.model$importance

rf.predictions <- predict(rf.model, test.college)
table(rf.predictions,test.college$Private)




















