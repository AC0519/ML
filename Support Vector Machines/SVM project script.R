loans <- read.csv("loan_data.csv")

#Clean Data
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

#EDA
library(ggplot2)

#Histogram of "not fully paid" by credit score
ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black', bins = 40, alpha=.5) + theme_bw() +scale_fill_manual(values = c('green','red'))

#Barplot of "not fully paid" by purpose of the loan.  The position='dodge' arguement allows the bars to be offset from each other rather than stacked on top of each other.  
ggplot(loans,aes(purpose)) + geom_bar(aes(fill=not.fully.paid), position='dodge') + theme_bw() +scale_fill_manual(values = c('green','red'))

#scatter plot of credit score vs. interest rate
ggplot(loans, aes(int.rate, fico)) + geom_point() +theme_bw() 
ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color=not.fully.paid), alpha=.3) +theme_bw() 

#Build a SVM model
library(caTools)

set.seed(101)

sample <- sample.split(loans$not.fully.paid,.7)

train.loans <- subset(loans, sample==T)
test.loans <- subset(loans, sample==F)

library(e1071)

svm.model <- svm(not.fully.paid ~.,data=train.loans)
summary(svm.model)

#use model to predict new values
predict.svm.model <- predict(svm.model,test.loans[1:13])

table(predict.svm.model,test.loans$not.fully.paid)


#The results are terrible.  Try to tunre the model.
tune.svm.model <- tune(svm,train.x = not.fully.paid~.,data = train.loans,kernel='radial',ranges=list(cost=c(1,10),gamma=c(.1,1)))








