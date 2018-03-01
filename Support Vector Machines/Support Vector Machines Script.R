#i will be using the iris dataset in the ISLR package
library(ISLR)

#To use the support vector machine algoritm install and call the e1071 package
library(e1071)

#create an svm model
model <- svm(Species ~., data =iris)
summary(model)

#tuning the model

    #cost is what allows the SVM to have what is known as a "soft Margin."  This means that the SVM allows for some of the data to be ignored or placed on the wrong side of the margin. 
    #a small gamma means a large variance which means the influence of the support vector is greater. If the gamma is large the variance is small.  Large gamma leads to high bias and low variance and small gamma gives low bias and high variance.  

tune.results <- tune(svm,train.x = iris[1:4],train.y = iris[,5], kernel='radial', ranges=list(cost=c(.1,1,10),gamma=c(.5,1,2)))
summary(tune.results)

#Once you get the best parameters you can
tuned.svm <- svm(Species~.,data=iris,kerenel='radial',cost=1.5,gamma=.1)
summary(tuned.svm)
