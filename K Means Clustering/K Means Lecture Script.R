#This will be using the iris data set.  However, since we are taking an unsupervised approach we will not let the algorithim see the data labels.  We will only use the labels later to evaluate the algorithm.  
library(ISLR)
library(ggplot2)

#These are what the cluster should look like.  I am showing this as an example to compare to later.
pl <- ggplot(iris,aes(Petal.Length, Petal.Width,color=Species))
pl + geom_point(size=4)

#K means model
set.seed(101)

iriscluster <- kmeans(iris[,1:4],3,nstart=20)
iriscluster

table(iriscluster$cluster,iris$Species)

#Plot the clusters.  Clusplot attmepts to plot the two features that explain the most variability.  This works less and less for the more features your data has. 
library(cluster)
clusplot(iris,iriscluster$cluster, color=T,shade=T,labels=0,lines=0)
