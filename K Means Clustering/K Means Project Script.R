#Read in data

df1 <- read.csv("winequality-red.csv",sep=';')
df2 <- read.csv("winequality-white.csv",sep=';')


#Clean Data
df1$label <- sapply(df1$volatile.acidity,function(x){'red'})
df2$label <- sapply(df2$volatile.acidity,function(x){'white'})

wine <- rbind(df1,df2)

#EDA
library(ggplot2)

ggplot(wine,aes(residual.sugar))+geom_histogram(aes(fill=label),color='black',bins=50) + scale_fill_manual(values = c('red','#white'))

ggplot(wine,aes(citric.acid))+geom_histogram(aes(fill=label),color='black',bins=50) + scale_fill_manual(values = c('red','white'))

ggplot(wine,aes(alcohol))+geom_histogram(aes(fill=label),color='black',bins=50) + scale_fill_manual(values = c('red','white'))

ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label), alpha=.2) + scale_color_manual(values = c('red','white')) +theme_dark()

ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label), alpha=.2) + scale_color_manual(values = c('red','white')) +theme_dark()


#Build the model

clus.data <- wine[,1:12]

wine.cluster <- kmeans(clus.data,2)
wine.cluster
wine.cluster$centers

table(wine$label,wine.cluster$cluster)



















