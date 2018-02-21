#Logistic Regression Lecture
  #Load data
df.train <- read.csv("titanic_train.csv")
df.test <- read.csv("titanic_test.csv")

#Exploratory Data Analysis (EDA)
head(df.train)
print('                                               ')
str(df.train)

#Missingness Map (how many values are NA or NULL)
library(Amelia)
missmap(df.train, main='Missing map', col = c('yellow', 'black'), legend = F)

#more EDA
library(ggplot2)
ggplot(df.train,aes(Survived)) +geom_bar()

ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))

ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)))

ggplot(df.train,aes(Age)) + geom_histogram(bins=20,alpha=.5,fill='blue')

ggplot(df.train,aes(SibSp)) + geom_bar()

ggplot(df.train,aes(Fare)) +geom_histogram(fill='green',color='black',alpha=.5)

#Clean the data
pl <- ggplot(df.train, aes(Pclass, Age))
pl <- pl+ geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha= .4))
pl+scale_y_continuous(breaks = seq(min(0), max(80),by=2)) + theme_bw()

#Imputation of age based on class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
