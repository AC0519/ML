adult <- read.csv('adult_sal.csv')

library(dplyr)
adult <- select(adult,-X)

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)


#Format Education Variable
education <- function(edu){
  edu <- as.character(edu)
  if(edu=='Preschool'|edu=='10th'|edu=='11th'|edu=='12th'|edu=='1st-4th'|edu=='5th-6th'|edu=='7th-8th'|edu=='9th'){
    return('LessThanHS')
  }else if (edu=='HS-grad'|edu=='Some-college'){
    return('HS')
  } else if(edu=='Assoc-acdm'|edu=='Assoc-voc'){
    return('Associates')
  }else if(edu=='Bachelors'){
      return(edu)
  }else if (edu=='Masters'|edu=='Prof-school'){
    return('Masters')
  }else{
    return(edu)
  }
}

adult$education <- sapply(adult$education,education)
adult$education <- factor(adult$education)

######Missing Data
library(Amelia)

adult[adult=='?'] <- NA

any(is.na(adult))


adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$education <- factor(adult$education)


missmap(adult,y.at=c(1),y.labels = c(""),col=c('yellow','black'), legend = F)

#Drop Missing Data
adult <- na.omit(adult)

#EDA
library(ggplot2)
library(dplyr)

ggplot(adult,aes(age))+geom_histogram(aes(fill=income),color='black',binwidth = 1) +theme_bw()

ggplot(adult,aes(hr_per_week)) + geom_histogram() +theme_bw()

adult <- rename(adult,region=country)

pl <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black') +theme_bw()
pl


######Logistic Regression Model (Does someone make more or less than $50k a year)

#Train Test split
library(caTools)
set.seed(101)
sample <- sample.split(adult$income,SplitRatio = .7)
train <- subset(adult,sample==T)
test <- subset(adult,sample==F)

#model
model <- glm(income ~ ., family=binomial(link=logit),data=train)

new.step.model <- step(model)

test$predicted.income <- predict(model,newdata=test,type='response')

testdf <- as.data.frame(test)

#confusion matrix
table(test$income,test$predicted.income > .5)

#Accuracy of model
acc <- ((6380+1419)/9215) *100 
acc








