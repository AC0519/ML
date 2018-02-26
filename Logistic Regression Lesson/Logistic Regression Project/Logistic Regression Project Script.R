######Read in Data
adult <- read.csv("adult_sal.csv")


######EDA and Cleaning Data
library(dplyr)
adult <- select(adult,-X)

table(adult$type_employer)

#Join Never-worked and Without-pay together

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked'| job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemp)

table(adult$type_employer)

#Join State and Local go

gov <- function(job){
  job <- as.character(job)
  if (job=='State-gov'| job=='Local-gov'){
    return('SL-gov')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, gov)

table(adult$type_employer)

#Join both forms of Self employment

selfemp <- function(job){
  job <- as.character(job)
  if (job=='Self-emp-inc'| job=='Self-emp-not-inc'){
    return('Self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, selfemp)

table(adult$type_employer)

#Create Married Column
marriage <- function(mar){
  mar <- as.character(mar)
  if (mar=='Married-AF-spouse'| mar=='Married-civ-spouse' | mar=='Married-spouse-absent'){
    return('Married')
  }else if (mar=='Divorced' | mar=='Widowed' | mar=='Separated'){
    return('Not-Married')
  }else{
    return(mar)
  }
}

adult$marital <- sapply(adult$marital, marriage)

table(adult$marital)

#####Group Countries

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

table(adult$country)

adult$type_employer <- factor(adult$type_employer)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)



#Group Education levels

education <- function(edu){
  edu <- as.character(edu)
  if (edu== 'Assoc-acdm' | edu=='Assoc-voc'){
    return('Associates')
  }else if (edu== 'Bachelors'){
    return(edu)
  }else if (edu== 'Masters' | edu== 'Prof-school'){
    return(Masters)
  }else if (edu== 'HS-grad' | edu=='Some-college') 
  
}













