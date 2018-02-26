bike <- read.csv("bikeshare.csv")  

head(bike)

#Exploratory Data Analysis (EDA)
library(ggplot2)

#EDA Scatter plot
ggplot(bike, aes(temp, count)) + geom_point(alpha=0.3, aes(color=temp)) + theme_bw()


#Convert to POSIXct
bike$datetime <- as.POSIXct(bike$datetime)

#Plot Count vs Datetime with a color gradient based on temperatture
pl <- ggplot(bike, aes(datetime, count)) +geom_point(aes(color=temp), alpha= .5)

#adjust color scale of previous plot

pl + scale_color_continuous(high='orange', low='blue') + theme_bw()

#correlation between temperature and count
cor(bike[,c('temp','count')])
cor(bike$temp,bike$count)

#Create a boxplot with the y axis indicating count and the x axis beginning a box for each season

ggplot(bike, aes(factor(season),count)) +geom_boxplot(aes(color=factor(season))) + theme_bw()

#Feature Engineering

#Create an hour coulmn that takes the info from the datetime column
bike$hour <- sapply(bike$datetime, function(x){format(x, "%H")})

#Create scatter plot of count v. hour with color scale based on temp.  Using bike data only where working day is a 1

library(dplyr)

pl <- ggplot(filter(bike, workingday==1), aes(hour, count)) +geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha= .8) + theme_bw()


pl + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))


#Linear Regression Models

#Build a linear regression model base solely off of temperature

temp.model <- lm(count ~ temp,bike)
print(summary(temp.model))

#How many bike rentals at 25 degrees c
6.0462+9.147*25
  #or
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

#Use sapply() and as.numeric to change the hour column to a column of numeric values

bike$hour <- sapply(bike$hour, as.numeric)

#Build a model that incorporates season, holiday, workingday, weather, temp, humidity, windspeed, hour as a factor

model <- lm(count ~ . -casual - registered - datetime- atemp, bike)

print(summary(model))




