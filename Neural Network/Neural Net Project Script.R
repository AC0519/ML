#The point of this project is to use a neural net to see if bank notes are real or fake.  The class column indicates whether a bank note is authentic.  We do not have to scale our data in this instance because our values were relatively close to each other.  Anytime that there is a large difference between data types in the same column you need to normalize the data.

#read in data
df <- read.csv('bank_note_data.csv')

#train/test split

library(caTools)
set.seed(101)
split <- sample.split(df$Class, SplitRatio = .7)
train <- subset(df, split==T)
test <- subset(df, split==F)

#Build Model
library(neuralnet)

nn <-neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train,hidden = c(5,3), linear.output = F)

predicted.nn.values <- compute(nn,test[1:4])
predictions <- sapply(predicted.nn.values$net.result,round)

table(predictions,test$Class)


#Compare against a random forest model since we got perfect results and we should be suspicious of this.

library(randomForest)

df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

rf.model <- randomForest(Class ~.,data = train)

rf.pred <- predict(rf.model,test)

table(rf.pred,test$Class)

#Since the random forest predicted so well, it does show that the perfect results obtained by the neural network are valid
