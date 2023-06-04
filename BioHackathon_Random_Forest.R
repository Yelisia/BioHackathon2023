##@Article{,
##    title = {Classification and Regression by randomForest},
##    author = {Andy Liaw and Matthew Wiener},
##    journal = {R News},
##    year = {2002},
##    volume = {2},
##    number = {3},
##    pages = {18-22},
##    url = {https://CRAN.R-project.org/doc/Rnews/},
##  }

## Starmer, John, director. StatQuest: Random Forests in R. YouTube, YouTube, 26 Feb. 2018, https://www.youtube.com/watch?v=6EXPYzbfLCE. 

##Section 1: build the model

df <- read.csv("~/Desktop/train_don't_panic.csv")

df$Age <- as.factor(df$Age)
df$Feeling.sad.or.Tearful <- as.factor(df$Feeling.sad.or.Tearful)
df$Irritable.towards.baby...partner <- as.factor(df$Irritable.towards.baby...partner)
df$Trouble.sleeping.at.night <- as.factor(df$Trouble.sleeping.at.night)
df$Problems.concentrating.or.making.decision <- as.factor(df$Problems.concentrating.or.making.decision)
df$Overeating.or.loss.of.appetite <- as.factor(df$Overeating.or.loss.of.appetite)
df$Feeling.anxious <- as.factor(df$Feeling.anxious)
df$Feeling.of.guilt <- as.factor(df$Feeling.of.guilt)
df$Problems.of.bonding.with.baby <- as.factor(df$Problems.of.bonding.with.baby)
df$Suicide.attempt <- as.factor(df$Suicide.attempt)

##Splitting inside of R didn't work so excel did the job of splitting train and test data sets
##library("dplyr")
##sample <- sample.split(df, SplitRatio = 0.8) 

##data_train <- subset(data, split == "TRUE") 
##data_test <- subset(data, split == "FALSE") 

##library(randomForest)

model <- randomForest(Suicide.attempt ~ ., data = df, proximity = TRUE)

plot(model)
model

##plot the error rates as more trees were used
oob.error.data <- data.frame(Trees = rep(1:nrow(model$err.rate), times = 3), Type = rep(c("OOB", "Had Suicide Attempts", "No Previous Suicide Attempts"), each = nrow(model$err.rate)), Error = c(model$err.rate[, "OOB"], model$err.rate[, "1"], model$err.rate[, "0"]))
library(ggplot2)
ggplot(data = oob.error.data, aes(x = Trees, y = Error)) + geom_line(aes(color = Type))

##plot the important variables identified by mean decrease in gini
varImpPlot(model)


##Section 2: use the model on test data and see the confusion matrix
df2 <- read.csv("~/Desktop/test_don't_panic.csv")

df2$Age <- as.factor(df2$Age)
df2$Feeling.sad.or.Tearful <- as.factor(df2$Feeling.sad.or.Tearful)
df2$Irritable.towards.baby...partner <- as.factor(df2$Irritable.towards.baby...partner)
df2$Trouble.sleeping.at.night <- as.factor(df2$Trouble.sleeping.at.night)
df2$Problems.concentrating.or.making.decision <- as.factor(df2$Problems.concentrating.or.making.decision)
df2$Overeating.or.loss.of.appetite <- as.factor(df2$Overeating.or.loss.of.appetite)
df2$Feeling.anxious <- as.factor(df2$Feeling.anxious)
df2$Feeling.of.guilt <- as.factor(df2$Feeling.of.guilt)
df2$Problems.of.bonding.with.baby <- as.factor(df2$Problems.of.bonding.with.baby)
df2$Suicide.attempt <- as.factor(df2$Suicide.attempt)

pred_test <- predict(model, newdata = df2, type = "class")

##prepare the packages; caret is used for confusionMatrix function and R requires lattice package to use caret package
install.packages("caret")
install.packages("lattice")
library(caret)
confusionMatrix(pred_test, df2$Suicide.attempt)


##Section 3: see how many percentage of mother who reported "not interested to say" actually had suicide attempts based on our model
df3 <- read.csv("~/Desktop/Suicide_NA_don't_panic.csv")

df3$Age <- as.factor(df3$Age)
df3$Feeling.sad.or.Tearful <- as.factor(df3$Feeling.sad.or.Tearful)
df3$Irritable.towards.baby...partner <- as.factor(df3$Irritable.towards.baby...partner)
df3$Trouble.sleeping.at.night <- as.factor(df3$Trouble.sleeping.at.night)
df3$Problems.concentrating.or.making.decision <- as.factor(df3$Problems.concentrating.or.making.decision)
df3$Overeating.or.loss.of.appetite <- as.factor(df3$Overeating.or.loss.of.appetite)
df3$Feeling.anxious <- as.factor(df3$Feeling.anxious)
df3$Feeling.of.guilt <- as.factor(df3$Feeling.of.guilt)
df3$Problems.of.bonding.with.baby <- as.factor(df3$Problems.of.bonding.with.baby)
df3$Suicide.attempt <- as.factor(df3$Suicide.attempt)

pred_2 <- predict(model, newdata = df3)
pred_2
summary(pred_2)
