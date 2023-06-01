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

df <- read.csv("~/Desktop/Suicide_Yes_and_No.csv")

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

model <- randomForest(Suicide.attempt ~ ., data = df, proximity = TRUE)

plot(model)

oob.error.data <- data.frame(Trees = rep(1:nrow(model$err.rate), times = 3), Type = rep(c("OOB", "Had Suicide Attempts", "No Previous Suicide Attempts"), each = nrow(model$err.rate)), Error = c(model$err.rate[, "OOB"], model$err.rate[, "Yes"], model$err.rate[, "No"]))
library(ggplot2)
ggplot(data = oob.error.data, aes(x = Trees, y = Error)) + geom_line(aes(color = Type))
ggplot(data = oob.error.data, aes(x = Trees, y = Error)) + geom_line(aes(color = Type))

install.packages("dplyr")
varImpPlot(model)