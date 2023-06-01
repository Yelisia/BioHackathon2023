##@Manual{,
##    title = {ISLR: Data for an Introduction to Statistical Learning with Applications in R},
##    author = {Gareth James and Daniela Witten and Trevor Hastie and Rob Tibshirani},
##    year = {2021},
##    note = {R package version 1.4},
##    url = {https://CRAN.R-project.org/package=ISLR},
##  }


data <- read.csv("~/Desktop/Suicide_Yes_and_No_10.csv")

library(ISLR)
set.seed(1)

data$Age <- as.factor(data$Age)
data$Feeling.sad.or.Tearful <- as.factor(data$Feeling.sad.or.Tearful)
data$Irritable.towards.baby...partner <- as.factor(data$Irritable.towards.baby...partner)
data$Trouble.sleeping.at.night <- as.factor(data$Trouble.sleeping.at.night)
data$Problems.concentrating.or.making.decision <- as.factor(data$Problems.concentrating.or.making.decision)
data$Overeating.or.loss.of.appetite <- as.factor(data$Overeating.or.loss.of.appetite)
data$Feeling.anxious <- as.factor(data$Feeling.anxious)
data$Feeling.of.guilt <- as.factor(data$Feeling.of.guilt)
data$Problems.of.bonding.with.baby <- as.factor(data$Problems.of.bonding.with.baby)
data$Suicide.attempt <- as.factor(data$Suicide.attempt)

sample <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[sample, ]
test <- data[!sample, ]

model <-glm(Suicide.attempt~Feeling.anxious+Age+Feeling.sad.or.Tearful+Irritable.towards.baby...partner+Trouble.sleeping.at.night+Problems.concentrating.or.making.decision+Overeating.or.loss.of.appetite+Feeling.of.guilt+Problems.of.bonding.with.baby, family = "binomial", data = train)

summary(model)

citation("ISLR")