library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$Conj <- 0
train$Conj[is.na(train$Age) == FALSE & train$Age >= 18] <- train$SibSp[is.na(train$Age) == FALSE & train$Age >= 18]

test$Conj <- 0
test$Conj[is.na(test$Age) == FALSE & test$Age >= 18] <- test$SibSp[is.na(test$Age) == FALSE & test$Age >= 18]

tree <- rpart(Age ~ Suvived + Pclass + Sex + SibSp + Conj + Parch + Fare, data = train, method = "class")
              
fancyRpartPlot(tree, cex=0.7)

my_prediction <- predict(tree, test, type="class")

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)