data <- read.csv("train.csv")

apply(data,2,function(x) sum(is.na(x)))

train_tree <- data[!is.na(data$Age), !names(data) %in% c("PassengerId", "Name", "Ticket", "Embarked", "Cabin") ]

library(rpart)
tree <- rpart(Age ~ Survived + Pclass + Sex + SibSp + Parch + Fare, data = train_tree, method = "anova")

no_ages <- data[is.na(data$Age), !names(data) %in% c("PassengerId", "Name", "Ticket", "Embarked", "Cabin") ]

ages <- predict(tree, no_ages)
data$Age[is.na(data$Age)] <- ages

data <- as.matrix(sapply(data, as.numeric))

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
#library(e1071)

n <- names(train_)
f <- as.formula(paste("Survived ~", paste(n[!n %in% c("PassengerId", "Survived", "Embarked", "Name", "X", "Cabin")], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=F, learningrate = 0.005, stepmax = 2e6, act.fct = "logistic")
#svm.model <- svm(f, data = train_, cost = 100, gamma = 1)

plot(nn)

#svm.pred <- predict(svm.model, test_[,!names(test_) %in% c("Survived")])

nn_prediction <- compute(nn, test_[,!names(test_) %in% c("PassengerId", "Survived", "Embarked", "Name", "X", "Cabin")])
prediction <- ifelse(nn_prediction$net.result < 0.5, 0, 1)
prop.table(table(pred = prediction, fact = test_$Survived))



