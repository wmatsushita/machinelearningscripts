data <- read.csv("test.csv")

train_tree_test <- data[!is.na(data$Age), !names(data) %in% c("PassengerId", "Name", "Ticket", "Embarked", "Cabin", "Survived") ]

library(rpart)
tree_test <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = train_tree_test, method = "anova")

no_ages <- data[is.na(data$Age), !names(data) %in% c("PassengerId", "Name", "Ticket", "Embarked", "Cabin") ]

ages <- predict(tree_test, no_ages)
data$Age[is.na(data$Age)] <- ages

test_matrix <- as.matrix(sapply(data, as.numeric))

maxs <- apply(test_matrix, 2, max, na.rm = TRUE) 
mins <- apply(test_matrix, 2, min, na.rm = TRUE)

scaled <- as.data.frame(scale(test_matrix, center = mins, scale = maxs - mins))

nn_prediction <- compute(nn, scaled[,!names(scaled) %in% c("PassengerId", "Survived", "Embarked", "Name", "X", "Cabin")])

prediction <- nn_prediction$net.result

prediction <- ifelse(prediction < 0.5, 0, 1)

result <- data.frame(PassengerId = data$PassengerId, Prediction = prediction)

write.csv(result, file="titanic_prediction.csv", row.names = FALSE, col.names = FALSE)
