scaled_test <- normalizeDataForNN(test)
predNN <- predictNN(nn, scaled_test, "Survived", c("PassengerId", "Survived", "Name"))
test$Survived <- sapply(predNN$net.result, function(x) if (x > 0.5) 1 else 0)
write.csv(test[, c("PassengerId", "Survived")], file="titanic_pred_3hn_feat_eng.csv", row.names = F)
