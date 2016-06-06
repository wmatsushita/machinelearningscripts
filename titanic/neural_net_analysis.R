train <- readData("train.csv")
test <- readData("test.csv")

test$Survived <- 0
allData <- rbind(train, test)

allData$Fare[is.na(allData$Fare)] <- mean(allData$Fare, na.rm = TRUE)

allData$FamilySize <- allData$SibSp + allData$Parch + 1
allData$Family <- allData$FamilySize > 1

dummy <- strsplit(as.character(allData$Name), ",")
allData$Salutation <- as.factor(
  sapply(dummy, function(x) {
    tokens <- strsplit(x[2], " "); 
    sapply(tokens, function(x) x[2])
  }))

ages <- predictMissingValuesWithRPart(allData, "Age", c("PassengerId", "Survived", "Name", "Age"))
allData$Age[is.na(allData$Age)] <- ages

train <- allData[allData$PassengerId %in% train$PassengerId,]
test <- allData[allData$PassengerId %in% test$PassengerId,]

scaled_train <- normalizeDataForNN(train)
nn <- trainNN(scaled_train, "Survived", c("PassengerId", "Survived", "Name"), hidden=c(5,2), algorithm="sag", threshold=0.01, stepmax=20e6, linear.output=F)

