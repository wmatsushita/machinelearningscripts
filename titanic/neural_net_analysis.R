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

allData$CabinClass <- as.factor(sapply(allData$Cabin, function(x) substring(x,1,1)))

ages <- predictMissingValuesWithRPart(allData, "Age", c("PassengerId", "Survived", "Name", "Age"))
allData$Age[is.na(allData$Age)] <- ages

allData <- normalizeCategoricalFeatures(allData, c("CabinClass", "Salutation"))

scaled_allData <- normalizeDataForNN(allData[,!names(allData) %in% c("PassengerId", "Survived", "Name")])
clusters <- kmeans(scaled_allData[,!names(scaled_allData) %in% c("PassengerId", "Survived", "Name")], 3)
allData$Clusters <- clusters$cluster

allData <- normalizeCategoricalFeatures(allData, c("Clusters"))

scaled_allData <- normalizeDataForNN(allData[,!names(allData) %in% c("PassengerId", "Name")])
scaled_allData$PassengerId <- allData$PassengerId

scaled_train <- scaled_allData[scaled_allData$PassengerId %in% train$PassengerId,]
scaled_test <- scaled_allData[scaled_allData$PassengerId %in% test$PassengerId,]

#nn <- trainNN(scaled_train, "Survived", c("PassengerId", "Survived", "Name"), hidden=c(5,2), algorithm="sag", threshold=0.01, stepmax=20e6, linear.output=F)

