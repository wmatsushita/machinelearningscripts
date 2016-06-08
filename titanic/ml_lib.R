readData <- function(file = "train.csv") {
  
  library(dplyr)
  # Loads the train data
  data <- tbl_df(read.csv(file))
  # Prints the NA count for all the columns in the data
  print(apply(data, 2, function(x) sum(is.na(x))))
  
  data
  
}


predictMissingValuesWithRPart <- function (data, class, features, exclude=TRUE, method="anova", type="vector") {
  
  # Subsets the data to only train with data that have the class and the desired features
  train_columns <- if(exclude) setdiff(features, class) else c(features, class)
  tree_train <- data[ !is.na(data[class]), if(exclude) !names(data) %in% train_columns else train_columns ]
  
  
  n <- names(tree_train)
  f <- as.formula(paste(class, " ~ ", if(exclude) paste(n[!n %in% c(features)], collapse = " + ") else paste(features, collapse = " + ")))
  
  library(rpart)
  tree <- rpart(f, data = tree_train, method = method)
  
  #library(rattle)
  #library(rpart.plot)
  #library(RColorBrewer)
  #fancyRpartPlot(tree, cex=0.7)
  
  # Make sure features is appropriate regarding containing or not class
  # If exclude is TRUE, features must contain class otherwise it must not contain it
  test_columns <- if(exclude) c(features, class) else setdiff(features, class)
  
  tree_test <- data[ is.na(data[class]), if(exclude) !names(data) %in% test_columns else test_columns ]
  pclass <- predict(tree, tree_test, type)
  
}

normalizeDataForNN <- function (data) {
  
  data <- as.matrix(sapply(data, as.numeric))
  
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
}

trainNN <- function (data, class, features, exclude=TRUE, ...) {
  
  n <- names(data)
  f <- as.formula(paste(class, " ~ ", if(exclude) paste(n[!n %in% c(features)], collapse = " + ") else paste(features, collapse = " + ")))  
  
  print(paste("Training began at: ", format(Sys.time(), "%H:%M:%S of %a %b %d %Y"), " ..."))
  
  library(neuralnet)
  nn <- neuralnet(f,data=data, ...)
  
  print(paste("Training ended at: ", format(Sys.time(), "%H:%M:%S of %a %b %d %Y"), " ..."))
  
  plot(nn)
  
  nn
  
}

predictNN <- function (nn, data, class, features, exclude=TRUE, ...) {
  
  test_columns <- if(exclude) c(features, class) else setdiff(features, class)
  test_data <- data[, if (exclude) !names(data) %in% test_columns else test_columns ]
  
  nn_prediction <- neuralnet::compute(nn, test_data)
  #prediction <- ifelse(nn_prediction$net.result < 0.5, 0, 1)
  #prop.table(table(pred = prediction, fact = test_$Survived))
  
}

writePredictionFile <- function(nn, data, id, class, features, exclude=TRUE, fileName="prediction.csv") {
  
  scaled_test <- normalizeDataForNN(test)
  predNN <- predictNN(nn, scaled_test, class, features, exclude)
  test[class] <- sapply(predNN$net.result, function(x) if (x > 0.5) 1 else 0)
  write.csv(test[, c(id, class)], file=fileName, row.names = F)
  
}

checkForNAs <- function (data) {
  
  print(apply(data, 2, function(x) sum(is.na(x))))
  
}

normalizeCategoricalFeatures <- function (data, features) {
  #factor_cols <- sapply(names(allData), function(colName) is.factor(allData[1,colName]) & nlevels(allData[1,colName] > 2))
  #factor_cols <- factor_cols[factor_cols==TRUE]
  
  for(colName in features) {
    for(level in levels(data[[colName]])) {
      newColName <- paste(colName, "Eq", level, sep = "")
      data[[newColName]] <- 0
      data[data[[colName]] == level, newColName] <- 1
    }
    data[[colName]] <- NULL
  }
  
  data
}