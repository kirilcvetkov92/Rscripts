rm(list = ls())
library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
library(titanic)
dataset <- titanic_train
dataset <- na.omit(dataset)
dataset$Survived <- as.factor(dataset$Survived)
dataset$PassengerId <- NULL
dataset$Name <- NULL
dataset$Ticket <- NULL
dataset$Cabin <- NULL
x <- dataset[,2:ncol(dataset)]
y <- dataset[,1]

idx <- createDataPartition(dataset$Survived, p = 0.8, list=FALSE)
train <- dataset[idx,]
test <- dataset[-idx,]


metric <- "Kappa"

seed <- 7 

control <- trainControl(method="repeatedcv",
                        number=10,
                        repeats=5,
                        search = "random")
set.seed(seed)

modellist <- data.frame(model=character(),
                        Accuracy=numeric(),
                        Kappa=numeric(),
                        stringsAsFactors = FALSE)
#algos <- list("lm","rpart","knn")
algos <- list("glm","nb","svmLinear","rpart","rf","knn")
# linear model, boosted lin mod, decision tree, kNN
i<-1
#windows(width = 20, height = 16)
#par(mfrow=c(length(algos),2))
for (algo in algos) {
  print(algo)
  model_default <- train(Survived ~ . ,
                         data= train,
                         method=algo, 
                         metric=metric,
                         trControl=control,
                         #importance = TRUE, 
                         #preProcess = c("center","scale"),
                         tuneLength = 10
  )
  pred <- predict(model_default, newdata = test)
  modelvalues <- data.frame(obs = test$Survived, pred=pred)
  #residuals <- resid(model_default)
  #plot(residuals)
  #abline(0,0)
  #plot(test$medv,pred)
  mod_summary <- defaultSummary(modelvalues)
  modellist[i,1] <- algo
  modellist[i,2:3] <- mod_summary
  i<-i+1
}
modellist
modelLookup("knn")



