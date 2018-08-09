rm(list = ls())
library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
library(MASS)
attach(Boston)
dataset <- Boston
x <- dataset[,2:ncol(dataset)]
y <- dataset[,1]

idx <- createDataPartition(Boston$medv, p = 0.8, list=FALSE)
train <- Boston[idx,]
test <- Boston[-idx,]

metric <- "Rsquared"

seed <- 7 

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(medv ~ . , data= train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, importance = TRUE)
print(rf_default)

rf_pred <- predict(rf_default, newdata = test)
modelvalues <- data.frame(obs = test$medv, pred=rf_pred)
rf_summary <- defaultSummary(modelvalues)
rf_summary

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(medv ~., data = Boston, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:ncol(x)))
rf_gridsearch <- train(medv ~., data = Boston, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Create model with optimal paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(seed)
mtry <- 4
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(medv ~ . , data= Boston, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

# lm regression
lm_default <- train(medv ~ . , data = train, method="lm", metric=metric, trControl=control)
print(lm_default)

lm_pred <- predict(lm_default, newdata = test)
modelvalues <- data.frame(obs = test$medv, pred=lm_pred)
lm_summary <- defaultSummary(modelvalues)
lm_summary

# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"),z=list(relation="free"))
dotplot(results, scales=scales, layout = c(1,3))
parallelplot(results)
splom(results)




