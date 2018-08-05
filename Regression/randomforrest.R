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

metric <- "Rsquared"
metric <- "MSE"
seed <- 7 
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(medv ~ . , data= Boston, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

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
lm_default <- train(medv ~ . , data= Boston, method="lm", metric=metric, trControl=control)
print(lm_default)

# glmnet regression
eGrid <- expand.grid(.alpha=seq(0, 1, by=0.1),.lambda=seq(0,1,by=0.01))
glmnet_default <- train(medv ~ . , data= Boston, method="glmnet", metric=metric, tuneGrid = eGrid, trControl=control)
print(glmnet_default)

bayesglm_default <- train(medv ~ . , data= Boston, method="bayesglm", metric=metric, trControl=control)
print(bayesglm_default)

glmboost_default <- train(medv ~ . , data= Boston, method="glmboost", metric=metric, trControl=control)
print(glmboost_default)

rpart_default <- train(medv ~ . , data= Boston, method="rpart", metric=metric, trControl=control)
print(rpart_default)

xgbLinear_default <- train(medv ~ . , data= Boston, method="xgbLinear", metric=metric, trControl=control)
print(xgbLinear_default)


bam_default <- train(medv ~ . , data= Boston, method="bam", metric=metric, trControl=control)
print(bam_default)

knn_default <- train(medv ~ . , data= Boston, method="knn", metric=metric, trControl=control)
print(knn_default)

gbm_default <- train(medv ~ . , data= Boston, method="gbm", metric=metric, trControl=control)
print(gbm_default)

blasso_default <- train(medv ~ . , data= Boston, method="blasso", metric=metric, trControl=control)
print(blasso_default)

results <- resamples(list(lm=lm_default,rf=rf_default,glmboost=glmboost_default,xgblinear=xgbLinear_default,rpart=rpart_default))
summary(results)                     

# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"),z=list(relation="free"))
dotplot(results, scales=scales, layout = c(1,3))
parallelplot(results)
splom(results)




