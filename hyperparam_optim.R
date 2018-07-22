library(caret)
set.seed(17516)
training_data <- SLC14_1(500)
testing_data <- SLC14_1(10^5)

svm_fit <- function(x) {
  mod <- train(y ~ ., data = training_data,
               method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv"),
               tuneGrid = data.frame(C = 2^x[1], sigma = exp(x[2])))
  -getTrainPerf(mod)[, "TrainRMSE"]
}

gbm_fit <- function(x) {
  mod <- train(y ~ ., data = training_data,
               method = "gbm",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = data.frame(n.trees = floor(x[1])+1, 
                                     interaction.depth = floor(x[2])+1,
                                     shrinkage = x[3],
                                     n.minobsinnode = floor(x[4])+1),
               verbose = FALSE)
  -getTrainPerf(mod)[, "TrainRMSE"]
}
library(GA)
library(kernlab)
library(gbm)
svm_ga_obj <- ga(type = "real-valued", 
                 fitness = svm_fit,
                 ## test cost values between ~0 and 2^10,
                 ## test sigma values between exp(-5) and 1
                 min = c(-5, -5), 
                 max = c(10, 0), 
                 popSize = 50, 
                 maxiter = 12,
                 seed = 16478,
                 keepBest = TRUE,
                 monitor = NULL,
                 elitism = 2)

gbm_ga_obj <- ga(type = "real-valued", 
                 fitness = gbm_fit,
                 ## Trees between [1, 5000], 
                 ## depth between [1, 11]
                 ## shrinkage between [0, 0.2]
                 ## terminal node size in [5, 25]
                 min = c(   1,  1, 0, 5), 
                 max = c(5000, 11, .2, 25), 
                 popSize = 50, 
                 maxiter = 12,
                 seed = 513,
                 keepBest = TRUE,
                 monitor = NULL)

# yet another approach to search hyperparam
library(caret)
library(parallel)
library(doMC)

set.seed(17516)
training_data <- SLC14_1(500)
testing_data <- SLC14_1(10^5)

registerDoMC(cores = detectCores())

svm_fit <- function(x) {
  mod <- train(y ~ ., data = training_data,
               method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv"),
               tuneGrid = data.frame(C = 2^x[1], sigma = exp(x[2])))
  getTrainPerf(mod)[, "TrainRMSE"]
}

library(DEoptim)
library(kernlab)

## converged after 31 iterations
svm_de_obj <-  DEoptim(fn = svm_fit,
                       ## test cost values between ~0 and 2^10,
                       ## test sigma values between exp(-5) and 1
                       lower = c(-5, -5), 
                       upper = c(10, 0),
                       control = DEoptim.control(reltol = 1e-3,
                                                 steptol = 10,
                                                 itermax = 100))


fitted_params <- svm_de_obj$optim$bestmem

svm_model <- train(y ~ ., data = training_data,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = data.frame(C = 2^fitted_params[1], 
                                         sigma = exp(fitted_params[2])))

predictions <- predict(svm_model, testing_data)

cat("Validation RMSE:", getTrainPerf(svm_model)[, "TrainRMSE"], "\n")
cat("Test RMSE:", RMSE(predictions, testing_data$y))








