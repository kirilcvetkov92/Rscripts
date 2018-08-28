rm(list = ls())
library(foreach)
library(doParallel)
library(iterators)
# clean_data from generate sample from distribution
#source("Rscripts\\COST\\generate_distribution.R")

clean_data <- read.csv("COST\\clean_data.csv") 
#
#clean_data <- read.csv("clean_data.csv")
clean_data$X <- NULL
feature_names <- names(clean_data)
iter <- nrow(clean_data)

library(rpart)
#library(rattle)
#library(NoiseFiltersR)
#noisy_data$Service.Model <- as.factor(noisy_data$Service.Model)
#out <- C45robustFilter(Service.Model ~.,data = noisy_data) 
#cldata <- out$cleanData
#print(out)
#identical(out$cleanData, noisy_data[setdiff(1:nrow(noisy_data),out$remIdx),])
library(mlbench)
library(caret)
#trainmodel <- function(algo,data,metric,control,tunelen){
#   model <- train(Service.Model ~ . ,
#                  data=data,
#                  method=algo, 
#                  metric=metric,
#                  trControl=control,
#                  tuneLength = tunelen)
#   
#   return(model)
# } 
#multiResultClass <- function(result1=NULL,result2=NULL){
#   me <- list(result1 = result1,result2 = result2)
#   class(me) <- append(class(me),"multiResultClass")
#   return(me)
# }
control <- trainControl(method="repeatedcv",
                        number=2,
                        repeats=1,
                        allowParallel = FALSE,
                        search = "random",
                        verboseIter = TRUE)
tunelen <- 3

#algos <- list("glm","nb","svmLinear","rpart2","rf","knn")
#algos <- c("rpart2","nb","rf","adaboost","xgbLinear")
# get all model names for classification
m <- unique(modelLookup()[modelLookup()$forClass,c(1)])

algos <- c("xgbTree","xgbLinear","rf")
#algos <- c("rpart2","nb")
#metric <- "Kappa"
cmodellist <- array(0,dim=c(length(algos),3,1))

noisy_list <- c(0,10,20,30,40,50)
noisy_list <- c(0,10)
pkg <- c("caret")

cl <- makeCluster(detectCores())
registerDoParallel(cl)
datalist <- foreach(i = 1:length(noisy_list), .combine = "list") %dopar% {
  labelnoise <- noisy_list[i]
  noisy_data <- clean_data
  resample <- sample.int(iter, iter/100*labelnoise)
  mylabels <- unique(clean_data$Service.Model)
  for(k in resample){
    myset <- noisy_data[k,]
    noisy_data[k,1] <- sample(mylabels[!(myset$Service.Model == mylabels)],1)
}
return(noisy_data)
}

result <- foreach(data = datalist,j=icount(), .combine = rbind) %:% 
  foreach(algo = algos, .combine = rbind,.packages = pkg) %dopar% {
      #data <- datalist[[1]]
      #algo <- algos[[1]]
      #print(data)
      set.seed(1234)
      start.time <- Sys.time()
      model <- train(Service.Model ~ . ,
                                      data= data,
                                      method=algo, 
                                      #metric=metric,
                                      trControl=control,
                                      tuneLength = tunelen)
      end.time <- Sys.time()
      time <- end.time - start.time
      #modellist[j] <- model$finalModel 
      kappa <- max(model$results$Kappa)
      accuracy <- max(model$results$Accuracy)
      #datavec[j] <- datachr
      #print(datavec)
      #return(accuracy)
  
  return(data.frame(noise=noisy_list[j], algo = algo,time = time ,kappa = kappa, accuracy = accuracy))
  #print(data.frame(k = j, algo = algos, kappa = kappa, accuracy = accuracy))
}
stopCluster(cl)

write.csv(result,"result.csv")

# mytree_clean <- model_clean_default$finalModel
# mytree_noisy <- model_noisy_default$finalModel
# mytree_noisy_rm <- model_noisy_rm_default$finalModel
# max(model_clean_default$results$Accuracy)
# max(model_noisy_default$results$Accuracy)
# #fancyRpartPlot(mytree_clean, type = 3)
# model_noisy_default <- train(Service.Model ~ . ,
#                              data= noisy_data,
#                              method=algo, 
#                              metric=metric,
#                              trControl=control,
#                              tuneLength = tunelen)
# model_noisy_rm_default <- train(Service.Model ~ . ,
#                                 data= cldata,
#                                 method=algo, 
#                                 metric=metric,
#                                 trControl=control,
#                                 tuneLength = tunelen)
# 
# 
# library(ggplot2)
# feature_names
# ggplot(clean_data, aes_string(feature_names[1],feature_names[5])) + geom_boxplot(aes(colour = Service.Model))
# library(rgl)
# hist3d(clean_data$AuM,clean_data$Nr.Positions, alpha=0.4, nclass=10, scale=30)
