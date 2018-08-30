rm(list = ls())
library(foreach)
library(doParallel)
library(iterators)
library(mlbench)
library(caret)
library(rpart)
library(R.utils)
#library(data.table)
# clean_data from generate sample from distribution
#source("Rscripts\\COST\\generate_distribution.R")

noisy_list <- c(0,10,20,30,40,50)
clean_data <- read.csv("COST\\data_00.csv") 

#clean_data <- read.csv("data_00.csv")
clean_data$X <- NULL
partition <- createDataPartition(clean_data$Service.Model, p = 0.1, list = FALSE)
clean_data <- clean_data[partition,]

feature_names <- names(clean_data)
iter <- nrow(clean_data)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

datalist <- foreach(labelnoise = noisy_list) %dopar% {
  print(labelnoise)
  noisy_data <- clean_data
  resample <- sample.int(iter, iter/100*labelnoise)
  mylabels <- unique(clean_data$Service.Model)
  for(k in resample){
    myset <- noisy_data[k,]
    noisy_data[k,1] <- sample(mylabels[!(myset$Service.Model == mylabels)],1)
  }
  print(head(noisy_data))
  return(noisy_data)
}
# stop cluster and register sequntial front end
stopCluster(cl); registerDoSEQ();

m <- unique(modelLookup()[modelLookup()$forClass,c(1)])
all_model <-getModelInfo()
tags <- lapply(all_model,"[[","tags")
all_model_tags <- lapply(tags, function(x) "Two Class Only" %in% x)
not_two_class_models <- all_model_tags[!unlist(all_model_tags)]
m <- m[m %in% names(not_two_class_models)]
print(m)
m <- c("glmnet","gam","rpart2","C5.0Tree","rf","lda","knn","svmLinear","svmRadial","nb","lvq","Mlda","xgbDARTR")
m <- c("gam","C5.0Tree","rpart2")

# show which libraries were loaded  
sessionInfo()

# register parallel front-end
library(doParallel); 
cl <- makeCluster(detectCores()); 
#cl <- makeCluster(2); 
registerDoParallel(cl)

# this setup actually calls the caret::train function, in order to provide
# minimal error handling this type of construct is needed.
trainCall <- function(i) 
{
  tunelen <- 10
  timeout <- 3600 
  cat("----------------------------------------------------","\n");
  set.seed(123); cat(i," <- loaded\n");
  #tunelen <- 3
  # control <- trainControl(method="boot632",
  #                         allowParallel = FALSE,
  #                         verboseIter = TRUE)
  control <- trainControl(method="repeatedcv",
                         number=10, repeats = 4,
                         #returnResamp = "final",
                         #adaptive = list(min = 5,alpha = .05, method="gls",complete = TRUE),
                         #allowParallel = FALSE,
                         #search = "random",
                         verboseIter = TRUE
                         )
  return(tryCatch(
    t2 <- withTimeout(train(y=Y, x=X, (i), trControl = control, tuneLength = tunelen),timeout = timeout, onTimeout = "silent"),
    error=function(e) NULL))
  #here the time
}

result <- foreach(data = datalist, j=icount() ,.combine = "rbind") %do% {
  # load X and Y (this will be transferred to to train function)
  #data <- datalist
  X = data[,-1]
  Y = data[,1]
  # use lapply/loop to run everything, required for try/catch error function to work
  t2 <- lapply(m, trainCall)
  
  #remove NULL values, we only allow succesful methods, provenance is deleted.
  t2 <- t2[!sapply(t2, is.null)]
  
  # this setup extracts the results with minimal error handling 
  # TrainKappa can be sometimes zero, but Accuracy SD can be still available
  # see Kappa value http://epiville.ccnmtl.columbia.edu/popup/how_to_calculate_kappa.html
  printCall <- function(i) 
  {
    return(tryCatch(
      {
        cat(sprintf("%-22s",(m[i])))
        cat(round(getTrainPerf(t2[[i]])$TrainAccuracy,4),"\t")
        cat(round(getTrainPerf(t2[[i]])$TrainKappa,4),"\t")
        cat(t2[[i]]$times$everything[3],"\n")},
      error=function(e) NULL))
  }
  
  r2 <- lapply(1:length(t2), printCall)
  
  # preallocate data types
  i = 1; MAX = length(t2);
  x0 <- numeric()   # noise
  x1 <- character() # Name
  x2 <- numeric()   # R2
  x3 <- numeric()   # RMSE
  x4 <- numeric()   # time [s]
  x5 <- character() # long model name
  
  # fill data and check indexes and NA with loop/lapply 
  for (i in 1:length(t2)) {
    x0[i] <- noisy_list[j]
    x1[i] <- t2[[i]]$method
    x2[i] <- as.numeric(round(getTrainPerf(t2[[i]])$TrainAccuracy,4))
    x3[i] <- as.numeric(round(getTrainPerf(t2[[i]])$TrainKappa,4))
    x4[i] <- as.numeric(t2[[i]]$times$everything[3])
    x5[i] <- t2[[i]]$modelInfo$label
  }
  
  # coerce to data frame
  df1 <- data.frame(x0,x1,x2,x3,x4,x5, stringsAsFactors=FALSE)
  names(df1) <- c("noise","algorithm","Accuracy","Kappa","Time","Description")
  write.csv(df1,paste("COST/result_",as.character(x0[1]),".csv",sep=""))
  #write.csv(df1,paste("result_",as.character(x0[1]),".csv",sep=""))
  return(df1)
}
# stop cluster and register sequntial front end
stopCluster(cl); registerDoSEQ();
# print all results to R-GUI
print(result)
#write.csv(df1,"result_new.csv")
# plot models, just as example
#ggplot(t2[[1]])
# ggplot(t2[[1]])