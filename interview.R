rm(list = ls())
data <- read.csv("credit_data_track2_part_A.csv")

library(DataExplorer)
library(ggplot2)
create_report(data, y = "accepted")


data$personal_status

data$gender <- "male"
data$gender <- ifelse(data$personal_status == "female_divorced/separated/married","female","male")

table(data$accepted,data$gender)
data[is.na(data$age),c("age")] <- mean(data$age,na.rm=TRUE)

clean_data <- data[data$credit_amount < 1000000,]
table(clean_data$own_telephone,clean_data$accepted)

mydata <- clean_data[,c("own_telephone","age","gender","accepted")]

library(rpart)

mymodel_rpart <- rpart(accepted ~ ., data = mydata)
mymodel_lin <- glm(accepted ~., data = mydata, family = binomial(link = "logit"))

#-------------------
dataset <- na.omit(mydata) 
dataset$accepted <- as.factor(dataset$accepted)
idx <- createDataPartition(dataset$accepted, p = 0.8, list=FALSE)
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
#algos <- list("glm","nb","svmLinear","rpart","rf","knn")
algos <- list("glm","nb","rpart")
i<-1
for (algo in algos) {
  print(algo)
  model_default <- train(accepted ~ . ,
                         data= train,
                         method=algo, 
                         metric=metric,
                         trControl=control,
                         tuneLength = 10
  )
  pred <- predict(model_default, newdata = test)
  modelvalues <- data.frame(obs = test$accepted, pred=pred)
  mod_summary <- defaultSummary(modelvalues)
  modellist[i,1] <- algo
  modellist[i,2:3] <- mod_summary
  i<-i+1
}
modellist
