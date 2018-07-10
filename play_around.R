## Load Titanic library to get the dataset
library(titanic)

## Load the datasets
data("titanic_train")
data("titanic_test")

library(DataExplorer)

create_report(titanic_train) 