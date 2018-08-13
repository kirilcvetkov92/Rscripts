library(DataExplorer)
create_report(airquality)


library(DataExplorer)
library(ggplot2)
create_report(diamonds, y = "price")


library(DataExplorer)
library(ggplot2)
library(titanic)
create_report(titanic_train, y = "Survived")
