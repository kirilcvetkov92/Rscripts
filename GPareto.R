library(GPareto)

design.init <- matrix(seq(0, 1, length.out = 6), ncol = 1)
response.init <- MOP2(design.init)
mf1 <- km(~1, design = design.init, response = response.init[, 1])
mf2 <- km(~1, design = design.init, response = response.init[, 2])
model <- list(mf1, mf2)

res <- GParetoptim(model = model, fn = MOP2, crit = "EHI", nsteps = 7,
                   lower = 0, upper = 1, critcontrol = list(refPoint = c(2, 2)))


plotParetoGrid(P1)
myobj <- function (x) 
{
  if (is.null(dim(x))) {
    x <- matrix(x, nrow = 1)
  }
  #n <- ncol(x)
  #g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
  #return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
  return(cbind(x^2,x[, 1]))
  
  
  # f1 <- x1, f2 <- g(x)*(1-sqrt(x1/g(x)))
}

res <- easyGParetoptim(fn = myobj, budget = 50, lower = rep(0, 4),upper = rep(1, 4))

library(caret)
data("iris")
control <- trainControl(method="cv", number = 5, classProbs = TRUE, summaryFunction = mnLogLoss)
set.seed(7)
fit <- train(Species~. ,data = iris, method = "rf", metric="logLoss", trControl = control)

print(fit)


