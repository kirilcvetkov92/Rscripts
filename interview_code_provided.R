library(readr)
data <- read_csv("credit_data_track2_part_B.csv")

model_vars <- c("age", "duration", "credit_amount", "installment_commitment", "own_telephone")
samp_size <- 0.6
result_list <- c()
for (i in 1:100){
  sample_index <- sample(x = nrow(data), size = nrow(data) * samp_size, replace = TRUE)
  logistic_model <- glm(accepted ~., data=data[sample_index,c(model_vars, "accepted")], family=binomial(link="logit"))
  result_list <- c(result_list, exp(logistic_model$coefficients["own_telephoneyes"]))
}
mean(result_list)
sd(result_list)
hist(result_list)
