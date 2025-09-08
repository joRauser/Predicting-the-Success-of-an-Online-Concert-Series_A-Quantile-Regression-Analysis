rm(list = ls())
library(quantreg)
library(scoringRules)

# simulate data from linear model (single regressor)
n <- 1e3 # sample size
x <- rnorm(n) # regressor
e <- rnorm(n) # error term
b0 <- 1 # intercept
b1 <- 2 # slope coefficient
y <- b0 + b1*x + e # outcome variable
 
# train test split
train_ind <- 1:(.5*n) # indices of training data
test_ind <- (.5*n+1):n # indices of test data
df_train <- data.frame(y = y[train_ind], x = x[train_ind])
df_test <- data.frame(y = y[test_ind], x = x[test_ind])

# fit quantile regression model in-sample
tau <- .25 # quantile level
fit <- rq(y~x, data = df_train, tau = tau) # model using regressor x
fit_c <- rq(y~1, data = df_train, tau = tau) # model using a constant only

# compute out-of-sample predictions
pred <- predict(fit, newdata = df_test) |> 
  unname() |> as.numeric() # linear model
pred_c <- predict(fit_c, newdata = df_test) |> 
  unname() |> as.numeric() # constant model

# compute out-of-sample R2
qs <- qs_quantiles(y = df_test$y, x = pred, alpha = tau) |> 
  mean()
qs_c <- qs_quantiles(y = df_test$y, x = pred_c, alpha = tau) |> 
  mean()
r2_oos <- 1 - qs/qs_c # out of sample R2
r2_oos |> round(3) 
