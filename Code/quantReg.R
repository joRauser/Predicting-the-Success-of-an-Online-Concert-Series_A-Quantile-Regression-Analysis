#### Code nur für Visuals!  ->  Nicht für die eigentliche regression verwenden! 
printQuantreg <- function(data, Y, X, yTitle, xTitle){
  taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
  
  p <- ggplot(data, aes(x = X, y = Y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
    labs(title = "Quantile Regression", x = xTitle, y = yTitle)
  # Quantile-Regressionslinien hinzufügen
  for (tau in taus) {
    p <- p + geom_quantile(quantiles = tau, color = "gray")
  }
  
  # Median (tau = 0.5) hervorheben
  p <- p + geom_quantile(quantiles = 0.5, color = "blue")
  print(p)
}
##########
# pinball loss function
library(scoringRules)

# Estimate quantile Predictions
# quantPred <- predict.rq(quantReg, trainData)

# Evaluate
lossF <- qs_quantiles(y = trainData$Nr.views.on.Youtube, x = quantPred, alpha = .5)

summary(lossF)

##################################
library(quantreg)

### Quantile Regression
taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
fit <- rq(formula = viewCount ~ age + concertNumber + concertType, tau = taus, data = trainData)
summary(fit)

### Prediction 
# Method 1: 
quantPred <- predict.rq(object = fit, newdata = testData)
quantPred <- predict.rq(object = fit, newdata = testData, tau = taus)
# Method 2:
# Take bhetas and multiply with values. Add the mean of the squared residuals multiplied with the quantile at normal distribution
y_hat <- X%*%fit$coefficients + fit$residuals %*% qnorm(p) # p = quantile of the normal distr. 
# X = matrix of data corresponding to the bhetas (with intercept)
# => Need to create that X! 

### 
qr_quantiles <- qs_sample(y = testData$viewCount, dat = quantPred, alpha = .5)


### All together with 5-hold cross validation

# pinball-loss function:
pinball_loss <- function(y, q_hat, tau) {
  u <- y - q_hat
  mean((tau - (u < 0)) * u)
}

k_fold_cv <- function(data, formula, taus = c(.25,.5,.75), k = 5) {
  n <- nrow(data)
  set.seed(7)
  # split data into 5 distinct sets (folds)
  folds <- sample(rep(1:k, length.out = n))  
  
  # extract response variable
  responseVar <- all.vars(formula)[1]
  
  lossMatrix <- matrix(NA, nrow = k, ncol = length(taus))
  colnames(lossMatrix) <- taus
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data  <- data[folds == i, ]
    
    # fit model
    fit <- rq(formula, tau = taus, data = train_data)
    
    # prediction
    preds <- predict(fit, newdata = test_data)
    
    # in case of only 1 tau
    if(is.vector(preds)){
      preds <- matrix(preds, ncol = 1)
    }
    
    # Pinball-Loss
    for (j in seq_along(taus)) {
      lossMatrix[i, j] <- pinball_loss(test_data[[responseVar]], preds[, j], taus[j])
    }
  }
  lossMeans <- colMeans(lossMatrix)
  
  return(list(meanLoss = lossMeans, all_losses = lossMatrix))
}

fiveFold_loss <- k_fold_cv(vidStat_cleaned, viewCount ~ age + concertNumber + concertType, taus = taus)

lossesPerFold <- fiveFold_loss$all_losses
