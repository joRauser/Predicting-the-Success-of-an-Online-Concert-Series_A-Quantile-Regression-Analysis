##################################
library(quantreg)
library(scoringRules)

### Quantile Regression
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
fit <- rq(formula = viewCount ~ age + concertNumber + concertType, tau = taus, data = trainData)
summary(fit)

### Prediction 
# Method 1: 
quantPred <- predict.rq(object = fit, newdata = testData)
quantPred <- predict.rq(object = fit, newdata = testData, tau = taus)
# Method 2:
# Take bhetas and multiply with values. 
# Add the mean of the squared residuals multiplied with the quantile at normal distribution
# !!! Not finished yet !!! 
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


################################## Plotting :) 
### Plot losses per tau
loss_df <- data.frame(
  tau = as.numeric(names(fiveFold_loss$meanLoss)),
  loss = as.numeric(fiveFold_loss$meanLoss)
)

# Plot: Pinball Loss vs. Quantile
ggplot(loss_df, aes(x = tau, y = loss)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "pink", size = 3) +
  labs(
    title = "Pinball Loss pro Quantil",
    x = "Quantile (τ)",
    y = "Mean Pinball Loss"
  ) +
  theme_minimal(base_size = 14)


### Plot quantileRegression 
plot_quantileRegression <- function(data, formula, fit, taus = NULL, varNum, sortByX = FALSE) {
  # extract variables
  response_var <- all.vars(formula)[1]
  predictor_var <- all.vars(formula)[varNum]
  
  # sort data by x
  if(sortByX == TRUE){
    df_plot <- data %>%
      #select(all_of(c(predictor_var, response_var))) %>%
      arrange(.data[[predictor_var]])
  }else{
    df_plot <- data
    }
  
  # If Taus not given, extract from fit
  if (is.null(taus)) {
    taus <- fit$tau
  }
  
  preds <- predict(fit, newdata = df_plot)
  
  # In case of one Tau: 
  if (is.vector(preds)){
    preds <- matrix(preds, ncol = length(taus))
  } 
  
  # DataFrame for ggplot
  pred_long <- data.frame(
    x = rep(df_plot[[predictor_var]], times = length(taus)),
    y_hat = as.vector(preds),
    tau = factor(rep(taus, each = nrow(df_plot)))
  )
  
  # Plot
  ggplot(df_plot, aes(x = .data[[predictor_var]], y = .data[[response_var]])) +
    geom_point(alpha = 0.6, color = "gray40") +
    geom_line(data = pred_long, aes(x = x, y = y_hat, color = tau), size = 1.2) +
    scale_color_viridis_d(option = "plasma") +
    labs(
      title = "Quantilsregression (auf Originaldaten)",
      x = predictor_var,
      y = response_var,
      color = "Quantil"
    ) +
    theme_minimal(base_size = 14)
}

plot_quantileRegression(data = trainData, formula = viewCount ~ age + concertNumber + concertType, fit = fit, varNum = 2, sortByX = FALSE)

