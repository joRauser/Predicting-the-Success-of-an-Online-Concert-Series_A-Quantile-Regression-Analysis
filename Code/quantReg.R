
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







# pinball loss function
library(scoringRules)

# Estimate quantile Predictions
quantPred <- predict.rq(quantReg, trainData)

# Evaluate
lossF <- qs_quantiles(y = trainData$Nr.views.on.Youtube, x = quantPred, alpha = .5)

summary(lossF)

# Wie weiter auswerten? 