library(purrr)
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Made for single Tau's only:
r2_kfold_cv <- function(data, formula, tau = .5, k = 5) {
  n <- nrow(data)
  set.seed(7)
  varCount <- length(all.vars(formula)) - 1
  # split data into 5 distinct sets (folds)
  folds <- sample(rep(1:k, length.out = n))  
  
  # extract response variable
  responseVar <- all.vars(formula)[1]
  
  scoreMatrix <- matrix(NA, nrow = k+1, ncol = 4, dimnames = list(c(1:k, "mean"), c("QuantileScores", "QS-Nullmod", "R2", "adjR2")))
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data  <- data[folds == i, ]
    
    # fit model
    fit <- rq(formula, tau = tau, data = train_data)
    # nullmodel: 
    fitNull <- rq(as.formula(paste(responseVar, "~ 1")), tau = tau, data = train_data)
    
    # prediction
    pred <- predict(fit, newdata = test_data)
    predNull <- predict(fitNull, newdata = test_data)
    
    # (Here would be "If pred is Vector" - thingi)
    
    qs <- qs_quantiles(y = test_data[[responseVar]], x = pred, alpha = tau) %>% 
      mean()
    qsNull <- qs_quantiles(y = test_data[[responseVar]], x = predNull, alpha = tau) %>% 
      mean()
    
    scoreMatrix[i,1] <- as.numeric(qs)
    scoreMatrix[i,2] <- as.numeric(qsNull)
    scoreMatrix[i,3] <- 1 - qs/qsNull # R-Squared
    scoreMatrix[i,4] <- 1-((n-1)/(n-varCount))*(1-scoreMatrix[i,3]) # adjusted R-Squared
  }
  for(j in 1:4){
    scoreMatrix[k+1,j] <- mean(scoreMatrix[1:k,j])
  }
  return(scoreMatrix)
}

# Model 1 -> Age of the Video
r2_m1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 2 -> + ConcertType
r2_m2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 3 -> + conertNumber
r2_m3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + concertNumber, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 4 -> + durationMinutes
r2_m4 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + concertNumber + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 5 -> -concertNumber, 
r2_m5 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 6 -> + Seasonalities
# Years
r2_m6.1 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + year, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Months
r2_m6.2 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Seasons
r2_m6.3 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + season, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Months and Year (no Season, bcs its already included in months. Therefore it doesnt give any additional information)
r2_m6 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + year + month, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})
# Model 7 -> + caption
r2_m7 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + year + month + caption, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})

# Model 8 -> + artistFollower
r2_m8 <- map_dfr(taus, ~{
  res <- r2_kfold_cv(data = vidStats, formula = viewCount ~ age + concertType + durationMinutes + year + month + caption + artistFollower, tau = .x)
  tibble(tau = .x, R2_mean = res["mean", "R2"], adjR2_mean = res["mean", "adjR2"])
})




r2_allModels <- inner_join(r2_m1%>%select(-adjR2_mean), r2_m2%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m3%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m4%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m5%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m6.1%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m6.2%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m6%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m7%>%select(-adjR2_mean), by = "tau")%>%
  inner_join(.,r2_m8%>%select(-adjR2_mean), by = "tau")

colnames(r2_allModels) <- c("tau", "+Age", "+cT", "cN", "+d", "-cN", "+y", "+m", "+S", "+c", "+aF")
