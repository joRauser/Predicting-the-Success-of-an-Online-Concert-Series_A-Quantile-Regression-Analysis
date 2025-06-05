# Divide data into train- and test-data
set.seed(7)
train_dummy <- sample.int(nrow(vidStat_cleaned), floor(.7*nrow(vidStat_cleaned)))
trainData <- vidStat_cleaned[train_dummy,]
testData <- vidStat_cleaned[-train_dummy,]

# GET OVERVIEW
summary(trainData)

# Descriptives
plot(trainData$publishedAt, trainData$viewCount)

plot_Dataframe(trainData)

qqplot(trainData$viewCount, trainData$likeCount)
qqplot(trainData$viewCount, trainData$commentCount)
qqplot(trainData$likeCount, trainData$commentCount)


quantilesNormals <- trainData %>%
  filter(concertType == "N") %>%
  pull(viewCount)%>%
  quantile(c(.25,.5,.75))  

# Plot Quantiles by Concert-type  
quantile_df <- trainData %>%
  group_by(concertType) %>%
  summarise(Q25 = quantile(viewCount, 0.25, na.rm = TRUE), 
            Q50 = quantile(viewCount, 0.50, na.rm = TRUE),
            Q75 = quantile(viewCount, 0.75, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Q"), names_to = "quantile", values_to = "value")  

ggplot(quantile_df, aes(x = quantile, y = value, fill = concertType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Youtube Views: Quantile per concert type")


# REGRESSION

# Linear Regression
linReg_clicks <- lm(data = trainData, formula = viewCount ~ concertNumber + age + concertType)
summary(linReg_clicks)

linReg_likes <- lm(data = trainData, formula = likeCount ~ concertNumber + age + concertType)
summary(linReg_likes)

linReg_comments <- lm(data = trainData, formula = commentCount ~ concertNumber + age + concertType)
summary(linReg_comments)


# Loglinear Regression
loglinReg_clicks <- lm(data = trainData, formula = log(viewCount) ~ concertNumber + age + concertType)
summary(loglinReg_clicks)

loglinReg_likes <- lm(data = trainData, formula = log(likeCount) ~ concertNumber + age + concertType)
summary(loglinReg_likes)


logData <- trainData %>%
  mutate(commentCount = ifelse(commentCount == 0, 1, commentCount))
loglinReg_comments <- lm(data = logData, formula = log(commentCount) ~ concertNumber + age + concertType)
summary(loglinReg_comments)

