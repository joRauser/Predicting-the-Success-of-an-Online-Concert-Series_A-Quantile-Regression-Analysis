# Divide data into train- and test-data
# -> Delete (its fivehold crossvalidation now)
set.seed(7)
train_dummy <- sample.int(nrow(vidStat_cleaned), floor(.7*nrow(vidStat_cleaned)))
trainData <- vidStat_cleaned[train_dummy,]
testData <- vidStat_cleaned[-train_dummy,]


# GET OVERVIEW
summary(trainData)

# Descriptives
plot(vidStat_cleaned$publishedAt, vidStat_cleaned$viewCount)

qqplot(vidStat_cleaned$viewCount, vidStat_cleaned$likeCount)
qqplot(vidStat_cleaned$viewCount, vidStat_cleaned$commentCount)
qqplot(vidStat_cleaned$likeCount, vidStat_cleaned$commentCount)


quantilesNormals <- vidStat_cleaned %>%
  filter(concertType == "N") %>%
  pull(viewCount)%>%
  quantile(c(.25,.5,.75))  

# Plot Quantiles by Concert-type  
quantile_df <- vidStat_cleaned %>%
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
linReg_clicks <- lm(data = trainData, formula = viewCount ~ age + concertType)
summary(linReg_clicks)

linReg_likes <- lm(data = trainData, formula = likeCount ~ age + concertType)
summary(linReg_likes)

linReg_comments <- lm(data = trainData, formula = commentCount ~ age + concertType)
summary(linReg_comments)


# Loglinear Regression
loglinReg_clicks <- lm(data = trainData, formula = log(viewCount) ~ age + concertType)
summary(loglinReg_clicks)

loglinReg_likes <- lm(data = trainData, formula = log(likeCount) ~ age + concertType)
summary(loglinReg_likes)


logData <- trainData %>%
  mutate(commentCount = ifelse(commentCount == 0, 1, commentCount))
loglinReg_comments <- lm(data = logData, formula = log(commentCount) ~ age + concertType)
summary(loglinReg_comments)

