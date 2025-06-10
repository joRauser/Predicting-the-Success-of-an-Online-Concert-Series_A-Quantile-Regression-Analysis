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


# When the concertNumber should be examined, the concert which are not analysable regarding the ConcertCount have to be filtered out:
trainData_coNum <- trainData %>%
  filter(concertNumber != 0) %>%
  mutate(isSecCon = ifelse(concertNumber == 1, FALSE, TRUE))

secClicks <- lm(data = trainData_coNum, formula = viewCount ~ isSecCon + age)
summary(secClicks)

logsecClicks <- lm(data = trainData_coNum, formula = log(viewCount) ~ isSecCon + age)
summary(logsecClicks)

# Coronaeinfluss?? -> Jahreszeiträume aufteilen und untersuchen ODER veröffentlichungsjahr als Dummy-Variable => Effekte wie zB Videos aus 2020 sehr oft geklickt
# Saisonalität untersuchen -> Monate untersuchen







