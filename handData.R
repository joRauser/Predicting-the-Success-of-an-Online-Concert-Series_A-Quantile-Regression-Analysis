setwd("/Users/jonasrauser/Downloads")
rm(list = ls())
library(tidyverse)


### ! DIESER DATENSATZ WIRD NICHT LÄNGER VERWENDET ! ###

## -> Diese Code-Seite wird nur noch zur Informationsbeschaffung verwendet und kann prinzipiell gelöscht werden

# Load dataset
handData_updated <- read.csv("TinyDesk_handData - Tabellenblatt1.csv", header = T)

# Set/Fix datatypes
handData_updated$Release.date.NPR <- as.Date(handData_updated$Release.date.NPR, format = "%d.%m.%Y")
handData_updated$Access.date <- as.Date(handData_updated$Access.date, format = "%d.%m.%Y")
handData_updated$Nr.views.on.Youtube <- as.numeric(handData_updated$Nr.views.on.Youtube)
handData_updated$Type.of.Concert <- relevel(as.factor(handData_updated$Type.of.Concert), ref = "D")

# Drop unnecessary columns and NA-rows. Create feature "Age" 
handData_work <- handData_updated %>% 
  select(-Release.date.youtube, -Artist.wiki.URL)%>%
  drop_na()%>%
  mutate(age = Access.date - Release.date.NPR)%>%
  select(-Access.date)
# Filter out just the number of days 
handData_work$age <- as.numeric(handData_work$age)

# Divide data into train- and test-data
set.seed(7)
train_dummy <- sample.int(nrow(handData_work), floor(.7*nrow(handData_work)))
trainData <- handData_work[train_dummy,]
testData <- handData_work[-train_dummy,]

# get overview of the data
summary(trainData)

# Descriptives
plot(trainData$Release.date.NPR, trainData$Nr.views.on.Youtube)

plot_Dataframe(trainData)

quantilesNormals <- trainData %>%
  filter(Type.of.Concert == "D") %>%
  pull(Nr.views.on.Youtube)%>%
  quantile(c(.25,.5,.75))  

# Plot Quantiles by Concert-type  
quantile_df <- trainData %>%
  group_by(Type.of.Concert) %>%
  summarise(Q25 = quantile(Nr.views.on.Youtube, 0.25, na.rm = TRUE), 
            Q50 = quantile(Nr.views.on.Youtube, 0.50, na.rm = TRUE),
            Q75 = quantile(Nr.views.on.Youtube, 0.75, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Q"), names_to = "quantile", values_to = "value")  

ggplot(quantile_df, aes(x = quantile, y = value, fill = Type.of.Concert)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Youtube Views: Quantile per concert type")


# Fit linear Regression via. OLS
# Type of Concert as dummyvar! 
linReg <- lm(data = trainData, formula = Nr.views.on.Youtube ~ Is.second.concert..or.more. + age + Type.of.Concert)
summary(linReg)
# Throw linReg without log in the garbage :D

loglinReg <- lm(data = trainData, formula = log(Nr.views.on.Youtube) ~ Is.second.concert..or.more. + age + Type.of.Concert)
summary(loglinReg)
# loglinReg seems good for the values of the data
# Intercept and Type of Concert seem significant

# Quantile Estimation
# Gamma-Estimation
gamma <- mean(loglinReg$residuals)

# Quantile-Estimation => NEED TO LOOK AT THIS AGAIN! 
quant <- function(datapoint, bhetas, gamma, p){
  quantile <- datapoint %*% bhetas + qnorm(p)*gamma
}


# Quantile Regression: 
library(quantreg)

quantReg <- rq(data = trainData, formula = Nr.views.on.Youtube ~ Is.second.concert..or.more. + age + Type.of.Concert)
# -> Use Observation weights? 
# -> Use other method than "br"
summary(quantReg)

plot(trainData$age,trainData$Nr.views.on.Youtube,xlab="Video age",ylab="Klicks on Youtube",type = "n", cex=.5) 
points(trainData$age,trainData$Nr.views.on.Youtube,cex=.5,col="blue")

taus <- c(.05,.1,.25,.75,.9,.95)
xx <- seq(min(trainData$age),max(trainData$age),100)
f <- coef(rq((trainData$Nr.views.on.Youtube)~(trainData$age),tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
  lines(xx,yy[,i],col = "gray")
}
abline(lm(trainData$Nr.views.on.Youtube ~ trainData$age),col="red",lty = 2)
abline(rq(trainData$Nr.views.on.Youtube ~ trainData$age), col="blue")
legend(3000,500,c("mean (LSE) fit", "median (LAE) fit"),
       col = c("red","blue"),lty = c(2,1))

# Logische Annahme mehr oder weniger bestätigt: Mit dem Alter eines Videos nimmt die Klickzahl mit kleiner Tendenz zu! 


# GGPLOT-Code (viel schöner)
taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

p <- ggplot(trainData, aes(x = age, y = viewCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
  labs(title = "Quantile Regression", x = "Age", y = "Youtube Views")
# Quantile-Regressionslinien hinzufügen
for (tau in taus) {
  p <- p + geom_quantile(quantiles = tau, color = "gray")
}

# Median (tau = 0.5) hervorheben
p <- p + geom_quantile(quantiles = 0.5, color = "blue")
print(p)


# pinball loss function
library(scoringRules)

# Estimate quantile Predictions
quantPred <- predict.rq(quantReg, trainData)

# Evaluate
lossF <- qs_quantiles(y = trainData$Nr.views.on.Youtube, x = quantPred, alpha = .5)

summary(lossF)

# Wie weiter auswerten? 