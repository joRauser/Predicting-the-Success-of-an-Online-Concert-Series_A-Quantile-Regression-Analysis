## Here are some log-linear models to analyze the influence of seasons on clickCount

### YEARLY Effects
dummyYear <- trainData %>%
  mutate(year = as.factor(format(as.Date(trainData$publishedAt, format="%Y-%m-%d"),"%Y"))) %>%
  select(-publishedAt, -title)

# Baseline = 2009
linYrReg09 <- lm(data = dummyYear, formula = log(viewCount) ~ year)
summary(linYrReg09)

# Baseline = 2015 
linYrReg15 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2015")))
summary(linYrReg15)

# Baseline = 2020
linYrReg20 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2020")))
summary(linYrReg20)
# -> Von wann bis wann Homevideos -> Grund fpr rpckgang 2020

# Baseline = 2023
linYrReg23 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2023")))
summary(linYrReg23)

# Ergebnisse:
# In den frühen Jahren signifikant weniger Clicks generiert
# In den Jahren 2023 und 2024 am meisten Clicks
# Coronazeit scheint sich positiv auf die Clicks ausgewirkt zu haben (2020 bspw. signifikant negativ)

# In BA beschreiben, was die versch. Effekte sind + dass dies (Datenlage) nicht vergleichbar sind -> Warum + Was dafür nötig

### MONTHLY Effects

dummyMonth <- trainData %>%
  mutate(month = as.factor(format(as.Date(trainData$publishedAt, format="%Y-%m-%d"),"%m"))) %>%
  select(-publishedAt, -concertNumber, -title)

# Month. Baseline = January
linMonRegJan <- lm(data = dummyMonth, formula = log(viewCount) ~ month)
summary(linMonRegJan)

# Month. Baseline = August
linMonRegAug <- lm(log(viewCount) ~ month_fact, data = transform(dummyMonth, month_fact = relevel(as.factor(month), ref = "08")))
summary(linMonRegAug)



### SEASONAL Effects

dummySeason <- trainData %>%
  mutate(month = as.integer(format(as.Date(trainData$publishedAt, format="%Y-%m-%d"),"%m"))) %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2)  ~ "winter",
    month %in% c(3, 4, 5)   ~ "spring",
    month %in% c(6, 7, 8)   ~ "summer",
    month %in% c(9, 10, 11) ~ "autumn"
  )) %>%
  select(-publishedAt, -concertNumber, -title)


# Seasonal Baseline = Autumn
linSeaRegAut <- lm(data = dummySeason, formula = log(viewCount) ~ season)
summary(linSeaRegAut)
# Im Herbst mehr Clicks als im Frühling

# Seasonal Baseline = Winter 
linSeaRegWin <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "winter")))
summary(linSeaRegWin)
# Wenig erkennbar

# Seasonal Baseline = Spring
linSeaRegSpr <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "spring")))
summary(linSeaRegSpr)
# Signifikant weniger views als im Sommer oder Herbst

# Seasonal Baseline = Summer
linSeaRegSum <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "summer")))
summary(linSeaRegSum)
# Signifikant mehr views als im Frühling

# Zusammengefasst: Im Vergleich zum Winter wenig Aussagen treffbar, im Frühling eher weniger Clicks, im Sommer und Herbst am Meisten Clicks.



#### PASTE RELEVANT STATS OF MULTIPLE REGRESSIONS IN ONE DATAFRAME
library(broom)
# List of Models:
models <- list(
  "Autumn as Baseline" = lm(log(viewCount) ~ season, data = vidStats),
  "Winter as Baseline" = lm(log(viewCount) ~ season_fact,
                            data = transform(vidStats, season_fact = relevel(as.factor(season), ref = "winter"))),
  "Spring as Baseline" = lm(log(viewCount) ~ season_fact,
                            data = transform(vidStats, season_fact = relevel(as.factor(season), ref = "spring"))),
  "Summer as Baseline" = lm(log(viewCount) ~ season_fact,
                            data = transform(vidStats, season_fact = relevel(as.factor(season), ref = "summer")))
)

# extract results
seasonModels <- lapply(names(models), function(model_name) {
  tidy(models[[model_name]]) %>%
    mutate(Model = model_name) # Modellname hinzufügen
})

seasonModels_df <- do.call(rbind, seasonModels)
# optional: 
seasonModels_df <- seasonModels_df[, c("Model", "term", "estimate", "std.error", "statistic", "p.value")]

# Ausgabe
print(seasonModels_df)



### Bigger Regressions :) 
linSeasonal_all <- lm(log(viewCount) ~ season_fact + age + concertType, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "spring")))
summary(linSeasonal_all)


linYrReg09_all <- lm(data = dummyYear, formula = log(viewCount) ~ year + concertType + concertNumber)
summary(linYrReg09_all)

linYrReg20_all <- lm(log(viewCount) ~ year_fact + concertType + concertNumber, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2020")))
summary(linYrReg20_all)

