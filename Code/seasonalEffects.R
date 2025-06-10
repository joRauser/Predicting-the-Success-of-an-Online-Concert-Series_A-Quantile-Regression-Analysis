### YEARLY EFFECTS
dummyYear <- vidStat_cleaned %>%
  mutate(year = as.factor(format(as.Date(vidStat_cleaned$publishedAt, format="%Y-%m-%d"),"%Y"))) %>%
  select(-publishedAt, -concertNumber, -title)

# Baseline = 2009
linYrReg09 <- lm(data = dummyYear, formula = log(viewCount) ~ year)
summary(linYrReg09)



# Baseline = 2015 
linYrReg15 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2015")))
summary(linYrReg15)

# Baseline = 2020
linYrReg20 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2020")))
summary(linYrReg20)

# Baseline = 2023
linYrReg23 <- lm(log(viewCount) ~ year_fact, data = transform(dummyYear, year_fact = relevel(as.factor(year), ref = "2023")))
summary(linYrReg23)

# Ergebnisse:
# In den frühen Jahren signifikant weniger Clicks generiert
# In den Jahren 2023 und 2024 am meisten Clicks
# Coronazeit scheint sich sogar eher negativ auf die Clicks ausgewirkt zu haben (2020 bspw. signifikatn negativ)


### Monthly Effects

dummyMonth <- vidStat_cleaned %>%
  mutate(month = as.factor(format(as.Date(vidStat_cleaned$publishedAt, format="%Y-%m-%d"),"%m"))) %>%
  select(-publishedAt, -concertNumber, -title)

# Month. Baseline = January
linMonRegJan <- lm(data = dummyMonth, formula = log(viewCount) ~ month)
summary(linMonRegJan)

# Month. Baseline = August
linMonRegAug <- lm(log(viewCount) ~ month_fact, data = transform(dummyMonth, month_fact = relevel(as.factor(month), ref = "08")))
summary(linMonRegAug)


### SEASONAL EFFECTS

dummySeason <- vidStat_cleaned %>%
  mutate(month = as.integer(format(as.Date(vidStat_cleaned$publishedAt, format="%Y-%m-%d"),"%m"))) %>%
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

# Seasonal Baseline = Winter
linSeaRegWin <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "winter")))
summary(linSeaRegWin)

# Seasonal Baseline = Spring
linSeaRegSpr <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "spring")))
summary(linSeaRegSpr)

# Seasonal Baseline = Summer
linSeaRegSum <- lm(log(viewCount) ~ season_fact, data = transform(dummySeason, season_fact = relevel(as.factor(season), ref = "summer")))
summary(linSeaRegSum)
