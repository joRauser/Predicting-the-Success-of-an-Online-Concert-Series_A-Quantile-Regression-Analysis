library(tuber)
library(dplyr)
library(lubridate)


# Code wurde in "YoutubeAPI.R" überführt und wird dort simultan mit dem Rest ausgeführt
# ! vidStats_duration wird langfristig entfernt -> Beachten! 
vidStats_duration <- vidStats_df %>%
  rowwise() %>%
  mutate(
    details = list(get_video_details(id, part = "contentDetails")),
    duration_iso = details$items[[1]]$contentDetails$duration,
    durationSeconds = lubridate::duration(duration_iso) %>% as.numeric(),
    durationMinutes = round(durationSeconds / 60, 2),
    dimension = details$items[[1]]$contentDetails$dimension,
    caption = details$items[[1]]$contentDetails$caption
  ) %>%
  ungroup() %>%
  select(-details, -duration_iso)

vidStats_duration$dimension <- as.factor(vidStats_duration$dimension)
vidStats_duration$caption <- as.factor(vidStats_duration$caption)

# ToDo: 
# - Features in die Dokumentation aufnehmen