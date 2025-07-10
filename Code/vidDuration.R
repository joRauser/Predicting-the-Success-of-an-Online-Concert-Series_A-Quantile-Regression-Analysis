library(tuber)
library(dplyr)
library(purrr)
library(lubridate)

# Es muss eine Spalte `video_id` geben
# vidStats_df <- data.frame(video_id = c("dQw4w9WgXcQ", "Ks-_Mh1QhMc", ...))

# Funktion zum Abrufen und Konvertieren der Videolänge
get_duration <- function(id) {
  tryCatch({
    details <- get_video_details(video_id = id, part = "contentDetails")
    iso <- details$contentDetails$duration
    seconds <- duration(iso) %>% as.numeric()
    return(seconds)
  }, error = function(e) NA_real_)
}

# Füge die Dauer in Sekunden und Minuten dem DataFrame hinzu
vidStat_test <- vidStat_test %>%
  mutate(
    duration_seconds = map_dbl(id, get_duration),
    duration_minutes = round(duration_seconds / 60, 2)
  )

vidStat_test <- head(vidStats_df_time)



### Versuch 2

get_video_metadata <- function(id) {
  tryCatch({
    details <- get_video_details(video_id = id, part = "contentDetails")
    duration_iso <- details$items[[1]]$contentDetails$duration
    duration_sec <- lubridate::duration(duration_iso) %>% as.numeric()
    dimension <- details$items[[1]]$contentDetails$dimension
    caption <- details$items[[1]]$contentDetails$caption
    
    tibble(
      id = id,
      duration_seconds = duration_sec,
      duration_minutes = round(duration_sec / 60, 2),
      dimension = dimension,
      caption = caption
    )
  }, error = function(e) {
    tibble(
      id = id,
      duration_seconds = NA_real_,
      duration_minutes = NA_real_,
      dimension = NA,
      caption = NA
    )
  })
}

# Hole alle Metadaten
vid_metadata <- map_dfr(vidStat_test$id, get_video_metadata)

# Merge mit dem ursprünglichen DataFrame
vidStat_test <- left_join(vidStat_test, vid_metadata, by = "id")


# Klappt: (Rest oberhalb kann eigentlich weg)
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