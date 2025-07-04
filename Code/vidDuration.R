library(tuber)
library(dplyr)
library(purrr)
library(lubridate)

# Es muss eine Spalte `video_id` geben
# vidStats_df <- data.frame(video_id = c("dQw4w9WgXcQ", "Ks-_Mh1QhMc", ...))

# Funktion zum Abrufen und Konvertieren der Videolänge
get_duration <- function(id) {
  tryCatch({
    details <- get_video_details(video_id = id)
    iso <- details$contentDetails$duration
    seconds <- duration(iso) %>% as.numeric()
    return(seconds)
  }, error = function(e) NA_real_)
}

# Füge die Dauer in Sekunden und Minuten dem DataFrame hinzu
vidStats_df_time <- vidStats_df_time %>%
  mutate(
    duration_seconds = map_dbl(video_id, get_duration),
    duration_minutes = round(duration_seconds / 60, 2)
  )


