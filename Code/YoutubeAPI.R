library(tuber)
## Code is partly from https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html

# Yt_Dev_Authorization
yt_oauth(app_id = "433850450109-37tg9e7qpcmfeioesfetc4kgl0d2old1.apps.googleusercontent.com",
         app_secret = "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")
# yt_oauth(app_id = "433850450109-37tg9e7qpcmfeioesfetc4kgl0d2old1.apps.googleusercontent.com",
#         app_secret = "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")



get_stats(video_id = "g5N9AOfgLZA")

# USE THIS TO GET: Video-Name, Upload-Date, 
test <- get_video_details(video_id="g5N9AOfgLZA")




# Channel-ID of NPR Music: UC4eYXhJI4-7wSWc8UNRwD4A

channel <- get_channel_stats(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A")





#### GET ALL VIDEOS STATS

chnlResources <- list_channel_resources(filter = list(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A"), part="contentDetails")

# Uploaded playlists:
playlist_id <- chnlResources$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist (#max_results > 50 == All results)
vids <- get_playlist_items(filter= c(playlist_id=playlist_id), max_results = 51) 

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)

res_df <- bind_rows(lapply(res, as.data.frame))

library(stringr)

# needed to get the subscribers of the artists => ON PAUSE
get_max_subscribers <- function(artist_name) {
  # Fehler abfangen & Ausgabe unterdrücken
  suppressMessages(
    tryCatch({
      channel_ids <- yt_search(artist_name, type = "channel") %>%
        head(5) %>%
        pull(channelId)
      
      if (length(channel_ids) == 0) return(NA_real_)
      
      sub_counts <- numeric(length(channel_ids))
      
      for (i in seq_along(channel_ids)) {
        chnStats <- tryCatch(get_channel_stats(channel_ids[i]), error = function(e) NULL)
        
        sub_counts[i] <- if (!is.null(chnStats$statistics$subscriberCount)) {
          as.numeric(chnStats$statistics$subscriberCount)
        } else {
          NA_real_
        }
      }
      
      max(sub_counts, na.rm = TRUE)
    }, error = function(e) NA_real_)
  )
}



res_df <- res_df %>%
  rowwise() %>%
  mutate(
    details = list(get_video_details(id)),
    title = details$items[[1]]$snippet$title,
    publishedAt = details$items[[1]]$snippet$publishedAt,
    accessDate = Sys.Date(),
  ) %>%
  ungroup() %>%
  select(-details)%>%
  # For more details about the String-processing visit: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
  filter(str_detect(title, regex("(T|t|)?iny\\s*(D|d)?esk\\s*(C|c)?oncert", ignore_case = TRUE)))%>%
  mutate(concertType = case_when(
    str_detect(title, regex("\\bhome", ignore_case = TRUE)) ~ "H", #Home Concerts
    str_detect(title, regex("\\bcontest|\\bfamily\\s*hour\\b|from\\s*the\\s*archiv(e|es|)?|\\bmeet(s)?|korea|japan", ignore_case = TRUE)) ~ "S",
    #Special Concerts (Contests, Family hours, from the Archives, Collaborations(Meets), other Countries(Here Japan and Korea))
    TRUE ~ "N"
  )) %>%
  # Get Artist-name out of title
  mutate(artist = ifelse(concertType=="N", str_trim(str_extract(title, "^[^:]+")), "Not a normal Concert")) %>%
  # Get number of concerts played
  arrange(artist, publishedAt) %>%
  group_by(artist) %>%
  mutate(concertNumber = row_number()) %>%
  ungroup() 
  # Get number of Followers per Artist => On Pause bcs of overload on API
  # mutate(artistFollowers = map_dbl(as.character(artist), get_max_subscribers)) 




# ToDo: 
# - Nach Künstler-Followern suchen -> Später überprüfen ob funktioniert (Kontingent verbraucht)
# -> Use yt_search for channels. Search multiple channels (first five or sth. and return the one with max followers)

# - Code auf gesamten Datensatz anwenden
# - Datensatz säubern (zB Datentyp bei den Datums)
# - Anwendung wie bei handData
  
