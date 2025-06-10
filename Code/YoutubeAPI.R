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
vidStats <- lapply(vid_ids, get_all_stats)

vidStats_df <- bind_rows(lapply(vidStats, as.data.frame))

library(rio)
# Save Quota
export(vidStats_df, "Rohdaten_YoutubeAPI.csv")
vidStats_df <- import("Rohdaten_YoutubeAPI.csv", format = "csv")

library(stringr)


vidStats_df <- vidStats_df %>%
  rowwise() %>%
  mutate(
    details = list(get_video_details(id)),
    title = details$items[[1]]$snippet$title,
    publishedAt = details$items[[1]]$snippet$publishedAt,
    accessDate = Sys.Date(),
  ) %>%
  ungroup() %>%
  select(-details)
# Save Quota
export(vidStats_df, "RohdatenVollständig_YoutubeAPI.csv")
vidStats_df <- import("RohdatenVollständig_YoutubeAPI.csv", format = "csv")

# From now on usage without Quota
vidStats_df <- vidStats_df %>%
  # For more details about the String-processing visit: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
  filter(str_detect(title, regex("tiny\\s*desk\\s*(\\(home\\)\\s*)?concert", ignore_case = TRUE))) %>%
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
  mutate(concertNumber = ifelse(artist != "Not a normal Concert", row_number(), 0)) %>%
  ungroup()
 

export(vidStats_df, "RohdatenGefiltert_YoutubeAPI.csv")
vidStats_df <- import("RohdatenGefiltert_YoutubeAPI.csv", format = "csv")

# Datentyp-Anpassung und Reinigung
vidStats_df$publishedAt <- as.Date(substr(vidStats_df$publishedAt, 1, 10))
vidStats_df$accessDate <- as.Date(vidStats_df$accessDate)

vidStats_df$viewCount <- as.numeric(vidStats_df$viewCount)
vidStats_df$likeCount <- as.numeric(vidStats_df$likeCount)
vidStats_df$commentCount <- as.numeric(vidStats_df$commentCount)

vidStats_df$concertType <- relevel(as.factor(vidStats_df$concertType), ref = "N")

vidStat_cleaned <- vidStats_df %>%
  mutate(age = accessDate - publishedAt) %>%
  select(-favoriteCount, -id, -accessDate) %>%
  drop_na()
  
vidStat_cleaned$age <- as.numeric(vidStat_cleaned$age)

export(vidStat_cleaned, "Daten_YoutubeAPI.csv")



# ToDo: 
# - Nach Künstler-Followern suchen -> Später überprüfen ob funktioniert (Kontingent verbraucht)
# -> Use yt_search for channels. Search multiple channels (first five or sth. and return the one with max followers)


# - Code auf gesamten Datensatz anwenden
# - Datensatz säubern (zB Datentyp bei den Datums)
# - Anwendung wie bei handData
  
