library(dplyr)
library(tuber)
library(rio)
library(stringr)
library(lubridate)
# Note: Code is partly from https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html

# Yt_Dev_Authorization
yt_oauth()

yt_oauth()

##############################
#### GET ALL VIDEOS STATS
##############################
# Channel-ID of NPR Music: "UC4eYXhJI4-7wSWc8UNRwD4A". Get channelinfo with: get_channel_stats(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A")

chnlResources <- list_channel_resources(filter = list(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A"), part="contentDetails")

# get ID of the Upload-playlist (this playlist contains every video uploaded on a youtube-channel):
playlist_id <- chnlResources$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos of the playlist 
vids <- get_playlist_items(filter= c(playlist_id=playlist_id), max_results = 51) #(max_results > 50 == All results)
# Get Video-IDs
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 
# Get stats and convert results to data frame 
vidStats <- lapply(vid_ids, get_all_stats)
vidStats_df <- bind_rows(lapply(vidStats, as.data.frame))

# Save dataset to conserve Quota
export(vidStats_df, "Rohdaten_YoutubeAPI.csv")
vidStats_df <- import("Rohdaten_YoutubeAPI.csv", format = "csv")


# Add features to the dataset 
vidStats_df <- vidStats_df %>%
  rowwise() %>%
  mutate(
    details = list(get_video_details(id)),
    title = details$items[[1]]$snippet$title,
    publishedAt = details$items[[1]]$snippet$publishedAt,
    accessDate = Sys.Date(),
  ) %>%
  ungroup() %>%
  select(-details) %>%
  # Added from "vidDuration.R"
  rowwise() %>%
  mutate(
    details = list(get_video_details(id, part = "contentDetails")),
    duration_iso = details$items[[1]]$contentDetails$duration,
    durationSeconds = lubridate::duration(duration_iso) %>% as.numeric(),
    durationMinutes = round(durationSeconds / 60, 2),
    caption = details$items[[1]]$contentDetails$caption
  ) %>%
  ungroup() %>%
  select(-details, -duration_iso)

# Save dataset to conserve Quota
export(vidStats_df, "RohdatenVollständig_YoutubeAPI.csv")
vidStats_df <- import("RohdatenVollständig_YoutubeAPI.csv", format = "csv")


# This part doesn't consume any quota since its only based on string-analysis of the video-title
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
#OLD:  mutate(artist = ifelse(concertType=="N", str_trim(str_extract(title, "^[^:]+")), "Not a normal Concert")) %>%
  mutate(artist = ifelse(str_detect(vidStats_df$title, ":"), str_trim(str_extract(vidStats_df$title, "^[^:]+")), "Artist not Found!")) %>%
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

vidStats_df$caption <- as.factor(vidStats_df$caption)

vidStats_df$viewCount <- as.numeric(vidStats_df$viewCount)
vidStats_df$likeCount <- as.numeric(vidStats_df$likeCount)
vidStats_df$commentCount <- as.numeric(vidStats_df$commentCount)

vidStats_df$concertType <- relevel(as.factor(vidStats_df$concertType), ref = "N")

vidStats_df <- vidStats_df %>%
  mutate(age = accessDate - publishedAt) %>%
  select(-favoriteCount, -accessDate) %>%
  drop_na()
  
vidStats_df$age <- as.numeric(vidStats_df$age)

# Save dataset
export(vidStats_df, "Daten_YoutubeAPI.csv")
vidStats_df <- import("Daten_YoutubeAPI.csv", format = "csv")



#### With Artist Followers and Season
vidStats <- inner_join(vidStats_df, follower_df%>%select(id, artistFollower), by = "id") %>%
  mutate(month = as.integer(format(as.Date(vidStats_df$publishedAt, format="%Y-%m-%d"),"%m"))) %>%
  mutate(year = as.factor(format(as.Date(vidStats_df$publishedAt, format="%Y-%m-%d"),"%Y"))) %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2)  ~ "winter",
    month %in% c(3, 4, 5)   ~ "spring",
    month %in% c(6, 7, 8)   ~ "summer",
    month %in% c(9, 10, 11) ~ "autumn"
  )) %>%
  select(-publishedAt, -title)

export(vidStats, "Youtube VideoData.csv")
