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

# TODO: Get videoname and uploaddate with "get_video_details" 
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
  filter(str_detect(title, regex("(T|t|)?iny\\s*(D|d)?esk\\s*(C|c)?oncert", ignore_case = TRUE)))%>%
  mutate(concertType <-  if(str_detect(title, regex("(H|h|)?ome", ignore_case = TRUE))){
    "H"}else if(str_detect(title, regex("(C|c|)?ontest", ignore_case = TRUE))| #Contest
                str_detect(title, regex("(F|f|)?amily\\s*(H|h|)?our", ignore_case = TRUE))| #Family hour
                str_detect(title, regex("(F|f|)?rom\\s*(T|t|)?he\\s*(A|a|)?rchiv(e|es|)?", ignore_case = TRUE))| #From the Archives
                str_detect(title, regex("(M|m|)?ee(t|ts|)?", ignore_case = TRUE))| #Meets...
                str_detect(title, regex("(K|k|)?orea", ignore_case = TRUE))| #Korea
                str_detect(title, regex("(J|j|)?apan", ignore_case = TRUE)) #Japan
                               ){"S"}else{"N"})

# ToDo: 
# - Filtern nach Special-Concerts -> String Abfrage und dann in extra Spalte als Merkmal hinterlegen
# - Künstler rausfiltern (Text bis ":")
# - Nach Künstler-Followern suchen


