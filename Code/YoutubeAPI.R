library(tuber)

yt_oauth(app_id = "433850450109-37tg9e7qpcmfeioesfetc4kgl0d2old1.apps.googleusercontent.com",
         app_secret = "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")
# yt_oauth(app_id = "433850450109-37tg9e7qpcmfeioesfetc4kgl0d2old1.apps.googleusercontent.com",
#         app_secret = "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")



get_stats(video_id = "g5N9AOfgLZA")
test <- get_video_details(video_id="g5N9AOfgLZA")




# Channel-ID of NPR Music: UC4eYXhJI4-7wSWc8UNRwD4A

channel <- get_channel_stats(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A")

# This code only gets the video data of 50 videos and the name of the videos are missing as well

chnResources <- list_channel_resources(filter = list(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A"), part="contentDetails")
## Code is from https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
# Uploaded playlists:
playlist_id <- chnResources$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id)) 

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
res_df <- do.call(rbind, lapply(res, data.frame))



#### GET ALL VIDEOS STATS

# 1. Get Playlist-ID of "Upload"-Playlist (so vorgesehen von Youtube und ist der Input für die im Folgenden verwendete Funktion)
chnResources <- list_channel_resources(
  filter = list(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A"),
  part = "contentDetails"
)
# 2. Extract Playlist-ID
playlist_ID <- chnResources$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get all Videos stats
allVids <- get_all_channel_video_stats(playlist_id = playlist_ID)



