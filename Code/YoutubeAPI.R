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

# TODO: Get videoname and uploaddate with "get_video_details" 

