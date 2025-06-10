# Get number of Followers per Artist => On Pause bcs of overload on API
# mutate(artistFollowers = map_dbl(as.character(artist), get_max_subscribers)) 
#### -> Händisch probieren, wie gut das beispielhaft überhaupt (konzeptionell) funktioniert 

testData <- vidStats_df[1:10,]

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


testData <- testData %>%
  mutate(artistFollower = )