# initally composition: 
follower_df <- vidStats_df

### To not retrieve every Follower again, but only those for new observations, 
# use "anti_join" and detect missing IDs by synchronizing the datasets
# New observations then get added to the follower_df (optional)###

# rows of vidStats_df, which are not yet part of follower_df 
followerMissing <- anti_join(vidStats_df, follower_df, by = "id") %>%
  select(id, artist)
# add these rows to follower_df
follower_df <- bind_rows(follower_df, followerMissing)
follower_df$artistFollower[which(is.na(follower_df$artistFollower))] <- 0



# function to scrape subscribers of an artist
get_max_subscribers <- function(artist_name) {
  # catch errors and suppres messages
  suppressMessages(
    tryCatch({
      # Retrieve channel-IDs from channel-search results
      channel_ids <- yt_search(artist_name, type = "channel", max_results = 5) %>%
        head(5) %>%
        pull(channelId)
      
      if (length(channel_ids) == 0) return(NA_real_)
      
      sub_counts <- numeric(length(channel_ids))
      
      for (i in seq_along(channel_ids)) {
        # Get Stats for every channel found
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

# Function, to create manual fallback for cases of "get_max_subscribers" not finding any proper channel
writeFollower_with_manual_fallback <- function(df, batch_size = 5) {
  count <- 0
  for (i in 1:nrow(df)) {
    if (!is.na(df$artistFollower[i]) && df$artistFollower[i] == 0 && count < batch_size) {
      
      artist_name <- df$artist[i]
      artist_name_clean <- trimws(as.character(artist_name))
      
      # try to get followers by using the API
      subs <- get_max_subscribers(artist_name_clean)
      
      # If API gives back "NA" -> manual input
      if (is.na(subs)) {
        message("\nKeine Followerzahl gefunden für: ", artist_name_clean)
        input <- readline(prompt = "Bitte manuell die Followerzahl eingeben (oder ENTER für NA): ")
        
        if (input == "") {
          subs <- NA_real_
        } else if (!grepl("^[0-9]+$", input)) {
          message("⚠️ Ungültige Eingabe – wird als NA gesetzt.")
          subs <- NA_real_
        } else {
          subs <- as.numeric(input)
        }
      }
      
      # if value is valid(not NA): update dataframe accordingly
      if (!is.na(subs) && subs > 0) {
        df$artistFollower[str_detect(df$artist, fixed(artist_name_clean)) & df$artistFollower == 0] <- subs
        count <- count + 1
      }
    }
  }
  
  message("✅ Fertig: ", count, " Künstler aktualisiert.")
  return(df)
}




# import "old" datasets with Followers already collected (optional)
follower_df <- import("FollowerDF_Stand,,,.csv", format = "csv")

# use Function "writeFollower_with_manual_fallback"
follower_df <- writeFollower_with_manual_fallback(follower_df, batch_size = 10)

# secure the Followers (optional)
export(follower_df, "FollowerDF_Stand23.07..csv")
follower_df <- import("FollowerDF_Stand23.07..csv", format = "csv")



### QUALITY_CHECK AND IMPROVEMENT OF THE DATA (artistFollower only)

# Check for possible wrong numbers
# Result: NEED TO CHECK FOR ARTISTS, WHEN "Followers > viewCount" => Its sometimes false then (not very often)
# Often the case, when artists are VERY famous -> Could analyse that by making this szenario a binary variable
# This could tell, whether an artist was already famous before the concert or retrieved fans from it


# Quality check checklist: (optional) -> DELETE LATER
# - Wrong String-Differentiations (Follower doubled even tho the artist is another) -> Completed
# - Followers > viewCount until row ... 
testing <- which(follower_df$artistFollower > follower_df$viewCount)
print(testing)

# To correct the followers in a certain row, use:
follower_df$artistFollower[1] <- 1
