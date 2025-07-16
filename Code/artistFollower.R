# Get number of Followers per Artist 

follower_df <- vidStats_df

# needed to get the subscribers of the artists
get_max_subscribers <- function(artist_name) {
  # Fehler abfangen & Ausgabe unterdrücken
  suppressMessages(
    tryCatch({
      channel_ids <- yt_search(artist_name, type = "channel", max_results = 5) %>%
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




writeFollower_with_manual_fallback <- function(df, batch_size = 5) {
  count <- 0
  for (i in 1:nrow(df)) {
    if (!is.na(df$artistFollower[i]) && df$artistFollower[i] == 0 && count < batch_size) {
      
      artist_name <- df$artist[i]
      artist_name_clean <- trimws(as.character(artist_name))
      
      # Follower über API versuchen
      subs <- get_max_subscribers(artist_name_clean)
      
      # Wenn API NA zurückgibt -> manuelle Eingabe
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
      
      # Wenn ein gültiger Wert (nicht NA), dann alle passenden Zeilen im df aktualisieren
      if (!is.na(subs) && subs > 0) {
        df$artistFollower[str_detect(df$artist, fixed(artist_name_clean)) & df$artistFollower == 0] <- subs
        count <- count + 1
      }
    }
  }
  
  message("✅ Fertig: ", count, " Künstler aktualisiert.")
  return(df)
}





follower_df <- follower_df %>%
  mutate(artistFollower = 0)

# to import datasets with Followers getted 
follower_df <- import("FollowerDF_Stand,,,.csv", format = "csv")

# Old Function:
# follower_df <- writeFollower(follower_df, limit = 10)

# New Function
follower_df <- writeFollower_with_manual_fallback(follower_df, batch_size = 10)

# to secure the Followers 
export(follower_df, "FollowerDF_Stand12.07..csv")



### QUALITY_CHECK AND IMPROVEMENTS OF THE DATA (artistFollower only)

# Check for possible wrong numbers
testfollower <- follower_df %>%
  mutate(MORE = ifelse(artistFollower > viewCount, 1, 0))
# Result: NEED TO CHECK FOR ARTISTS, WHEN Followers > viewCount => Its sometimes false then (really not often)
# Often the case, when artists are very famous -> Maybe account for that? 
# Divide Data in these 2 groups -> Maybe then people who just got famous after the Tiny desk concert vs those who were it before? 

# To correct the followers in a certain row use:
follower_df$artistFollower[1] <- 1

# Corrected:
# - Wrong String-Differentiations (Follower doubled even tho the artist is another) until row 1196 
# - Followers > viewCount until row ... 
# - Changed the followers of "Not a normal Concert"´s to 1 
#   follower_df$artistFollower <- ifelse(follower_df$artist == "Not a normal Concert", 1, follower_df$artistFollower)
