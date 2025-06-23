# Get number of Followers per Artist => On Pause bcs of overload on API
# mutate(artistFollowers = map_dbl(as.character(artist), get_max_subscribers)) 
#### -> Händisch probieren, wie gut das beispielhaft überhaupt (konzeptionell) funktioniert 

follower_df <- vidStats_df

# needed to get the subscribers of the artists => ON PAUSE
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


#testData <- testData %>%
#  mutate(artistFollower = map_dbl(as.character(artist), get_max_subscribers))


# ! Bis zu 100 Artist am Tag sind mit der Quota vereinbar -> Funktion schreiben, die für jeweils 5 oder 10 Künstler die Followeranzahlen überprüft
# Somit kann auch jeder einzelfall kurz angeschaut werden und überprüft werden. Ansonsten kann jede Followerzahl händisch von Youtube gezogen werden


writeFollower <- function(df, limit = 5) {
  # artists without followernumber only
  unprocessed_artists <- df %>%
    filter(artistFollower == 0) %>%
    distinct(artist) %>%
    pull(artist)
  # Set limit 
  artists_to_process <- head(unprocessed_artists, limit)
  
  for (artist_name in artists_to_process) {
    # Get follower
    subs <- get_max_subscribers(artist_name)
    
    # Write into dataframe
    df <- df %>%
    #  mutate(artistFollower = if_else(artist == artist_name & artistFollower == 0, subs, artistFollower))
      mutate(artistFollower = if_else(str_detect(artist, fixed(artist_name)) & artistFollower == 0, subs, artistFollower))
  }
  return(df)
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

# New Funciton
follower_df <- writeFollower_with_manual_fallback(follower_df, batch_size = 5)

# to secure the Followers 
export(follower_df, "FollowerDF_Stand23.06..csv")
