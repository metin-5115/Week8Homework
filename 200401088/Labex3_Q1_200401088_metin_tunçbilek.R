library(httr)



spotify_token <- function() {
  token_url <- "https://accounts.spotify.com/api/token"
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SPOTIFY_SECRET")
  
  body <- list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret
  )
  
  response <- httr::POST(
    url = token_url,
    body = body,
    encode = "form",
    httr::add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  
  status_code <- as.numeric(httr::status_code(response))
  
  token <- content(response)$access_token
  bearer_token <- paste("Bearer", token)
  
  result <- list(
    status_code = status_code,
    token = bearer_token
  )
  
  return(result)
}

spotify_artist_top_tracks <- function(artist_id) {
  token_result <- spotify_token()
  if (token_result$status_code != 200) {
    return(list(
      status_code = token_result$status_code,
      resultdf = NULL
    ))
  }
  
  token <- token_result$token
  
  endpoint <- paste0("https://api.spotify.com/v1/artists/", artist_id, "/top-tracks?market=US")
  response <- httr::GET(
    url = endpoint,
    httr::add_headers("Authorization" = token)
  )
  
  status_code <- as.numeric(httr::status_code(response))
  
  if (status_code != 200) {
    return(list(
      status_code = status_code,
      resultdf = NULL
    ))
  }
  
  track_list <- content(response)$tracks
  
  track_info <- lapply(track_list, function(track) {
    c(
      id = track$id,
      name = track$name,
      artist = track$artists[[1]]$name,
      album = track$album$name,
      year = substr(track$album$release_date, 1, 10)
    )
  })
  
  result_df <- do.call(rbind, track_info)
  result_df <- as.data.frame(result_df)
  
  colnames(result_df) <- c("id", "name", "artist", "album", "year")
  
  return(list(
    status_code = status_code,
    resultdf = result_df
  ))
}
