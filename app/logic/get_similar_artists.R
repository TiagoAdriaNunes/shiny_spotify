box::use(
  app/logic/lastfm[lastfm_api],
)

#' Get similar artists from Last.fm API and format the response
#' @param artist The name of the artist to find similar artists for
#' @param limit Optional limit for the number of similar artists to return
#' @return A data frame with similar artists' names and match scores

#' @export
get_similar_artists_formatted <- function(artist, limit = 5) {
  # Get API credentials from environment variables
  api_key <- Sys.getenv("LASTFM_API_KEY")
  api_secret <- Sys.getenv("LASTFM_API_SECRET")
  params <- list(
    artist = artist,
    api_key = api_key,
    method = "artist.getSimilar",
    format = "json"
  )
  # Add limit if specified
  if (!is.null(limit)) {
    params$limit <- limit
  }
  result <- lastfm_api(
    "artist.getSimilar",
    params = params
  )
  # Format the response into a clean data frame
  if (!is.null(result$similarartists$artist)) {
    similar_artists <- data.frame(
      name = vapply(result$similarartists$artist, function(x) x$name, character(1)),
      match = as.numeric(vapply(result$similarartists$artist, function(x) x$match, character(1)))
    )
    return(similar_artists)
  }
  return(NULL)
}
