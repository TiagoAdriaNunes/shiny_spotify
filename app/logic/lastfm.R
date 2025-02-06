box::use(
  httr[GET],
  jsonlite[fromJSON]
)

#' @export
lastfm_api <- function(method, params = list()) {
  base_url <- "http://ws.audioscrobbler.com/2.0/"
  # Add API key and format to parameters
  params$api_key <- Sys.getenv("LASTFM_API_KEY")
  params$method <- method
  params$format <- "json"
  # Create API signature if needed
  if (method %in% c("auth.getSession", "track.scrobble")) {
    params$api_sig <- create_signature(params, Sys.getenv("LASTFM_API_SECRET"))
  }
  # Make API request
  response <- httr::GET(
    base_url,
    query = params
  )
  # Parse response and convert to list
  content <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  return(content)
}
