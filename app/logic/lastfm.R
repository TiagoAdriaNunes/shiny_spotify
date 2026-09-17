box::use(
  digest[digest],
  httr[GET],
  jsonlite[fromJSON],
  purrr[map_chr],
)

#' Build the Last.fm API request signature.
#' Signs every param except `format`, sorted alphabetically by key, with
#' the shared secret appended, per https://www.last.fm/api/authspec.
#' @export
create_signature <- function(params, secret) {
  params$format <- NULL
  keys <- sort(names(params))
  signature_base <- paste0(
    paste0(keys, map_chr(params[keys], as.character), collapse = ""),
    secret
  )
  digest(signature_base, algo = "md5", serialize = FALSE)
}

#' @export
lastfm_api <- function(method, params = list()) {
  base_url <- "https://ws.audioscrobbler.com/2.0/"
  # Add API key and format to parameters
  params$api_key <- Sys.getenv("LASTFM_API_KEY")
  params$method <- method
  params$format <- "json"
  # Create API signature if needed
  if (method %in% c("auth.getSession", "track.scrobble")) {
    params$api_sig <- create_signature(params, Sys.getenv("LASTFM_API_SECRET"))
  }
  # Make API request
  response <- GET(
    base_url,
    query = params
  )
  # Parse response and convert to list
  content <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  content
}
