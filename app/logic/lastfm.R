box::use(
  digest[digest],
  httr2[
    request,
    req_error,
    req_perform,
    req_url_query,
    resp_body_json
  ],
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
  # Last.fm reports failures as a JSON body with an `error` field, so return
  # that to the caller instead of raising on the HTTP status
  request(base_url) |>
    req_url_query(!!!params) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE, simplifyDataFrame = FALSE)
}
