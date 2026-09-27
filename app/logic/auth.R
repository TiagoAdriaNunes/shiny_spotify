box::use(
  checkmate[
    assert_string,
    test_null
  ],
  digest[digest],
  dplyr[coalesce],
  httr2[
    request,
    req_auth_basic,
    req_body_form,
    req_error,
    req_perform,
    resp_body_json
  ],
  stringr[str_glue],
)

# Spotify client-credentials tokens last an hour. Requesting one per API call
# doubled the app's Spotify traffic and counted against its quota, so the
# token is cached and reused until shortly before it expires.

# Refresh this long before the stated expiry, so a token handed out just
# before it expires can't lapse while its request is in flight
expiry_margin_seconds <- 60

# Used when Spotify doesn't say how long the token lasts
default_lifetime_seconds <- 3600

# The cached token, when it should be refreshed, and a hash of the credentials
# it was issued for (so the secret itself isn't stored). An environment so it
# can be updated in place: box locks a module's own bindings.
token_cache <- new.env(parent = emptyenv())

# Request a new client-credentials token. Returns Spotify's response body,
# with `access_token` and `expires_in` (seconds).
request_spotify_token <- function(client_id, client_secret) {
  resp <- request("https://accounts.spotify.com/api/token") |>
    req_auth_basic(client_id, client_secret) |>
    req_body_form(grant_type = "client_credentials") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  body <- resp_body_json(resp)
  if (!test_null(body$error)) {
    stop(str_glue("Could not authenticate with given Spotify credentials:\n\t{body$error_description}"))
  }
  body
}

#' Get a Spotify access token, reusing the cached one while it's valid
#' @param client_id,client_secret Spotify app credentials, read from
#'   .Renviron by default
#' @param now The current time; an argument so tests can simulate expiry
#' @return The access token string
#' @export
get_spotify_access_token <- function(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  now = Sys.time()
) {
  assert_string(client_id, min.chars = 1)
  assert_string(client_secret, min.chars = 1)
  key <- digest(c(client_id, client_secret))
  if (identical(token_cache$key, key) && now < token_cache$refresh_at) {
    return(token_cache$token)
  }
  body <- request_spotify_token(client_id, client_secret)
  lifetime <- as.numeric(coalesce(body$expires_in, default_lifetime_seconds))
  token_cache$token <- body$access_token
  token_cache$key <- key
  token_cache$refresh_at <- now + lifetime - expiry_margin_seconds
  body$access_token
}

#' Forget the cached token, so the next call requests a new one. Used when
#' Spotify rejects a token before its stated expiry, e.g. if it was revoked.
#' @export
clear_spotify_token <- function() {
  rm(list = ls(token_cache), envir = token_cache)
  invisible(NULL)
}
