# spotify_api.R
#
# Spotify tightened its Web API and no longer accepts an access token as a
# query parameter -- the pattern spotifyr's request functions used -- so
# spotifyr's search_spotify(), get_artist(), get_artist_top_tracks() and
# get_genre_artists() all failed with 401 "Missing/invalid/expired access
# token", even for a freshly-issued, valid token. This is a known upstream
# issue: https://github.com/charlie86/spotifyr/issues/221.
#
# Separately, spotifyr's functions assumed a max `limit` of 50, but per
# Spotify's current docs (developer.spotify.com/documentation/web-api/
# reference/search) /v1/search now only accepts limit 0-10 (default 5);
# larger values are rejected with 400 "Invalid limit". get_artist_top_tracks
# is also a documented-deprecated endpoint (as are the followers/genres/
# popularity fields on get_artist), and observed to be inconsistently
# blocked (403) or stripped of fields, so top tracks are sourced from
# /v1/search (type=track, artist:"<name>" filter) instead, same as the
# other functions here.
#
# spotifyr itself is no longer used at all: its only piece we still needed
# was get_spotify_access_token(), a thin OAuth client-credentials POST,
# reimplemented below directly with httr2. That drops a GitHub-only
# dependency (spotifyr isn't on CRAN -- github.com/charlie86/spotifyr/
# issues/201) that otherwise pulled in a pile of transitive packages for a
# single ~10-line call.

box::use(
  dplyr[as_tibble],
  httr2[
    request, req_auth_basic, req_auth_bearer_token, req_body_form, req_error,
    req_perform, req_url_query, resp_body_json, resp_body_string, resp_status,
  ],
  jsonlite[fromJSON],
  stringr[str_glue],
)

# Documented max for /v1/search; used as a retry fallback if a request for
# more (e.g. a future default drifting again) gets rejected.
safe_limit <- 10

#' @export
get_spotify_access_token <- function(
    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")) {
  resp <- request("https://accounts.spotify.com/api/token") |>
    req_auth_basic(client_id, client_secret) |>
    req_body_form(grant_type = "client_credentials") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  body <- resp_body_json(resp)
  if (!is.null(body$error)) {
    stop(str_glue("Could not authenticate with given Spotify credentials:\n\t{body$error_description}"))
  }
  body$access_token
}

spotify_request <- function(url, query = list()) {
  token <- get_spotify_access_token()
  request(url) |>
    req_url_query(!!!query) |>
    req_auth_bearer_token(token) |>
    req_error(is_error = function(resp) FALSE)
}

spotify_get <- function(url, query = list()) {
  resp <- spotify_request(url, query) |> req_perform()

  if (resp_status(resp) == 400 && !is.null(query$limit) && query$limit > safe_limit) {
    body <- resp_body_string(resp)
    if (grepl("limit", body, ignore.case = TRUE)) {
      query$limit <- safe_limit
      resp <- spotify_request(url, query) |> req_perform()
    }
  }

  if (resp_status(resp) >= 400) {
    stop(str_glue("Spotify API error ({resp_status(resp)}): {resp_body_string(resp)}"))
  }

  fromJSON(resp_body_string(resp), flatten = TRUE)
}

#' @export
search_spotify <- function(q, type = "artist", limit = 10) {
  res <- spotify_get(
    "https://api.spotify.com/v1/search",
    query = list(q = q, type = type, limit = limit)
  )
  as_tibble(res[[str_glue("{type}s")]]$items)
}

#' @export
get_artist <- function(id) {
  spotify_get(str_glue("https://api.spotify.com/v1/artists/{id}"))
}

#' @export
get_artist_top_tracks <- function(artist_name, market = "US", limit = 10) {
  res <- spotify_get(
    "https://api.spotify.com/v1/search",
    query = list(q = str_glue('artist:"{artist_name}"'), type = "track", market = market, limit = limit)
  )
  as_tibble(res$tracks$items)
}

#' @export
get_genre_artists <- function(genre, limit = 10) {
  res <- spotify_get(
    "https://api.spotify.com/v1/search",
    query = list(q = str_glue('genre:"{genre}"'), type = "artist", limit = limit)
  )
  as_tibble(res$artists$items)
}
