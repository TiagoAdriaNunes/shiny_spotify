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
# reimplemented directly with httr2 in app/logic/auth.R (which also caches
# the token). That drops a GitHub-only
# dependency (spotifyr isn't on CRAN -- github.com/charlie86/spotifyr/
# issues/201) that otherwise pulled in a pile of transitive packages for a
# single ~10-line call.

box::use(
  checkmate[
    assert_int,
    assert_string,
    test_null
  ],
  dplyr[
    as_tibble,
    bind_rows,
    distinct
  ],
  httr2[
    request,
    req_auth_bearer_token,
    req_error,
    req_perform,
    req_url_query,
    resp_body_string,
    resp_status,
  ],
  jsonlite[fromJSON],
  purrr[map_lgl],
  stringr[str_glue],
)

box::use(
  app / logic / auth[clear_spotify_token, get_spotify_access_token],
)

# Documented max for /v1/search; used as a retry fallback if a request for
# more (e.g. a future default drifting again) gets rejected.
safe_limit <- 10

spotify_request <- function(url, query = list()) {
  token <- get_spotify_access_token()
  request(url) |>
    req_url_query(!!!query) |>
    req_auth_bearer_token(token) |>
    req_error(is_error = function(resp) FALSE)
}

spotify_get <- function(url, query = list()) {
  resp <- spotify_request(url, query) |> req_perform()

  # The token is cached (see app/logic/auth.R), and Spotify can reject one
  # before its stated expiry, e.g. if it was revoked. Get a fresh token and
  # retry once; a second 401 is reported as an error below.
  if (resp_status(resp) == 401) {
    clear_spotify_token()
    resp <- spotify_request(url, query) |> req_perform()
  }

  if (resp_status(resp) == 400 && !test_null(query$limit) && query$limit > safe_limit) {
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
  assert_string(q, min.chars = 1)
  assert_string(type, min.chars = 1)
  assert_int(limit, lower = 1)
  res <- spotify_get(
    "https://api.spotify.com/v1/search",
    query = list(q = q, type = type, limit = limit)
  )
  as_tibble(res[[str_glue("{type}s")]]$items)
}

#' @export
get_artist <- function(id) {
  assert_string(id, min.chars = 1)
  spotify_get(str_glue("https://api.spotify.com/v1/artists/{id}"))
}

#' Get a page of an artist's releases. Returns the raw response, whose
#' `total` counts every release in `include_groups`, not just this page.
#' @export
get_artist_albums <- function(id, include_groups = "album", limit = 10) {
  assert_string(id, min.chars = 1)
  group <- "(album|single|compilation|appears_on)"
  assert_string(include_groups, pattern = str_glue("^{group}(,{group})*$"))
  assert_int(limit, lower = 1)
  spotify_get(
    str_glue("https://api.spotify.com/v1/artists/{id}/albums"),
    query = list(include_groups = include_groups, limit = limit)
  )
}

#' Search for tracks and verify that they credit the selected Spotify artist ID.
#' @export
get_artist_top_tracks <- function(artist_name, artist_id, market = "US", limit = 10) {
  assert_string(artist_name, min.chars = 1)
  assert_string(artist_id, min.chars = 1)
  assert_string(market, pattern = "^[A-Z]{2}$")
  assert_int(limit, lower = 1)
  search_tracks <- function(query) {
    res <- spotify_get(
      "https://api.spotify.com/v1/search",
      query = list(q = query, type = "track", market = market, limit = limit)
    )
    tracks <- as_tibble(res$tracks$items)
    # Spotify's artist-name filter is fuzzy (e.g. Romy matches Romeo Santos).
    # Only keep tracks crediting the selected ID, including collaborations.
    if (!"artists" %in% names(tracks)) {
      return(tracks[0, ])
    }
    matches <- map_lgl(tracks$artists, function(artists) {
      is.data.frame(artists) && artist_id %in% artists$id
    })
    tracks[matches, ]
  }
  tracks <- search_tracks(str_glue('artist:"{artist_name}"'))
  # The fielded search can miss the artist entirely; a broader search finds
  # candidates, still subject to the same exact-ID check.
  if (nrow(tracks) < min(5, limit)) {
    fallback <- tryCatch(search_tracks(artist_name), error = function(e) {
      if (nrow(tracks) == 0) {
        stop(e)
      }
      tracks[0, ]
    })
    tracks <- bind_rows(tracks, fallback)
    if ("id" %in% names(tracks)) tracks <- distinct(tracks, id, .keep_all = TRUE)
  }
  tracks
}
