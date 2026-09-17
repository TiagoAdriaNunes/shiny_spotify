# auth.R

box::use(
  app/logic/spotify_api[get_spotify_access_token],
)

# get_spotify_access_token get client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
# client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET") from .Renviron
# Wrapped in tryCatch so a Spotify outage or bad credentials doesn't take
# down the whole app at box::use() load time (e.g. the Last.fm-only tab
# should keep working without a Spotify token).
access_token <- tryCatch(
  get_spotify_access_token(), #nolint
  error = function(e) {
    warning("Spotify authentication failed: ", conditionMessage(e), call. = FALSE)
    NULL
  }
)
