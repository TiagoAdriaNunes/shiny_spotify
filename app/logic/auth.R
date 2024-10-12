# auth.R

box::use(
  spotifyr[get_spotify_access_token],
)

# get_spotify_access_token get client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
# client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET") from .Renviron
get_spotify_access_token()

access_token <- get_spotify_access_token() #nolint
