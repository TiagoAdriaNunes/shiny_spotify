box::use(
  checkmate[test_null],
  httr2[
    local_mocked_responses,
    response_json
  ],
  memoise[forget],
  withr[local_envvar],
)

# Replace the Spotify Web API with `api` for the rest of the calling test.
# Token requests always succeed; every other request is passed to `api`,
# which receives the httr2 request and returns an httr2 response. Last.fm
# requests go to `lastfm` the same way, and fail the test when it's NULL.
local_spotify_api <- function(api, lastfm = NULL, env = parent.frame()) {
  local_envvar(
    SPOTIFY_CLIENT_ID = "test-client-id",
    SPOTIFY_CLIENT_SECRET = "test-client-secret",
    LASTFM_API_KEY = "test-lastfm-key",
    .local_envir = env
  )
  local_mocked_responses(
    function(req) {
      if (grepl("accounts.spotify.com", req$url, fixed = TRUE)) {
        return(response_json(body = list(access_token = "test-token")))
      }
      if (grepl("ws.audioscrobbler.com", req$url, fixed = TRUE)) {
        if (test_null(lastfm)) {
          stop("Unexpected Last.fm request: ", req$url)
        }
        return(lastfm(req))
      }
      api(req)
    },
    env = env
  )
}

# View modules memoise their API calls; clear the cache so a response mocked
# in one test can't leak into another.
forget_memo <- function(module, name) {
  forget(get(name, envir = environment(module$server)))
}
