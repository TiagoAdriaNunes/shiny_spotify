box::use(
  httr2[
    local_mocked_responses,
    response_json
  ],
  testthat[
    describe,
    expect_equal,
    expect_error,
    it
  ],
  withr[local_envvar],
)
box::use(
  app / logic / auth[clear_spotify_token, get_spotify_access_token],
)

# Fake token endpoint issuing "token-1", "token-2", ... and counting requests
local_token_endpoint <- function(expires_in = 3600, env = parent.frame()) {
  local_clean_token_cache(env)
  calls <- new.env()
  calls$n <- 0
  local_mocked_responses(
    function(req) {
      calls$n <- calls$n + 1
      body <- list(access_token = paste0("token-", calls$n))
      # Assigning NULL leaves the field out, like a response without it
      body$expires_in <- expires_in
      response_json(body = body)
    },
    env = env
  )
  calls
}

describe("get_spotify_access_token", {
  it("returns the access token from a successful response", {
    local_token_endpoint()
    expect_equal(get_spotify_access_token("id", "secret"), "token-1")
  })

  it("reuses the cached token instead of requesting a new one", {
    calls <- local_token_endpoint()
    now <- Sys.time()
    expect_equal(get_spotify_access_token("id", "secret", now = now), "token-1")
    expect_equal(get_spotify_access_token("id", "secret", now = now + 1800), "token-1")
    expect_equal(calls$n, 1)
  })

  it("requests a new token a minute before the old one expires", {
    calls <- local_token_endpoint(expires_in = 3600)
    now <- Sys.time()
    get_spotify_access_token("id", "secret", now = now)
    expect_equal(get_spotify_access_token("id", "secret", now = now + 3539), "token-1")
    expect_equal(get_spotify_access_token("id", "secret", now = now + 3540), "token-2")
    expect_equal(calls$n, 2)
  })

  it("assumes an hour when Spotify doesn't say how long the token lasts", {
    calls <- local_token_endpoint(expires_in = NULL)
    now <- Sys.time()
    get_spotify_access_token("id", "secret", now = now)
    expect_equal(get_spotify_access_token("id", "secret", now = now + 3000), "token-1")
    expect_equal(calls$n, 1)
  })

  it("requests a new token when the credentials change", {
    calls <- local_token_endpoint()
    expect_equal(get_spotify_access_token("id", "secret"), "token-1")
    expect_equal(get_spotify_access_token("other-id", "secret"), "token-2")
    expect_equal(get_spotify_access_token("other-id", "other-secret"), "token-3")
    expect_equal(calls$n, 3)
  })

  it("requests a new token after the cache is cleared", {
    calls <- local_token_endpoint()
    get_spotify_access_token("id", "secret")
    clear_spotify_token()
    expect_equal(get_spotify_access_token("id", "secret"), "token-2")
    expect_equal(calls$n, 2)
  })

  it("raises the error description when Spotify rejects the credentials, and caches nothing", {
    local_clean_token_cache()
    local_mocked_responses(function(req) {
      response_json(
        status_code = 400,
        body = list(error = "invalid_client", error_description = "Invalid client secret")
      )
    })
    expect_error(get_spotify_access_token("id", "wrong"), "Invalid client secret")
    expect_error(get_spotify_access_token("id", "wrong"), "Invalid client secret")
  })

  it("rejects missing credentials before calling Spotify", {
    local_envvar(SPOTIFY_CLIENT_ID = "", SPOTIFY_CLIENT_SECRET = "")
    expect_error(get_spotify_access_token(), "client_id")
    expect_error(get_spotify_access_token("id", ""), "client_secret")
  })
})
