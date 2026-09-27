box::use(
  digest[digest],
  httr2[
    response_json,
    url_parse
  ],
  testthat[
    describe,
    expect_equal,
    expect_error,
    expect_false,
    expect_null,
    it,
    test_that
  ],
)
box::use(
  app / logic / lastfm[create_signature, lastfm_api, stop_for_lastfm_error],
)

test_that("stop_for_lastfm_error raises Last.fm's error code and message", {
  expect_error(
    stop_for_lastfm_error(list(error = 10, message = "Invalid API key")),
    "Last.fm API error (10): Invalid API key",
    fixed = TRUE
  )
})

test_that("stop_for_lastfm_error passes successful responses through", {
  result <- list(toptags = list(tag = list()))
  expect_equal(stop_for_lastfm_error(result), result)
})

test_that("create_signature signs params sorted by key with the secret appended", {
  result <- create_signature(list(method = "auth.getSession", api_key = "key"), "secret")
  expected <- digest("api_keykeymethodauth.getSessionsecret", algo = "md5", serialize = FALSE)
  expect_equal(result, expected)
})

test_that("create_signature ignores the format param", {
  with_format <- create_signature(list(api_key = "key", format = "json"), "secret")
  without_format <- create_signature(list(api_key = "key"), "secret")
  expect_equal(with_format, without_format)
})

test_that("create_signature does not depend on param order", {
  expect_equal(
    create_signature(list(b = "2", a = "1"), "secret"),
    create_signature(list(a = "1", b = "2"), "secret")
  )
})

test_that("create_signature changes with the secret", {
  params <- list(api_key = "key")
  expect_false(create_signature(params, "one") == create_signature(params, "two"))
})

describe("lastfm_api", {
  it("sends the method, API key and JSON format with the params", {
    requested <- NULL
    local_spotify_api(
      function(req) stop("unexpected Spotify request"),
      lastfm = function(req) {
        requested <<- url_parse(req$url)$query
        response_json(body = list(ok = TRUE))
      }
    )
    lastfm_api("artist.getInfo", list(artist = "Artist A"))
    expect_equal(requested$method, "artist.getInfo")
    expect_equal(requested$artist, "Artist A")
    expect_equal(requested$api_key, "test-lastfm-key")
    expect_equal(requested$format, "json")
    expect_null(requested$api_sig)
  })

  it("keeps lists of objects as lists", {
    local_spotify_api(
      function(req) stop("unexpected Spotify request"),
      lastfm = function(req) {
        response_json(body = list(toptags = list(tag = list(list(name = "rock"), list(name = "pop")))))
      }
    )
    result <- lastfm_api("artist.getTopTags", list(artist = "Artist A"))
    expect_equal(result$toptags$tag, list(list(name = "rock"), list(name = "pop")))
  })

  it("returns Last.fm's error body instead of raising", {
    local_spotify_api(
      function(req) stop("unexpected Spotify request"),
      lastfm = function(req) {
        response_json(status_code = 400, body = list(error = 6, message = "Artist not found"))
      }
    )
    result <- lastfm_api("artist.getTopTags", list(artist = "Nobody"))
    expect_equal(result$error, 6)
  })
})
