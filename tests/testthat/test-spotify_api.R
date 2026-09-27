box::use(
  checkmate[
    expect_data_frame,
    expect_list
  ],
  httr2[
    local_mocked_responses,
    response,
    response_json,
    url_parse
  ],
  testthat[
    describe,
    expect_equal,
    expect_error,
    it,
    test_that
  ],
  withr[local_envvar],
)
box::use(
  app /
    logic /
    spotify_api[
      get_artist,
      get_artist_albums,
      get_artist_top_tracks,
      get_genre_artists,
      get_spotify_access_token,
      search_spotify
    ],
)

artist_items <- list(
  list(id = "id1", name = "Artist A"),
  list(id = "id2", name = "Artist B")
)

describe("get_spotify_access_token", {
  it("returns the access token from a successful response", {
    local_mocked_responses(function(req) {
      response_json(body = list(access_token = "abc123", token_type = "Bearer"))
    })
    expect_equal(get_spotify_access_token("id", "secret"), "abc123")
  })

  it("raises the error description when Spotify rejects the credentials", {
    local_mocked_responses(function(req) {
      response_json(
        status_code = 400,
        body = list(error = "invalid_client", error_description = "Invalid client secret")
      )
    })
    expect_error(get_spotify_access_token("id", "wrong"), "Invalid client secret")
  })

  it("rejects missing credentials before calling Spotify", {
    local_envvar(SPOTIFY_CLIENT_ID = "", SPOTIFY_CLIENT_SECRET = "")
    expect_error(get_spotify_access_token(), "client_id")
    expect_error(get_spotify_access_token("id", ""), "client_secret")
  })
})

describe("search_spotify", {
  it("queries /v1/search and returns the items as a data frame", {
    requested <- NULL
    local_spotify_api(function(req) {
      requested <<- url_parse(req$url)
      response_json(body = list(artists = list(items = artist_items)))
    })
    result <- search_spotify("Artist", type = "artist", limit = 5)
    expect_equal(requested$path, "/v1/search")
    expect_equal(requested$query, list(q = "Artist", type = "artist", limit = "5"))
    expect_data_frame(result, nrows = 2)
    expect_equal(result$id, c("id1", "id2"))
  })

  it("retries with the documented max limit when Spotify rejects the limit", {
    limits <- character(0)
    local_spotify_api(function(req) {
      limit <- url_parse(req$url)$query$limit
      limits <<- c(limits, limit)
      if (limit != "10") {
        return(response(status_code = 400, body = charToRaw("Invalid limit")))
      }
      response_json(body = list(artists = list(items = artist_items)))
    })
    result <- search_spotify("Artist", limit = 50)
    expect_equal(limits, c("50", "10"))
    expect_data_frame(result, nrows = 2)
  })

  it("raises an error with the status code for other API failures", {
    local_spotify_api(function(req) {
      response(status_code = 500, body = charToRaw("server exploded"))
    })
    expect_error(search_spotify("Artist"), "Spotify API error \\(500\\)")
  })

  it("validates its arguments", {
    expect_error(search_spotify(""), "q")
    expect_error(search_spotify("Artist", type = ""), "type")
    expect_error(search_spotify("Artist", limit = 0), "limit")
  })
})

test_that("get_artist requests the artist by id", {
  requested <- NULL
  local_spotify_api(function(req) {
    requested <<- url_parse(req$url)
    response_json(body = list(id = "id1", name = "Artist A", popularity = 42))
  })
  result <- get_artist("id1")
  expect_equal(requested$path, "/v1/artists/id1")
  expect_list(result)
  expect_equal(result$name, "Artist A")
  expect_error(get_artist(""), "id")
})

test_that("get_artist_albums requests the artist's releases in the given groups", {
  requested <- NULL
  local_spotify_api(function(req) {
    requested <<- url_parse(req$url)
    response_json(body = list(total = 35, items = list(list(id = "a1", name = "Album"))))
  })
  result <- get_artist_albums("id1", include_groups = "album,single", limit = 1)
  expect_equal(requested$path, "/v1/artists/id1/albums")
  expect_equal(requested$query, list(include_groups = "album,single", limit = "1"))
  expect_equal(result$total, 35)
  expect_data_frame(result$items, nrows = 1)
  expect_error(get_artist_albums(""), "id")
  expect_error(get_artist_albums("id1", include_groups = "albums"), "include_groups")
  expect_error(get_artist_albums("id1", include_groups = "album,"), "include_groups")
})

test_that("get_artist_top_tracks searches tracks filtered by artist and market", {
  requested <- NULL
  local_spotify_api(function(req) {
    requested <<- url_parse(req$url)
    response_json(body = list(tracks = list(items = list(list(id = "t1", name = "Song")))))
  })
  result <- get_artist_top_tracks("Artist A", market = "BR", limit = 3)
  expect_equal(requested$path, "/v1/search")
  expect_equal(
    requested$query,
    list(q = 'artist:"Artist A"', type = "track", market = "BR", limit = "3")
  )
  expect_data_frame(result, nrows = 1)
  expect_error(get_artist_top_tracks("Artist A", market = "brazil"), "market")
})

test_that("get_genre_artists searches artists filtered by genre", {
  requested <- NULL
  local_spotify_api(function(req) {
    requested <<- url_parse(req$url)
    response_json(body = list(artists = list(items = artist_items)))
  })
  result <- get_genre_artists("rock", limit = 10)
  expect_equal(requested$query, list(q = 'genre:"rock"', type = "artist", limit = "10"))
  expect_data_frame(result, nrows = 2)
  expect_error(get_genre_artists(""), "genre")
})
