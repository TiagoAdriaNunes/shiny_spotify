box::use(
  httr2[
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
)
box::use(
  app / logic / get_artist_tags[get_artist_tags, parse_artist_tags],
)

top_tags <- list(
  toptags = list(
    tag = list(
      list(name = "electronic", count = 100),
      list(name = "house", count = 80),
      list(name = "dance", count = 60)
    )
  )
)

describe("parse_artist_tags", {
  it("returns the tag names in order", {
    expect_equal(parse_artist_tags(top_tags), c("electronic", "house", "dance"))
  })

  it("returns no tags for an artist without any", {
    expect_equal(parse_artist_tags(list(toptags = list(tag = list()))), character(0))
  })

  it("returns no tags for a Last.fm error response", {
    expect_equal(parse_artist_tags(list(error = 6, message = "The artist could not be found")), character(0))
  })

  it("rejects non-list input", {
    expect_error(parse_artist_tags("not a list"), "list")
  })
})

test_that("get_artist_tags requests the artist's top tags and keeps the first `limit`", {
  requested <- NULL
  local_spotify_api(
    function(req) stop("unexpected Spotify request"),
    lastfm = function(req) {
      requested <<- url_parse(req$url)$query
      response_json(body = top_tags)
    }
  )
  expect_equal(get_artist_tags("Artist A", limit = 2), c("electronic", "house"))
  expect_equal(requested$method, "artist.getTopTags")
  expect_equal(requested$artist, "Artist A")
})

test_that("get_artist_tags validates its arguments before calling the API", {
  expect_error(get_artist_tags(""), "artist")
  expect_error(get_artist_tags("Artist A", limit = 0), "limit")
})
