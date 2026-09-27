box::use(
  checkmate[
    expect_data_frame,
    expect_names
  ],
  httr2[
    response_json,
    url_parse
  ],
  testthat[
    describe,
    expect_equal,
    expect_error,
    expect_null,
    it,
    test_that
  ],
)
box::use(
  app / logic / lastfm_genres[
    add_artist_stats,
    get_artist_stats,
    get_genre_artists,
    get_top_genres,
    parse_artist_stats,
    parse_genre_artists,
    parse_top_genres
  ],
)

# Serves `body` for every Last.fm request and records the last query sent
local_lastfm <- function(body, env = parent.frame()) {
  requested <- new.env()
  local_spotify_api(
    function(req) stop("unexpected Spotify request"),
    lastfm = function(req) {
      requested$query <- url_parse(req$url)$query
      response_json(body = body)
    },
    env = env
  )
  requested
}

lastfm_error <- list(error = 10, message = "Invalid API key")

describe("parse_top_genres", {
  it("keeps tag names in chart order and drops tags that aren't genres", {
    result <- list(tags = list(tag = list(
      list(name = "rock"),
      list(name = "seen live"),
      list(name = "Female Vocalists"),
      list(name = "80s"),
      list(name = "jazz")
    )))
    expect_equal(parse_top_genres(result), c("rock", "80s", "jazz"))
  })

  it("returns no genres for an empty chart", {
    expect_equal(parse_top_genres(list(tags = list(tag = list()))), character(0))
  })
})

describe("get_top_genres", {
  it("requests the top tags chart with the limit", {
    requested <- local_lastfm(list(tags = list(tag = list(list(name = "rock")))))
    expect_equal(get_top_genres(limit = 50), "rock")
    expect_equal(requested$query$method, "chart.getTopTags")
    expect_equal(requested$query$limit, "50")
  })

  it("raises Last.fm errors instead of returning an empty list", {
    local_lastfm(lastfm_error)
    expect_error(get_top_genres(), "Invalid API key")
  })
})

describe("parse_genre_artists", {
  it("returns rank, name and url for each artist", {
    result <- list(topartists = list(artist = list(
      list(name = "Artist A", url = "https://www.last.fm/music/A", `@attr` = list(rank = "1")),
      list(name = "Artist B", url = "https://www.last.fm/music/B", `@attr` = list(rank = "2"))
    )))
    artists <- parse_genre_artists(result)
    expect_data_frame(artists, nrows = 2)
    expect_equal(artists$rank, c(1L, 2L))
    expect_equal(artists$name, c("Artist A", "Artist B"))
    expect_equal(artists$url, c("https://www.last.fm/music/A", "https://www.last.fm/music/B"))
  })

  it("returns NULL for an unknown genre", {
    expect_null(parse_genre_artists(list(topartists = list(artist = list()))))
  })
})

describe("get_genre_artists", {
  it("requests the tag's top artists", {
    requested <- local_lastfm(list(topartists = list(artist = list())))
    get_genre_artists("post-rock", limit = 20)
    expect_equal(requested$query$method, "tag.getTopArtists")
    expect_equal(requested$query$tag, "post-rock")
    expect_equal(requested$query$limit, "20")
  })

  it("raises Last.fm errors", {
    local_lastfm(lastfm_error)
    expect_error(get_genre_artists("rock"), "Last.fm API error \\(10\\)")
  })

  it("validates its arguments", {
    expect_error(get_genre_artists(""), "genre")
    expect_error(get_genre_artists("rock", limit = 0), "limit")
  })
})

describe("parse_artist_stats", {
  it("converts the counts to numbers, keeping play counts past 2^31", {
    result <- list(artist = list(stats = list(listeners = "8452823", playcount = "3000000000")))
    expect_equal(parse_artist_stats(result), list(listeners = 8452823, playcount = 3e9))
  })

  it("returns NA for missing stats", {
    expect_equal(parse_artist_stats(list()), list(listeners = NA_real_, playcount = NA_real_))
  })
})

test_that("get_artist_stats requests the artist's info", {
  requested <- local_lastfm(list(artist = list(stats = list(listeners = "10", playcount = "20"))))
  expect_equal(get_artist_stats("Artist A"), list(listeners = 10, playcount = 20))
  expect_equal(requested$query$method, "artist.getInfo")
  expect_equal(requested$query$artist, "Artist A")
})

describe("add_artist_stats", {
  artists <- data.frame(rank = 1:2, name = c("Artist A", "Artist B"))

  it("adds listeners and plays for each artist", {
    stats <- function(name) list(listeners = nchar(name) * 10, playcount = nchar(name) * 100)
    result <- add_artist_stats(artists, get_stats = stats)
    expect_names(names(result), identical.to = c("rank", "name", "listeners", "playcount"))
    expect_equal(result$listeners, c(80, 80))
    expect_equal(result$playcount, c(800, 800))
  })

  it("uses NA for an artist whose lookup fails", {
    stats <- function(name) {
      if (name == "Artist B") stop("not found")
      list(listeners = 1, playcount = 2)
    }
    result <- add_artist_stats(artists, get_stats = stats)
    expect_equal(result$listeners, c(1, NA))
    expect_equal(result$playcount, c(2, NA))
  })
})
