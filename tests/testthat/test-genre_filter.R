box::use(
  checkmate[test_null],
  httr2[
    response,
    response_json,
    url_parse
  ],
  jsonlite[fromJSON],
  shiny[testServer],
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_no_match,
    expect_null,
    expect_warning,
    test_that
  ],
)
box::use(
  app / view / genre_filter,
)

default_artists <- list(
  list(name = "Artist A", url = "https://www.last.fm/music/Artist+A", `@attr` = list(rank = "1")),
  list(name = "Artist B", url = "https://www.last.fm/music/Artist+B", `@attr` = list(rank = "2"))
)

# Fake Last.fm answering each method the genre tab uses
lastfm_mock <- function(artists = default_artists, top_tags = list(list(name = "rock"))) {
  function(req) {
    query <- url_parse(req$url)$query
    switch(
      query$method,
      chart.getTopTags = response_json(body = list(tags = list(tag = top_tags))),
      tag.getTopArtists = response_json(body = list(topartists = list(artist = artists))),
      artist.getInfo = response_json(
        body = list(artist = list(stats = list(listeners = "1500", playcount = "3000000000")))
      ),
      stop("Unexpected Last.fm method: ", query$method)
    )
  }
}

# Searches `genre` against the mocked Last.fm and returns the rendered outputs
search_genre <- function(genre, lastfm = lastfm_mock()) {
  for (memo in c("get_top_genres_memo", "get_genre_artists_memo", "get_artist_stats_memo")) {
    forget_memo(genre_filter, memo)
  }
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm, env = parent.frame())
  result <- NULL
  testServer(genre_filter$server, {
    session$setInputs(genre = genre, search = 1)
    result <<- list(
      message = output$message,
      table = output$artist_table,
      chart = output$listeners_chart
    )
  })
  result
}

test_that("renders the table and chart for the genre's top artists", {
  result <- search_genre("rock")
  expect_equal(result$message, "")
  expect_false(test_null(result$table))
  expect_match(result$table, "Artist B", fixed = TRUE)
  expect_match(result$table, "https://www.last.fm/music/Artist+A", fixed = TRUE)
  # Play counts past 2^31 survive as numbers
  expect_match(result$table, "3e+09|3000000000")
  expect_match(result$chart, "tagged 'rock' by Last.fm listeners", fixed = TRUE)
})

test_that("still renders when an artist's stats can't be fetched", {
  failing_stats <- function(req) {
    if (url_parse(req$url)$query$method == "artist.getInfo") {
      return(response(status_code = 500))
    }
    lastfm_mock()(req)
  }
  result <- search_genre("jazz", lastfm = failing_stats)
  expect_equal(result$message, "")
  expect_match(result$table, "Artist A", fixed = TRUE)
  # With no listener counts at all there's nothing to chart
  expect_null(fromJSON(result$chart)$x)
})

test_that("tells the user when no artists match the genre", {
  result <- search_genre("not-a-genre", lastfm = lastfm_mock(artists = list()))
  expect_equal(result$message, "No artists found for the genre 'not-a-genre'. Please try a different genre.")
  # A widget rendered from NULL serialises with no data (`x`)
  expect_null(fromJSON(result$table)$x)
  expect_null(fromJSON(result$chart)$x)
})

test_that("shows an unavailable message when Last.fm returns an error", {
  lastfm_error <- function(req) {
    response_json(status_code = 403, body = list(error = 10, message = "Invalid API key"))
  }
  expect_warning(
    result <- search_genre("rock", lastfm = lastfm_error),
    "Last.fm genre search failed: .*Invalid API key"
  )
  expect_equal(result$message, "Genre search unavailable. Please try again later.")
  expect_null(fromJSON(result$table)$x)
})

# Renders the genre tab UI against the mocked Last.fm and returns its HTML
render_genre_ui <- function(lastfm) {
  forget_memo(genre_filter, "get_top_genres_memo")
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm, env = parent.frame())
  as.character(genre_filter$ui("genre_filter"))
}

test_that("the genre list is rendered into the page from Last.fm's top tags", {
  top_tags <- list(list(name = "rock"), list(name = "seen live"), list(name = "jazz"))
  html <- render_genre_ui(lastfm_mock(top_tags = top_tags))
  expect_match(html, '<option value="rock">rock</option>', fixed = TRUE)
  expect_match(html, '<option value="jazz">jazz</option>', fixed = TRUE)
  expect_no_match(html, "seen live", fixed = TRUE)
})

test_that("the genre tab still renders when Last.fm's top tags can't be fetched", {
  expect_warning(
    html <- render_genre_ui(function(req) response(status_code = 500)),
    "Last.fm top genres fetch failed"
  )
  expect_match(html, 'id="genre_filter-genre"', fixed = TRUE)
  expect_no_match(html, "<option value=\"[^\"]+\">", perl = TRUE)
})
