box::use(
  httr2[response_json],
  jsonlite[fromJSON],
  shiny[testServer],
  testthat[expect_equal, expect_false, expect_match, expect_null, test_that],
)
box::use(
  app / view / genre_filter,
)

# Searches `genre` against the mocked Spotify `items` and returns the
# rendered outputs.
search_genre <- function(genre, items) {
  forget_memo(genre_filter, "get_genre_artists_memo")
  local_spotify_api(
    function(req) response_json(body = list(artists = list(items = items))),
    env = parent.frame()
  )
  result <- NULL
  testServer(genre_filter$server, {
    session$setInputs(genre = genre, search = 1)
    result <<- list(
      message = output$message,
      table = output$artist_table,
      chart = output$followers_chart
    )
  })
  result
}

test_that("renders the table and chart for artists in the genre", {
  result <- search_genre("rock", list(
    list(name = "Artist A", popularity = 70, followers = list(total = 100), genres = list("rock")),
    list(name = "Artist B", popularity = 90, followers = list(total = 500), genres = list("rock", "pop"))
  ))
  expect_equal(result$message, "")
  expect_false(is.null(result$table))
  expect_false(is.null(result$chart))
})

test_that("still renders when Spotify omits followers, popularity and genres", {
  result <- search_genre("jazz", list(list(name = "Artist A"), list(name = "Artist B")))
  expect_equal(result$message, "")
  expect_false(is.null(result$table))
})

test_that("tells the user when no artists match the genre", {
  result <- search_genre("not-a-genre", list())
  expect_match(result$message, "No artists found for the genre", fixed = TRUE)
  # A widget rendered from NULL serialises with no data (`x`)
  expect_null(fromJSON(result$table)$x)
  expect_null(fromJSON(result$chart)$x)
})
