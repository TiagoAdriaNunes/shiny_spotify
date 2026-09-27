box::use(
  checkmate[
    expect_data_frame,
    expect_names,
    expect_numeric
  ],
  testthat[
    expect_error,
    expect_null,
    test_that
  ],
)
box::use(
  app / logic / get_similar_artists[get_similar_artists_formatted, parse_similar_artists],
)

mock_response <- list(
  similarartists = list(
    artist = list(
      list(name = "Artist B", match = "0.9"),
      list(name = "Artist C", match = "0.5")
    )
  )
)

test_that("parse_similar_artists returns a data frame with correct columns", {
  result <- parse_similar_artists(mock_response)
  expect_data_frame(result)
  expect_names(names(result), must.include = c("name", "match"))
})

test_that("parse_similar_artists returns correct number of rows", {
  result <- parse_similar_artists(mock_response)
  expect_data_frame(result, nrows = 2)
})

test_that("parse_similar_artists match column is numeric", {
  result <- parse_similar_artists(mock_response)
  expect_numeric(result$match, lower = 0, upper = 1, any.missing = FALSE)
})

test_that("parse_similar_artists returns NULL when no artists found", {
  result <- parse_similar_artists(list(similarartists = list()))
  expect_null(result)
})

test_that("parse_similar_artists returns NULL when the artist list is empty", {
  result <- parse_similar_artists(list(similarartists = list(artist = list())))
  expect_null(result)
})

test_that("parse_similar_artists rejects non-list input", {
  expect_error(parse_similar_artists("not a list"), "list")
})

test_that("get_similar_artists_formatted validates its arguments before calling the API", {
  expect_error(get_similar_artists_formatted(NULL), "artist")
  expect_error(get_similar_artists_formatted(""), "artist")
  expect_error(get_similar_artists_formatted(c("A", "B")), "artist")
  expect_error(get_similar_artists_formatted("Artist A", limit = 0), "limit")
  expect_error(get_similar_artists_formatted("Artist A", limit = 2.5), "limit")
})
