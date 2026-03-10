box::use(
  testthat[expect_equal, expect_null, expect_true, test_that],
)
box::use(
  app/logic/get_similar_artists[get_similar_artists_formatted],
)

# Mock Last.fm response with two similar artists
mock_lastfm_response <- list(
  similarartists = list(
    artist = list(
      list(name = "Artist B", match = "0.9"),
      list(name = "Artist C", match = "0.5")
    )
  )
)

test_that("get_similar_artists_formatted returns a data frame with correct columns", {
  mockery::stub(get_similar_artists_formatted, "lastfm_api", mock_lastfm_response)
  result <- get_similar_artists_formatted("Artist A", limit = 2)
  expect_true(is.data.frame(result))
  expect_true(all(c("name", "match") %in% names(result)))
})

test_that("get_similar_artists_formatted returns correct number of rows", {
  mockery::stub(get_similar_artists_formatted, "lastfm_api", mock_lastfm_response)
  result <- get_similar_artists_formatted("Artist A", limit = 2)
  expect_equal(nrow(result), 2)
})

test_that("get_similar_artists_formatted match column is numeric", {
  mockery::stub(get_similar_artists_formatted, "lastfm_api", mock_lastfm_response)
  result <- get_similar_artists_formatted("Artist A", limit = 2)
  expect_true(is.numeric(result$match))
})

test_that("get_similar_artists_formatted returns NULL when no artists found", {
  mockery::stub(get_similar_artists_formatted, "lastfm_api", list(similarartists = list()))
  result <- get_similar_artists_formatted("Unknown Artist")
  expect_null(result)
})
