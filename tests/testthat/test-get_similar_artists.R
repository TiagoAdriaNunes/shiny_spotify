box::use(
  testthat[expect_equal, expect_null, expect_true, test_that],
)
box::use(
  app/logic/get_similar_artists[parse_similar_artists],
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
  expect_true(is.data.frame(result))
  expect_true(all(c("name", "match") %in% names(result)))
})

test_that("parse_similar_artists returns correct number of rows", {
  result <- parse_similar_artists(mock_response)
  expect_equal(nrow(result), 2)
})

test_that("parse_similar_artists match column is numeric", {
  result <- parse_similar_artists(mock_response)
  expect_true(is.numeric(result$match))
})

test_that("parse_similar_artists returns NULL when no artists found", {
  result <- parse_similar_artists(list(similarartists = list()))
  expect_null(result)
})
