box::use(
  checkmate[expect_class, expect_data_frame],
  testthat[describe, expect_error, it, test_that],
)
box::use(
  app/view/related_artists[render_similar_artists_network],
)

test_that("render_similar_artists_network returns tags$p when similar_artists is NULL", {
  result <- render_similar_artists_network(NULL, "Artist A", NULL)
  expect_class(result, "shiny.tag")
})

test_that("render_similar_artists_network returns tags$p when similar_artists has no rows", {
  empty <- data.frame(name = character(0), match = numeric(0))
  result <- render_similar_artists_network(NULL, "Artist A", empty)
  expect_class(result, "shiny.tag")
})

test_that("render_similar_artists_network requires a main artist name", {
  expect_error(render_similar_artists_network(NULL, "", NULL), "main_artist_name")
  expect_error(render_similar_artists_network(NULL, NULL, NULL), "main_artist_name")
})

test_that("render_similar_artists_network returns a visNetwork object with valid data", {
  similar_artists <- data.frame(
    name  = c("Artist B", "Artist C"),
    match = c(0.9, 0.7),
    stringsAsFactors = FALSE
  )
  result <- render_similar_artists_network(NULL, "Artist A", similar_artists)
  expect_class(result, "visNetwork")
})

describe("render_similar_artists_network node structure", {
  similar_artists <- data.frame(
    name  = c("Artist B", "Artist C"),
    match = c(0.9, 0.7),
    stringsAsFactors = FALSE
  )

  it("includes main artist and similar artists as nodes", {
    result <- render_similar_artists_network(NULL, "Artist A", similar_artists)
    # Main artist + 2 similar + up to second-level (network mocked via real data)
    expect_data_frame(result$x$nodes, min.rows = 3)
  })

  it("includes edges from main artist to similar artists", {
    result <- render_similar_artists_network(NULL, "Artist A", similar_artists)
    expect_data_frame(result$x$edges, min.rows = 2)
  })
})
