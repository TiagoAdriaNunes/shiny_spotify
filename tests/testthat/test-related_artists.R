box::use(
  testthat[describe, expect_equal, expect_true, it, test_that],
)
box::use(
  app/view/related_artists[render_similar_artists_network],
)

test_that("render_similar_artists_network returns tags$p when similar_artists is NULL", {
  result <- render_similar_artists_network(NULL, "Artist A", NULL)
  expect_true(inherits(result, "shiny.tag"))
})

test_that("render_similar_artists_network returns a visNetwork object with valid data", {
  similar_artists <- data.frame(
    name  = c("Artist B", "Artist C"),
    match = c(0.9, 0.7),
    stringsAsFactors = FALSE
  )
  result <- render_similar_artists_network(NULL, "Artist A", similar_artists)
  expect_true(inherits(result, "visNetwork"))
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
    expect_true(nrow(result$x$nodes) >= 3)
  })

  it("includes edges from main artist to similar artists", {
    result <- render_similar_artists_network(NULL, "Artist A", similar_artists)
    expect_true(nrow(result$x$edges) >= 2)
  })
})
