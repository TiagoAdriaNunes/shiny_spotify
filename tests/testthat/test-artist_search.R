box::use(
  httr2[response, response_json],
  shiny[isolate, reactiveVal, testServer],
  testthat[expect_equal, expect_match, expect_no_match, expect_null, expect_warning, test_that],
)
box::use(
  app / view / artist_search,
)

test_that("the search input is named for screen readers without a visible label", {
  html <- as.character(artist_search$ui("search"))
  expect_match(html, 'id="search-artist_name"[^>]*aria-label="Artist name"')
  # A visible label would push the input below the Search button
  expect_no_match(html, "<label[^>]*>[^<]+</label>")
})

test_that("the Search button says it's busy without a spinner of its own", {
  html <- as.character(artist_search$ui("search"))
  # The result cards show the loading spinner; one on the button too was noise
  expect_match(html, '<span slot="busy">Searching...</span>', fixed = TRUE)
  expect_no_match(html, "fa-spin", fixed = TRUE)
})

# Runs a search for "Artist" against the mocked `api` and returns the
# rendered message plus the values written to the shared reactives.
run_search <- function(api) {
  forget_memo(artist_search, "search_spotify_memo")
  local_spotify_api(api, env = parent.frame())
  selected_id <- reactiveVal(NULL)
  selected_name <- reactiveVal(NULL)
  message <- NULL
  testServer(
    artist_search$server,
    args = list(selected_artist_id = selected_id, selected_artist_name = selected_name),
    {
      session$setInputs(artist_name = "Artist", search = 1)
      message <<- output$artist_info
    }
  )
  list(message = message, id = isolate(selected_id()), name = isolate(selected_name()))
}

test_that("a successful search selects the first matching artist", {
  result <- run_search(function(req) {
    response_json(body = list(artists = list(items = list(
      list(id = "id1", name = "Artist A"),
      list(id = "id2", name = "Artist B")
    ))))
  })
  expect_equal(result$message, "Found artist: Artist A")
  expect_equal(result$id, "id1")
  expect_equal(result$name, "Artist A")
})

test_that("a search with no results says so and selects nothing", {
  result <- run_search(function(req) {
    response_json(body = list(artists = list(items = list())))
  })
  expect_equal(result$message, "Artist not found.")
  expect_null(result$id)
  expect_null(result$name)
})

test_that("an API failure shows an unavailable message instead of crashing", {
  expect_warning(
    result <- run_search(function(req) response(status_code = 503)),
    "Spotify search failed"
  )
  expect_equal(result$message, "Artist search unavailable. Please try again later.")
  expect_null(result$id)
})
