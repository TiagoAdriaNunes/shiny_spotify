box::use(
  httr2[response, response_json],
  shiny[reactiveVal, testServer],
  testthat[expect_equal, expect_match, expect_warning, test_that],
)
box::use(
  app / view / artist_top_tracks,
)

# Renders the top tracks for `name` against the mocked `api` and returns
# the resulting HTML.
render_top_tracks <- function(name, api = function(req) stop("unexpected request")) {
  forget_memo(artist_top_tracks, "get_artist_top_tracks_memoized")
  local_spotify_api(api, env = parent.frame())
  html <- NULL
  testServer(artist_top_tracks$server, args = list(artist_name = reactiveVal(name)), {
    session$flushReact()
    html <<- output$top_tracks_list$html
  })
  html
}

tracks_response <- function(n) {
  items <- lapply(seq_len(n), function(i) list(id = paste0("track", i), name = paste("Song", i)))
  response_json(body = list(tracks = list(items = items)))
}

test_that("asks for an artist when none is selected", {
  html <- render_top_tracks("")
  expect_match(html, "Please select an artist", fixed = TRUE)
})

test_that("embeds a Spotify player for each track", {
  html <- render_top_tracks("Artist A", function(req) tracks_response(3))
  expect_equal(lengths(regmatches(html, gregexpr("<iframe", html))), 3)
  expect_match(html, "https://open.spotify.com/embed/track/track1", fixed = TRUE)
  expect_match(html, "https://open.spotify.com/embed/track/track3", fixed = TRUE)
})

test_that("embeds at most five tracks", {
  html <- render_top_tracks("Artist B", function(req) tracks_response(8))
  expect_equal(lengths(regmatches(html, gregexpr("<iframe", html))), 5)
})

test_that("shows a message when the artist has no tracks", {
  html <- render_top_tracks("Artist C", function(req) tracks_response(0))
  expect_match(html, "No top tracks found.", fixed = TRUE)
})

test_that("shows a message instead of crashing when the API errors", {
  expect_warning(
    html <- render_top_tracks("Artist D", function(req) response(status_code = 500)),
    "Spotify top tracks fetch failed"
  )
  expect_match(html, "No top tracks found.", fixed = TRUE)
})
