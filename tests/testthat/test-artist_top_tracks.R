box::use(
  httr2[response, response_json],
  purrr[imap],
  shiny[reactiveVal, testServer],
  stringr[str_glue],
  testthat[describe, expect_equal, expect_match, expect_no_match, expect_warning, it, test_that],
)
box::use(
  app / view / artist_top_tracks,
  app / view / artist_top_tracks[unique_tracks],
)

# Renders the top tracks for `name` against the mocked `api` and returns
# the resulting HTML.
render_top_tracks <- function(name, api = function(req) stop("unexpected request")) {
  forget_memo(artist_top_tracks, "get_artist_top_tracks_memoized")
  local_spotify_api(api, env = parent.frame())
  html <- NULL
  testServer(artist_top_tracks$server, args = list(artist_name = reactiveVal(name), artist_id = reactiveVal("artist-id")), {
    session$flushReact()
    html <<- output$top_tracks_list$html
  })
  html
}

# A search response with `n` tracks, or with the given track names
tracks_response <- function(n, names = as.character(str_glue("Song {seq_len(n)}")), details = FALSE) {
  items <- imap(names, function(track_name, i) {
    track <- list(id = as.character(str_glue("track{i}")), name = track_name, artists = list(list(id = "artist-id")))
    if (details) {
      track$artists <- list(list(id = "artist-id", name = "Artist A"), list(id = "guest-id", name = "Guest"))
      track$album <- list(images = list(
        list(url = as.character(str_glue("large{i}.jpg")), height = 640, width = 640),
        list(url = as.character(str_glue("small{i}.jpg")), height = 64, width = 64)
      ))
    }
    track
  })
  response_json(body = list(tracks = list(items = items)))
}

count <- function(html, pattern, ...) lengths(regmatches(html, gregexpr(pattern, html, ...)))

test_that("asks for an artist when none is selected", {
  html <- render_top_tracks("")
  expect_match(html, "Please select an artist", fixed = TRUE)
})

test_that("embeds a Spotify player for each track", {
  html <- render_top_tracks("Artist A", function(req) tracks_response(3))
  expect_equal(count(html, "<iframe"), 3)
  expect_match(html, "https://open.spotify.com/embed/track/track1", fixed = TRUE)
  expect_match(html, "https://open.spotify.com/embed/track/track3", fixed = TRUE)
})

test_that("each player sits over a placeholder and is marked loaded for the fade-in", {
  html <- render_top_tracks("Artist E", function(req) tracks_response(2))
  expect_equal(count(html, '<div class="track-embed">\\s*<div class="track-embed-placeholder"'), 2)
  # htmltools escapes the quotes; the browser decodes them when reading it
  onload <- "onload=\"this.classList.add(&#39;is-loaded&#39;)\""
  expect_equal(count(html, onload, fixed = TRUE), 2)
  # Screen readers need a name for each embedded frame
  expect_match(html, 'title="Spotify player: Song 1"', fixed = TRUE)
  expect_match(html, 'title="Spotify player: Song 2"', fixed = TRUE)
})

test_that("the placeholder shows the track's title, artists and small cover while the player loads", {
  html <- render_top_tracks("Artist F", function(req) tracks_response(1, details = TRUE))
  expect_match(html, '<div class="track-embed-title">Song 1</div>', fixed = TRUE)
  expect_match(html, '<div class="track-embed-artists">Artist A, Guest</div>', fixed = TRUE)
  expect_match(html, 'src="small1.jpg"', fixed = TRUE)
  expect_no_match(html, "large1.jpg", fixed = TRUE)
  # The player announces itself, so the placeholder is hidden from screen readers
  expect_match(html, 'class="track-embed-placeholder" aria-hidden="true"', fixed = TRUE)
})

test_that("the placeholder still renders when Spotify leaves out artist names and covers", {
  html <- render_top_tracks("Artist G", function(req) tracks_response(1))
  expect_match(html, '<div class="track-embed-title">Song 1</div>', fixed = TRUE)
  expect_no_match(html, "<img", fixed = TRUE)
})

test_that("embeds at most five tracks", {
  html <- render_top_tracks("Artist B", function(req) tracks_response(8))
  expect_equal(count(html, "<iframe"), 5)
})

test_that("shows each song once even when search returns several releases of it", {
  names <- c("Go", "Go", "Galvanize", "Go - Radio Edit", "Hey Boy Hey Girl")
  html <- render_top_tracks("Artist H", function(req) tracks_response(names = names))
  expect_equal(count(html, "<iframe"), 3)
  expect_match(html, "embed/track/track1", fixed = TRUE)
  expect_no_match(html, "embed/track/track2", fixed = TRUE)
  expect_no_match(html, "embed/track/track4", fixed = TRUE)
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

describe("unique_tracks", {
  it("keeps the first release of each song and drops edits, remasters and repeats", {
    tracks <- data.frame(
      id = 1:7,
      name = c(
        "Go", "Go", "Go - Radio Edit", "Setting Sun (Remastered 2021)",
        "Setting Sun", "GO", "Galvanize"
      )
    )
    expect_equal(unique_tracks(tracks)$id, c(1L, 4L, 7L))
  })

  it("keeps songs whose titles only share a prefix", {
    tracks <- data.frame(id = 1:2, name = c("Go", "Go With the Flow"))
    expect_equal(unique_tracks(tracks)$id, 1:2)
  })
})
