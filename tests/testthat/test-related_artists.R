box::use(
  checkmate[expect_class, expect_data_frame],
  httr2[
    response_json,
    url_parse
  ],
  shiny[reactiveVal, testServer],
  testthat[describe, expect_equal, expect_error, expect_match, it, test_that],
)
box::use(
  app/view/related_artists,
  app/view/related_artists[render_similar_artists_network],
)

describe("related_artists server", {
  # Fake Last.fm: "Artist A" has one similar artist, everyone else has none
  lastfm_similar <- function(req) {
    similar <- if (url_parse(req$url)$query$artist == "Artist A") {
      list(list(name = "Artist B", match = "0.9"))
    } else {
      list()
    }
    response_json(body = list(similarartists = list(artist = similar)))
  }

  # Runs the module, applying each artist name in `names` in turn, and
  # returns the rendered HTML after each one
  render_related <- function(names) {
    forget_memo(related_artists, "get_similar_artists_memo")
    local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm_similar, env = parent.frame())
    artist_name <- reactiveVal(names[[1]])
    html <- list()
    testServer(related_artists$server, args = list(artist_name = artist_name), {
      for (name in names) {
        artist_name(name)
        session$flushReact()
        html[[length(html) + 1]] <<- output$related_artists_network$html
      }
    })
    html
  }

  it("asks for an artist when none is selected", {
    html <- render_related(list(NULL))
    expect_match(html[[1]], "No artist selected.", fixed = TRUE)
  })

  it("shows a message when the artist has no similar artists", {
    html <- render_related(list("Artist Z"))
    expect_match(html[[1]], "No similar artists found.", fixed = TRUE)
  })

  it("draws the network for an artist with similar artists", {
    html <- render_related(list("Artist A"))
    expect_match(html[[1]], "visNetwork", fixed = TRUE)
    expect_match(html[[1]], "Artist B", fixed = TRUE)
  })

  it("follows the selected artist as it changes, including back to none", {
    html <- render_related(list("Artist A", "Artist Z", NULL))
    expect_equal(length(html), 3)
    expect_match(html[[1]], "Artist B", fixed = TRUE)
    expect_match(html[[2]], "No similar artists found.", fixed = TRUE)
    expect_match(html[[3]], "No artist selected.", fixed = TRUE)
  })
})

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

test_that("the network fills the card width instead of a fixed 960px", {
  local_spotify_api(
    function(req) stop("unexpected Spotify request"),
    lastfm = function(req) response_json(body = list(similarartists = list(artist = list())))
  )
  network <- render_similar_artists_network(NULL, "Artist A", data.frame(name = "Artist B", match = 0.9))
  expect_equal(network$width, "100%")
  expect_equal(network$height, "400px")
})

test_that("node tooltips use readable dark-theme colours", {
  similar_artists <- data.frame(name = "Artist B", match = 0.9)
  local_spotify_api(
    function(req) stop("unexpected Spotify request"),
    lastfm = function(req) response_json(body = list(similarartists = list(artist = list())))
  )
  style <- render_similar_artists_network(NULL, "Artist A", similar_artists)$x$tooltipStyle
  # A real `color` property: visNetwork's default uses `font-color`, which
  # browsers ignore, leaving white theme text on a light background
  expect_match(style, "(^|;|\\s)color: #E0E0E0;")
  expect_match(style, "background-color: #1F1F1F;", fixed = TRUE)
  # visNetwork needs these to position and show/hide the tooltip
  expect_match(style, "position: fixed;", fixed = TRUE)
  expect_match(style, "visibility: hidden;", fixed = TRUE)
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
