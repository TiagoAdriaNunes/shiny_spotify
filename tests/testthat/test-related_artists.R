box::use(
  checkmate[expect_class, expect_data_frame, test_null],
  httr2[
    response_json,
    url_parse
  ],
  shiny[NS, reactiveVal, testServer],
  testthat[describe, expect_equal, expect_error, expect_match, expect_null, it, test_that],
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

describe("opening a related artist's profile", {
  lastfm_one_similar <- function(req) {
    response_json(body = list(similarartists = list(artist = list(list(name = "Artist B", match = "0.9")))))
  }

  it("reports node selection to the module's input", {
    local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm_one_similar)
    network <- render_similar_artists_network(NS("related"), "Artist A", data.frame(name = "Artist B", match = 0.9))
    expect_match(network$x$events$selectNode, "Shiny.setInputValue('related-selected_artist'", fixed = TRUE)
    expect_match(network$x$events$selectNode, "this.body.data.nodes.get(params.nodes[0]).label", fixed = TRUE)
    expect_match(network$x$events$deselectNode, "Shiny.setInputValue('related-selected_artist', null", fixed = TRUE)
  })

  it("doesn't report selection when there's nowhere to open a profile", {
    local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm_one_similar)
    network <- render_similar_artists_network(NULL, "Artist A", data.frame(name = "Artist B", match = 0.9))
    expect_null(network$x$events)
  })

  # Runs the module for "Artist A" and applies `inputs` (a list of input value
  # lists) in turn. Returns the artists open_artist() was asked for and the
  # actions area's HTML after each input.
  run_node_actions <- function(inputs, open_artist_result = TRUE) {
    forget_memo(related_artists, "get_similar_artists_memo")
    local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm_one_similar, env = parent.frame())
    opened <- character(0)
    open_artist <- function(name) {
      opened <<- c(opened, name)
      open_artist_result
    }
    actions <- list()
    testServer(
      related_artists$server,
      args = list(artist_name = reactiveVal("Artist A"), open_artist = open_artist),
      {
        session$flushReact()
        for (values in inputs) {
          do.call(session$setInputs, values)
          html <- output$node_actions$html
          actions[[length(actions) + 1]] <<- if (test_null(html)) "" else as.character(html)
        }
      }
    )
    list(opened = opened, actions = actions)
  }

  it("offers to open a selected artist's profile, and opens it on click", {
    result <- run_node_actions(list(
      list(selected_artist = "Artist B"),
      list(open_selected = 1)
    ))
    expect_match(result$actions[[1]], "Open Artist B's profile", fixed = TRUE)
    expect_match(result$actions[[1]], 'id="proxy1-open_selected"', fixed = TRUE)
    expect_equal(result$opened, "Artist B")
  })

  it("offers nothing for the artist already shown, or after deselecting", {
    result <- run_node_actions(list(
      list(selected_artist = "Artist A"),
      list(selected_artist = "Artist B"),
      list(selected_artist = NULL)
    ))
    expect_equal(result$actions[[1]], "")
    expect_match(result$actions[[2]], "Open Artist B", fixed = TRUE)
    expect_equal(result$actions[[3]], "")
  })

  it("says so when Spotify has no match, until another node is selected", {
    result <- run_node_actions(
      list(
        list(selected_artist = "Artist B"),
        list(open_selected = 1),
        list(selected_artist = "Artist C")
      ),
      open_artist_result = FALSE
    )
    expect_match(result$actions[[2]], "'Artist B' wasn't found on Spotify.", fixed = TRUE)
    expect_match(result$actions[[3]], "Open Artist C", fixed = TRUE)
  })
})
