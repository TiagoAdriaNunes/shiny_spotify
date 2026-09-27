box::use(
  checkmate[test_null],
  httr2[
    response,
    response_json,
    url_parse
  ],
  jsonlite[fromJSON],
  purrr[keep],
  shiny[testServer],
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_no_match,
    expect_null,
    expect_warning,
    test_that
  ],
)
box::use(
  app / view / genre_filter,
)

default_artists <- list(
  list(name = "Artist A", url = "https://www.last.fm/music/Artist+A", `@attr` = list(rank = "1")),
  list(name = "Artist B", url = "https://www.last.fm/music/Artist+B", `@attr` = list(rank = "2"))
)

# Fake Last.fm answering each method the genre tab uses
lastfm_mock <- function(artists = default_artists, top_tags = list(list(name = "rock"))) {
  function(req) {
    query <- url_parse(req$url)$query
    switch(
      query$method,
      chart.getTopTags = response_json(body = list(tags = list(tag = top_tags))),
      tag.getTopArtists = response_json(body = list(topartists = list(artist = artists))),
      artist.getInfo = response_json(
        body = list(artist = list(stats = list(listeners = "1500", playcount = "3000000000")))
      ),
      stop("Unexpected Last.fm method: ", query$method)
    )
  }
}

# Searches `genre` against the mocked Last.fm and returns the rendered outputs
search_genre <- function(genre, lastfm = lastfm_mock()) {
  for (memo in c("get_top_genres_memo", "get_genre_artists_memo", "get_artist_stats_memo")) {
    forget_memo(genre_filter, memo)
  }
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm, env = parent.frame())
  result <- NULL
  testServer(genre_filter$server, {
    session$setInputs(genre = genre, search = 1)
    result <<- list(
      message = output$message,
      table = output$artist_table,
      chart = output$listeners_chart,
      plays_chart = output$plays_chart
    )
  })
  result
}

test_that("renders the table and chart for the genre's top artists", {
  result <- search_genre("rock")
  expect_equal(result$message, "")
  expect_false(test_null(result$table))
  expect_match(result$table, "Artist B", fixed = TRUE)
  expect_match(result$table, "https://www.last.fm/music/Artist+A", fixed = TRUE)
  # Play counts past 2^31 survive as numbers
  expect_match(result$table, "3e+09|3000000000")
  expect_match(result$chart, "tagged 'rock': Last.fm listeners", fixed = TRUE)
  expect_match(result$plays_chart, "tagged 'rock': Last.fm total plays", fixed = TRUE)
  series <- fromJSON(result$chart, simplifyVector = FALSE)$x$ax_opts$series
  plays <- fromJSON(result$plays_chart, simplifyVector = FALSE)$x$ax_opts$series
  expect_equal(length(series), 1)
  expect_equal(length(plays), 1)
  expect_equal(series[[1]]$name, "Listeners")
  expect_equal(plays[[1]]$name, "Total plays")
  expect_equal(series[[1]]$data[[1]]$y, 1500)
  expect_equal(plays[[1]]$data[[1]]$y, 3000000000)
  expect_equal(series[[1]]$data[[1]]$x, plays[[1]]$data[[1]]$x)
})

test_that("successive searches replace the chart data and title", {
  for (memo in c("get_top_genres_memo", "get_genre_artists_memo", "get_artist_stats_memo")) {
    forget_memo(genre_filter, memo)
  }
  lastfm <- function(req) {
    query <- url_parse(req$url)$query
    if (query$method == "tag.getTopArtists") {
      return(response_json(body = list(topartists = list(artist = list(
        list(name = paste(query$tag, "artist"), url = "https://www.last.fm/music/test", `@attr` = list(rank = "1"))
      )))))
    }
    if (query$method == "artist.getInfo") {
      listeners <- if (query$artist == "rock artist") "100" else "200"
      return(response_json(body = list(artist = list(stats = list(listeners = listeners, playcount = "1000")))))
    }
    lastfm_mock()(req)
  }
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm, env = environment())
  testServer(genre_filter$server, {
    session$setInputs(genre = "rock", search = 1)
    rock <- fromJSON(output$listeners_chart, simplifyVector = FALSE)$x
    rock_plays <- fromJSON(output$plays_chart, simplifyVector = FALSE)$x
    expect_equal(rock$ax_opts$series[[1]]$data[[1]]$x, "rock artist")
    expect_equal(rock$ax_opts$series[[1]]$data[[1]]$y, 100)
    session$setInputs(genre = "pop")
    expect_equal(fromJSON(output$listeners_chart, simplifyVector = FALSE)$x, rock)
    expect_equal(fromJSON(output$plays_chart, simplifyVector = FALSE)$x, rock_plays)
    session$setInputs(search = 2)
    pop <- fromJSON(output$listeners_chart, simplifyVector = FALSE)$x
    expect_equal(pop$ax_opts$series[[1]]$data[[1]]$x, "pop artist")
    expect_equal(pop$ax_opts$series[[1]]$data[[1]]$y, 200)
    pop_plays <- fromJSON(output$plays_chart, simplifyVector = FALSE)$x
    expect_equal(pop_plays$ax_opts$series[[1]]$data[[1]]$x, "pop artist")
    expect_equal(pop_plays$ax_opts$series[[1]]$data[[1]]$y, 1000)
    expect_match(pop_plays$ax_opts$title$text, "tagged 'pop'", fixed = TRUE)
    expect_false(pop_plays$auto_update)
    expect_match(pop$ax_opts$title$text, "tagged 'pop'", fixed = TRUE)
    # Recreating the browser chart avoids stale bars from automatic updates.
    expect_false(pop$auto_update)
  })
})

test_that("still renders when an artist's stats can't be fetched", {
  failing_stats <- function(req) {
    if (url_parse(req$url)$query$method == "artist.getInfo") {
      return(response(status_code = 500))
    }
    lastfm_mock()(req)
  }
  result <- search_genre("jazz", lastfm = failing_stats)
  expect_equal(result$message, "")
  expect_match(result$table, "Artist A", fixed = TRUE)
  # With neither listener nor play counts there's nothing to chart
  expect_null(fromJSON(result$chart)$x)
  expect_null(fromJSON(result$plays_chart)$x)
})

test_that("tells the user when no artists match the genre", {
  result <- search_genre("not-a-genre", lastfm = lastfm_mock(artists = list()))
  expect_equal(result$message, "No artists found for the genre 'not-a-genre'. Please try a different genre.")
  # A widget rendered from NULL serialises with no data (`x`)
  expect_null(fromJSON(result$table)$x)
  expect_null(fromJSON(result$chart)$x)
  expect_null(fromJSON(result$plays_chart)$x)
})

test_that("shows an unavailable message when Last.fm returns an error", {
  lastfm_error <- function(req) {
    response_json(status_code = 403, body = list(error = 10, message = "Invalid API key"))
  }
  expect_warning(
    result <- search_genre("rock", lastfm = lastfm_error),
    "Last.fm genre search failed: .*Invalid API key"
  )
  expect_equal(result$message, "Genre search unavailable. Please try again later.")
  expect_null(fromJSON(result$table)$x)
})

# Renders the genre tab UI against the mocked Last.fm and returns its HTML
render_genre_ui <- function(lastfm) {
  forget_memo(genre_filter, "get_top_genres_memo")
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm, env = parent.frame())
  as.character(genre_filter$ui("genre_filter"))
}

test_that("the genre list is rendered into the page from Last.fm's top tags", {
  top_tags <- list(list(name = "rock"), list(name = "seen live"), list(name = "jazz"))
  html <- render_genre_ui(lastfm_mock(top_tags = top_tags))
  expect_match(html, '<option value="rock">rock</option>', fixed = TRUE)
  expect_match(html, '<option value="jazz">jazz</option>', fixed = TRUE)
  expect_no_match(html, "seen live", fixed = TRUE)
})

test_that("the genre tab still renders when Last.fm's top tags can't be fetched", {
  expect_warning(
    html <- render_genre_ui(function(req) response(status_code = 500)),
    "Last.fm top genres fetch failed"
  )
  expect_match(html, 'id="genre_filter-genre"', fixed = TRUE)
  expect_no_match(html, "<option value=\"[^\"]+\">", perl = TRUE)
})

test_that("the results table reserves its height before a search", {
  html <- render_genre_ui(lastfm_mock())
  # .genre-table's min-height is set in main.scss
  expect_match(html, '<div class="genre-table">\\s*<div [^>]*id="genre_filter-artist_table"')
})

test_that("the genre Search button says it's busy without a spinner of its own", {
  html <- render_genre_ui(lastfm_mock())
  expect_match(html, '<span slot="busy">Searching...</span>', fixed = TRUE)
  expect_no_match(html, "fa-spin", fixed = TRUE)
})

# Searches "rock", then applies `clicks` (a list of input values, e.g.
# list(artist_clicked = "Artist A")) one after another. `open_artist` stands in
# for the app's function; returns the artists it was asked to open and the
# outputs after each click.
click_genre_artists <- function(clicks, open_artist_result = TRUE, with_open_artist = TRUE) {
  for (memo in c("get_top_genres_memo", "get_genre_artists_memo", "get_artist_stats_memo")) {
    forget_memo(genre_filter, memo)
  }
  local_spotify_api(function(req) stop("unexpected Spotify request"), lastfm = lastfm_mock(), env = parent.frame())
  opened <- character(0)
  open_artist <- function(name) {
    opened <<- c(opened, name)
    open_artist_result
  }
  after <- list()
  testServer(
    genre_filter$server,
    args = list(open_artist = if (with_open_artist) open_artist),
    {
      session$setInputs(genre = "rock", search = 1)
      after[["search"]] <<- list(message = output$message, table = output$artist_table)
      for (click in clicks) {
        do.call(session$setInputs, click)
        after[[length(after) + 1]] <<- list(message = output$message, table = output$artist_table)
      }
    }
  )
  list(opened = opened, after = after)
}

# The rendered HTML of each cell in the table's Artist column
artist_cells <- function(table) {
  columns <- fromJSON(table, simplifyVector = FALSE)$x$tag$attribs$columns
  artist_column <- keep(columns, \(column) column$id == "name")[[1]]
  unlist(artist_column$cell)
}

test_that("artist names are buttons that report which artist was clicked", {
  cells <- artist_cells(click_genre_artists(list())$after$search$table)
  expect_equal(length(cells), 2)
  expect_match(cells, '<button type="button" class="genre-artist-link"', fixed = TRUE, all = TRUE)
  expect_match(cells, 'data-input-id="proxy1-artist_clicked"', fixed = TRUE, all = TRUE)
  expect_match(cells[[2]], 'data-artist="Artist B"', fixed = TRUE)
})

test_that("each artist keeps an icon link to their Last.fm page", {
  cell <- artist_cells(click_genre_artists(list())$after$search$table)[[1]]
  expect_match(cell, 'href="https://www.last.fm/music/Artist+A"', fixed = TRUE)
  expect_match(cell, 'class="lastfm-link"', fixed = TRUE)
  expect_match(cell, 'aria-label="Open Artist A on Last.fm"', fixed = TRUE)
  # Rendered as an icon, not shown as escaped SVG text
  expect_match(cell, "<svg ", fixed = TRUE)
  expect_no_match(cell, "&lt;svg", fixed = TRUE)
})

test_that("artist names aren't buttons when the app doesn't provide open_artist", {
  cells <- artist_cells(click_genre_artists(list(), with_open_artist = FALSE)$after$search$table)
  expect_no_match(cells, "genre-artist-link", fixed = TRUE)
  expect_match(cells, 'class="lastfm-link"', fixed = TRUE, all = TRUE)
})

test_that("clicking an artist's name or chart bar asks the app to open their profile", {
  result <- click_genre_artists(list(
    list(artist_clicked = "Artist A"),
    list(chart_click = "Artist B"),
    list(plays_chart_click = "Artist A")
  ))
  expect_equal(result$opened, c("Artist A", "Artist B", "Artist A"))
})

test_that("a chart bar being deselected doesn't open anything", {
  result <- click_genre_artists(list(list(chart_click = NULL)))
  expect_equal(result$opened, character(0))
})

test_that("says so when Spotify has no match, until the next genre search", {
  result <- click_genre_artists(
    list(list(artist_clicked = "Artist A"), list(search = 2)),
    open_artist_result = FALSE
  )
  expect_equal(result$after[[2]]$message, "'Artist A' wasn't found on Spotify.")
  expect_equal(result$after[[3]]$message, "")
})
