box::use(
  httr2[
    response_json,
    url_parse
  ],
  shiny[testServer],
  testthat[
    expect_equal,
    expect_match,
    expect_no_match,
    test_that
  ],
)
box::use(
  app / main[server, ui],
  app / view / artist_profile,
  app / view / artist_top_tracks,
  app / view / genre_filter,
  app / view / related_artists,
)

# Fake Spotify answering the calls made while loading an artist
spotify_mock <- function(req) {
  path <- url_parse(req$url)$path
  if (grepl("/albums$", path)) {
    return(response_json(body = list(total = 1, items = list())))
  }
  if (path == "/v1/search") {
    return(response_json(body = list(tracks = list(items = list(list(
      id = "track1", name = "Song", artists = list(list(id = "4tZwfgrHOc3mvqYlEYSvVi", name = "Daft Punk"))
    ))))))
  }
  response_json(body = list(id = basename(path), name = "Daft Punk"))
}

# Fake Last.fm answering the related artists, genre tag and genre list calls
lastfm_mock <- function(req) {
  switch(
    url_parse(req$url)$query$method,
    artist.getSimilar = response_json(body = list(similarartists = list(artist = list()))),
    artist.getTopTags = response_json(body = list(toptags = list(tag = list(list(name = "electronic"))))),
    chart.getTopTags = response_json(body = list(tags = list(tag = list(list(name = "rock"))))),
    stop("Unexpected Last.fm request: ", req$url)
  )
}

test_that("main server initializes and renders app title", {
  local_spotify_api(spotify_mock, lastfm = lastfm_mock)
  testServer(server, {
    expect_match(output$message, "Spotify Search App!", fixed = TRUE)
  })
})

test_that("profile genre tags search immediately and manual searches still work", {
  for (memo in c("get_top_genres_memo", "get_genre_artists_memo", "get_artist_stats_memo")) {
    forget_memo(genre_filter, memo)
  }
  local_spotify_api(spotify_mock, lastfm = function(req) {
    query <- url_parse(req$url)$query
    if (query$method == "tag.getTopArtists") {
      return(response_json(body = list(topartists = list(artist = list(
        list(name = paste(query$tag, "artist"), url = "https://www.last.fm/music/test", `@attr` = list(rank = "1"))
      )))))
    }
    if (query$method == "artist.getInfo") {
      return(response_json(body = list(artist = list(stats = list(listeners = "100", playcount = "1000")))))
    }
    lastfm_mock(req)
  })
  testServer(server, {
    session$setInputs(`genre_filter-genre` = "rock")
    for (genre in c("electronic", "house", "house")) {
      session$setInputs(`artist_profile-genre_clicked` = genre)
      expect_match(output[["genre_filter-artist_table"]], paste(genre, "artist"), fixed = TRUE)
      expect_match(output[["genre_filter-listeners_chart"]], paste0("tagged '", genre, "'"), fixed = TRUE)
      expect_match(output[["genre_filter-plays_chart"]], paste0("tagged '", genre, "'"), fixed = TRUE)
    }
    session$setInputs(`genre_filter-genre` = "rock", `genre_filter-search` = 1)
    expect_match(output[["genre_filter-artist_table"]], "rock artist", fixed = TRUE)
  })
})

test_that("the default artist is loaded when the app starts", {
  forget_memo(artist_profile, "get_artist_memo")
  forget_memo(artist_top_tracks, "get_artist_top_tracks_memoized")
  forget_memo(related_artists, "get_similar_artists_memo")
  requested <- character(0)
  local_spotify_api(
    function(req) {
      requested <<- c(requested, url_parse(req$url)$path)
      spotify_mock(req)
    },
    lastfm = lastfm_mock
  )
  testServer(server, {
    session$flushReact()
    expect_equal(output[["artist_profile-artist_name"]], "Daft Punk")
    expect_match(output[["artist_top_tracks-top_tracks_list"]]$html, "embed/track/track1", fixed = TRUE)
  })
  expect_match(requested, "/v1/artists/4tZwfgrHOc3mvqYlEYSvVi", fixed = TRUE, all = FALSE)
})

test_that("the search box starts empty even though the default artist is shown", {
  local_spotify_api(spotify_mock, lastfm = lastfm_mock)
  html <- as.character(ui("app"))
  expect_match(html, 'id="app-artist_search-artist_name"[^>]*value=""')
})

test_that("the three result cards reserve their height before loading", {
  local_spotify_api(spotify_mock, lastfm = lastfm_mock)
  html <- as.character(ui("app"))
  # .result-card's min-height is set in main.scss
  expect_equal(lengths(regmatches(html, gregexpr('class="card[^"]*\\bresult-card\\b', html))), 3)
})

test_that("the page no longer shows the Spotify API changes notice", {
  local_spotify_api(spotify_mock, lastfm = lastfm_mock)
  html <- as.character(ui("app"))
  expect_no_match(html, "Spotify API was changed", fixed = TRUE)
})

test_that("clicking an artist in the genre results loads their profile", {
  forget_memo(artist_profile, "get_artist_memo")
  forget_memo(artist_top_tracks, "get_artist_top_tracks_memoized")
  forget_memo(related_artists, "get_similar_artists_memo")
  # Spotify finds "Radiohead" by name, then serves its profile
  local_spotify_api(
    function(req) {
      url <- url_parse(req$url)
      if (url$path == "/v1/search" && url$query$type == "artist") {
        return(response_json(body = list(artists = list(items = list(list(id = "rh-id", name = "Radiohead"))))))
      }
      if (url$path == "/v1/artists/rh-id") {
        return(response_json(body = list(id = "rh-id", name = "Radiohead")))
      }
      spotify_mock(req)
    },
    lastfm = lastfm_mock
  )
  testServer(server, {
    session$flushReact()
    expect_equal(output[["artist_profile-artist_name"]], "Daft Punk")
    session$setInputs(`genre_filter-artist_clicked` = "Radiohead")
    expect_equal(output[["artist_profile-artist_name"]], "Radiohead")
    expect_equal(output[["artist_search-artist_info"]], "Found artist: Radiohead")
  })
})
