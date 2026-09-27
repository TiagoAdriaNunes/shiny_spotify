box::use(
  httr2[
    response,
    response_json,
    url_parse
  ],
  shiny[
    reactiveVal,
    testServer
  ],
  testthat[
    describe,
    expect_equal,
    expect_match,
    expect_no_match,
    expect_null,
    it,
    test_that
  ],
)
box::use(
  app / view / artist_profile,
  app / view / artist_profile[render_genre_tags, render_release_stats],
)

render_html <- function(tag) as.character(tag)

releases <- list(
  counts = c(album = 15L, single = 20L, appears_on = 28L),
  latest = list(
    name = "Latest Album",
    type = "album",
    release_date = "2023-11-17",
    url = "https://open.spotify.com/album/latest"
  )
)

describe("render_release_stats", {
  it("shows a count for every release group", {
    html <- render_html(render_release_stats(releases))
    expect_match(html, "Albums", fixed = TRUE)
    expect_match(html, "Singles &amp; EPs", fixed = TRUE)
    expect_match(html, "Appears on", fixed = TRUE)
    expect_match(html, ">28<", fixed = TRUE)
  })

  it("ignores release groups it has no label for", {
    counts <- c(releases$counts, compilation = 7L)
    html <- render_html(render_release_stats(list(counts = counts, latest = NULL)))
    expect_equal(lengths(regmatches(html, gregexpr('class="release-stat"', html))), 3)
    expect_no_match(html, ">7<", fixed = TRUE)
  })

  it("links the latest release with its type and year", {
    html <- render_html(render_release_stats(releases))
    expect_match(html, 'href="https://open.spotify.com/album/latest"', fixed = TRUE)
    expect_match(html, "Latest Album", fixed = TRUE)
    expect_match(html, "· Album · 2023", fixed = TRUE)
  })

  it("omits the latest release line when there isn't one", {
    html <- render_html(render_release_stats(list(counts = releases$counts, latest = NULL)))
    expect_no_match(html, "Latest release", fixed = TRUE)
  })

  it("shows a placeholder when releases couldn't be fetched", {
    expect_match(render_html(render_release_stats(NULL)), "Releases not available.", fixed = TRUE)
  })
})

describe("render_genre_tags", {
  it("shows a tag per genre with Last.fm attribution", {
    html <- render_html(render_genre_tags(c("electronic", "house")))
    expect_equal(lengths(regmatches(html, gregexpr('class="genre-tag"', html))), 2)
    expect_match(html, "Genres from Last.fm", fixed = TRUE)
  })

  it("shows a placeholder when there are no genres", {
    expect_match(render_html(render_genre_tags(character(0))), "Genres not available.", fixed = TRUE)
    expect_match(render_html(render_genre_tags(NULL)), "Genres not available.", fixed = TRUE)
  })
})

describe("artist_profile server", {
  artist <- list(
    name = "Artist A",
    external_urls = list(spotify = "https://open.spotify.com/artist/artist-id"),
    images = list(list(url = "large.jpg"), list(url = "medium.jpg"))
  )

  albums <- function(req) {
    group <- url_parse(req$url)$query$include_groups
    item <- list(
      name = paste("Newest", group),
      album_type = group,
      release_date = "2020-01-01",
      external_urls = list(spotify = "https://open.spotify.com/album/x")
    )
    response_json(body = list(total = 3, items = list(item)))
  }

  spotify <- function(req) {
    if (grepl("/albums", req$url, fixed = TRUE)) {
      return(albums(req))
    }
    response_json(body = artist)
  }

  top_tags <- function(req) {
    response_json(body = list(toptags = list(tag = list(list(name = "electronic"), list(name = "house")))))
  }

  # Runs the profile for "artist-id" against the mocked APIs and returns the
  # rendered outputs
  run_profile <- function(spotify, lastfm = top_tags) {
    for (memo in c("get_artist_memo", "get_release_summary_memo", "get_artist_tags_memo")) {
      forget_memo(artist_profile, memo)
    }
    local_spotify_api(spotify, lastfm = lastfm, env = parent.frame())
    result <- NULL
    testServer(artist_profile$server, args = list(artist_id = reactiveVal("artist-id")), {
      session$flushReact()
      result <<- list(
        name = output$artist_name,
        image = output$artist_image$html,
        link = output$artist_link$html,
        releases = output$artist_releases$html,
        genres = output$artist_genres$html
      )
    })
    result
  }

  it("renders the artist from Spotify with genres from Last.fm", {
    result <- run_profile(spotify)
    expect_equal(result$name, "Artist A")
    expect_match(result$image, 'src="medium.jpg"', fixed = TRUE)
    expect_match(result$link, 'href="https://open.spotify.com/artist/artist-id"', fixed = TRUE)
    expect_match(result$releases, "Appears on", fixed = TRUE)
    expect_match(result$releases, "Latest release", fixed = TRUE)
    expect_match(result$genres, "electronic", fixed = TRUE)
    expect_match(result$genres, "house", fixed = TRUE)
  })

  it("keeps the Spotify details when Last.fm fails", {
    result <- run_profile(spotify, lastfm = function(req) response(status_code = 500))
    expect_equal(result$name, "Artist A")
    expect_match(result$genres, "Genres not available.", fixed = TRUE)
  })

  it("keeps the artist details when the releases request fails", {
    result <- run_profile(function(req) {
      if (grepl("/albums", req$url, fixed = TRUE)) {
        return(response(status_code = 500))
      }
      response_json(body = artist)
    })
    expect_equal(result$name, "Artist A")
    expect_match(result$releases, "Releases not available.", fixed = TRUE)
  })

  it("shows placeholders and skips Last.fm when Spotify fails", {
    result <- run_profile(function(req) response(status_code = 500), lastfm = NULL)
    expect_equal(result$name, "Name not available.")
    expect_match(result$image, "Image not available.", fixed = TRUE)
    expect_null(result$link)
    expect_match(result$genres, "Genres not available.", fixed = TRUE)
  })
})
