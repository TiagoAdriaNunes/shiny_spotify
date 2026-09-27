box::use(
  httr2[
    response_json,
    url_parse
  ],
  testthat[
    describe,
    expect_equal,
    expect_error,
    expect_null,
    it,
    test_that
  ],
)
box::use(
  app / logic / artist_releases[get_release_summary, summarise_releases],
)

release_page <- function(total, name = NULL, type = NULL, date = NULL) {
  items <- if (is.null(name)) {
    data.frame()
  } else {
    data.frame(
      name = name,
      album_type = type,
      release_date = date,
      external_urls.spotify = paste0("https://open.spotify.com/album/", name)
    )
  }
  list(total = total, items = items)
}

describe("summarise_releases", {
  it("counts the releases in each group", {
    result <- summarise_releases(list(
      album = release_page(15),
      single = release_page(20),
      appears_on = release_page(28)
    ))
    expect_equal(result$counts, c(album = 15L, single = 20L, appears_on = 28L))
  })

  it("picks the newest of the latest album and latest single", {
    result <- summarise_releases(list(
      album = release_page(2, "Old Album", "album", "2013-05-20"),
      single = release_page(5, "New Single", "single", "2021-02-22"),
      appears_on = release_page(9, "Someone Else's Album", "album", "2024-01-01")
    ))
    expect_equal(
      result$latest,
      list(
        name = "New Single",
        type = "single",
        release_date = "2021-02-22",
        url = "https://open.spotify.com/album/New Single"
      )
    )
  })

  it("compares year-only release dates with full dates", {
    result <- summarise_releases(list(
      album = release_page(1, "Year Only", "album", "2020"),
      single = release_page(1, "Full Date", "single", "2019-12-31")
    ))
    expect_equal(result$latest$name, "Year Only")
  })

  it("has no latest release when the artist has no albums or singles", {
    result <- summarise_releases(list(album = release_page(0), single = release_page(0)))
    expect_equal(result$counts, c(album = 0L, single = 0L))
    expect_null(result$latest)
  })

  it("rejects unnamed pages", {
    expect_error(summarise_releases(list(release_page(1))), "names")
  })
})

test_that("get_release_summary asks Spotify for one release of each group", {
  requested <- list()
  local_spotify_api(function(req) {
    url <- url_parse(req$url)
    requested[[url$query$include_groups]] <<- url
    item <- list(
      name = "Latest",
      album_type = "album",
      release_date = "2020-01-01",
      external_urls = list(spotify = "https://open.spotify.com/album/latest")
    )
    response_json(body = list(total = 4, items = list(item)))
  })
  result <- get_release_summary("artist-id")
  expect_equal(names(requested), c("album", "single", "appears_on"))
  expect_equal(requested$album$path, "/v1/artists/artist-id/albums")
  expect_equal(requested$album$query$limit, "1")
  expect_equal(result$counts, c(album = 4L, single = 4L, appears_on = 4L))
  expect_equal(result$latest$url, "https://open.spotify.com/album/latest")
  expect_error(get_release_summary(""), "artist_id")
})
