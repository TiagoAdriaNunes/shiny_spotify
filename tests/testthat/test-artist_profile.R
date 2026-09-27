box::use(
  checkmate[expect_string],
  httr2[response, response_json],
  shiny[reactiveVal, testServer],
  testthat[describe, expect_equal, expect_error, expect_false, expect_match, expect_true, it, test_that],
)
box::use(
  app/view/artist_profile,
  app/view/artist_profile[generate_svg_circle],
)

test_that("generate_svg_circle returns a string", {
  result <- generate_svg_circle(50)
  expect_string(result, min.chars = 1)
})

test_that("generate_svg_circle rejects popularity outside 0-100", {
  expect_error(generate_svg_circle(-1), "popularity_value")
  expect_error(generate_svg_circle(101), "popularity_value")
  expect_error(generate_svg_circle(NA), "popularity_value")
  expect_error(generate_svg_circle(c(10, 20)), "popularity_value")
})

test_that("generate_svg_circle output contains SVG tags", {
  result <- generate_svg_circle(50)
  expect_true(grepl("<svg", result))
  expect_true(grepl("<circle", result))
  expect_true(grepl("<text", result))
})

test_that("generate_svg_circle embeds the popularity value", {
  result <- generate_svg_circle(75)
  expect_true(grepl("75", result))
})

describe("generate_svg_circle boundary values", {
  it("handles popularity = 0", {
    result <- generate_svg_circle(0)
    expect_true(grepl("<svg", result))
    expect_true(grepl("0", result))
  })

  it("handles popularity = 100", {
    result <- generate_svg_circle(100)
    expect_true(grepl("<svg", result))
    expect_true(grepl("100", result))
  })
})

test_that("generate_svg_circle produces different sizes for different popularity", {
  low <- generate_svg_circle(10)
  high <- generate_svg_circle(90)
  expect_false(identical(low, high))
})

describe("artist_profile server", {
  run_profile <- function(artist, code) {
    forget_memo(artist_profile, "get_artist_memo")
    local_spotify_api(function(req) artist, env = parent.frame())
    testServer(artist_profile$server, args = list(artist_id = reactiveVal("artist-id")), {
      session$flushReact()
      code(output)
    })
  }

  it("renders every field of the selected artist", {
    artist <- response_json(body = list(
      name = "Artist A",
      popularity = 80,
      followers = list(total = 1234567),
      genres = list("electro", "house"),
      images = list(list(url = "large.jpg"), list(url = "medium.jpg"))
    ))
    run_profile(artist, function(output) {
      expect_equal(output$artist_name, "Artist A")
      expect_equal(output$artist_followers, "Followers: 1,234,567")
      expect_equal(output$artist_genres, "Genres: electro, house")
      expect_match(output$artist_image$html, 'src="medium.jpg"', fixed = TRUE)
      expect_match(output$artist_popularity_circle$html, "<svg", fixed = TRUE)
    })
  })

  it("falls back to placeholders when Spotify omits deprecated fields", {
    run_profile(response_json(body = list(name = "Artist A")), function(output) {
      expect_equal(output$artist_name, "Artist A")
      expect_equal(output$artist_followers, "Followers not available.")
      expect_equal(output$artist_genres, "Genres not available.")
      expect_match(output$artist_image$html, "Image not available.", fixed = TRUE)
      expect_match(output$artist_popularity_circle$html, "Popularity not available.", fixed = TRUE)
    })
  })

  it("shows placeholders instead of failing when the API errors", {
    run_profile(response(status_code = 500), function(output) {
      expect_equal(output$artist_name, "Name not available.")
      expect_equal(output$artist_followers, "Followers not available.")
    })
  })
})
