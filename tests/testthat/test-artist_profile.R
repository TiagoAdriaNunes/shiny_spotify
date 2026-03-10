box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it, test_that],
)
box::use(
  app/view/artist_profile[generate_svg_circle],
)

test_that("generate_svg_circle returns a string", {
  result <- generate_svg_circle(50)
  expect_true(is.character(result))
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
