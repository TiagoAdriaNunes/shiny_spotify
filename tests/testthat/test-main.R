# test-main.R
box::use(
  shiny[testServer],
  testthat[expect_true, test_that],
)
box::use(
  app/main[server],
)

test_that("main server works", {
  testServer(server, {
    session$setInputs(id = "test")
    expect_true(grepl(x = output$message, pattern = "Spotify Search App!"))
  })
})
