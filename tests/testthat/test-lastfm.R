box::use(
  digest[digest],
  testthat[expect_equal, expect_false, test_that],
)
box::use(
  app / logic / lastfm[create_signature],
)

test_that("create_signature signs params sorted by key with the secret appended", {
  result <- create_signature(list(method = "auth.getSession", api_key = "key"), "secret")
  expected <- digest("api_keykeymethodauth.getSessionsecret", algo = "md5", serialize = FALSE)
  expect_equal(result, expected)
})

test_that("create_signature ignores the format param", {
  with_format <- create_signature(list(api_key = "key", format = "json"), "secret")
  without_format <- create_signature(list(api_key = "key"), "secret")
  expect_equal(with_format, without_format)
})

test_that("create_signature does not depend on param order", {
  expect_equal(
    create_signature(list(b = "2", a = "1"), "secret"),
    create_signature(list(a = "1", b = "2"), "secret")
  )
})

test_that("create_signature changes with the secret", {
  params <- list(api_key = "key")
  expect_false(create_signature(params, "one") == create_signature(params, "two"))
})
