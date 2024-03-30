test_that("d2_key_val ignores empty values", {
  expect_null(
    d2_key_val(NULL)
  )

  expect_null(
    d2_key_val(NA_character_)
  )

  expect_null(
    d2_key_val("")
  )
})
