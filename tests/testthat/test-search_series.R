test_that("search_series validates terms", {
  
  skip_on_cran()
  skip_if_offline()
  
  expect_error(
    search_series(terms = 1),
    class = "ipeadata_invalid_terms"
  )
  
  expect_error(
    search_series(terms = NA_character_),
    class = "ipeadata_invalid_terms"
  )
  
})
