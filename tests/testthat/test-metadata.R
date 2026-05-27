test_that("metadata returns expected columns", {
  
  skip_if_offline()
  
  x <- metadata(
    code = "PRECOS12_IPCA12",
    label = FALSE,
    quiet = TRUE
  )
  
  expect_s3_class(x, "data.frame")
  
  expect_true("code" %in% names(x))
  
})
