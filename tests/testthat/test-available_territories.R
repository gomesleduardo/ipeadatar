test_that("available_territories returns a data frame", {
  
  skip_on_cran()
  skip_if_offline()
  
  x <- available_territories(label = FALSE)
  
  expect_s3_class(x, "data.frame")
  
})
