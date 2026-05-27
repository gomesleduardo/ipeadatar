test_that("available_subjects returns a data frame", {
  
  skip_if_offline()
  
  x <- available_subjects(label = FALSE)
  
  expect_s3_class(x, "data.frame")
  
})
