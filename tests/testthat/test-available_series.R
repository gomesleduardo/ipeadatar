test_that("available_series returns a data frame", {
  
  x <- available_series(label = FALSE)
  
  expect_s3_class(x, "data.frame")
  
  expect_true(all(
    c(
      "code",
      "name",
      "theme",
      "source",
      "freq",
      "lastupdate",
      "status"
    ) %in% names(x)
  ))
  
})
