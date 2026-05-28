test_that("ipeadata returns values", {
  
  skip_on_cran()
  skip_if_offline()
  
  x <- ipeadata(
    code = "PRECOS12_IPCA12",
    label = FALSE,
    quiet = TRUE
  )
  
  expect_s3_class(x, "data.frame")
  
  expect_true(all(
    c("code", "date", "value") %in% names(x)
  ))
  
})
