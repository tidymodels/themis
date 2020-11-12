set.seed(1234)
test_that("samples stay inside convex hull of data.", {
  rdata <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2)

  expect_true(all(dplyr::between(smote_data(rdata, 3, 100), 0, 1)))
})

test_that("order doesn't matter", {
  df <- data.frame(
    target = rep(c("Yes", "No"), c(10, 50)),
    x = rep(1:2, c(10, 50))
  )
  expect_equal(100, nrow(themis:::smote(df, "target")))

  df <- data.frame(
    target = rep(c("Yes", "No"), c(50, 10)),
    x = rep(1:2, c(50, 10))
  )
  expect_equal(100, nrow(themis:::smote(df, "target")))
})
