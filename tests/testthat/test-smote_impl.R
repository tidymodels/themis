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
  expect_equal(100, nrow(themis:::smote_impl(df, "target", 5, 1)))

  df <- data.frame(
    target = rep(c("Yes", "No"), c(50, 10)),
    x = rep(1:2, c(50, 10))
  )
  expect_equal(100, nrow(themis:::smote_impl(df, "target", 5, 1)))
})


test_that("smote() interfaces correctly", {

  expect_error(smote(circle_example, var = "class"), NA)

  expect_error(smote(circle_example, var = "Class"))

  expect_error(smote(circle_example, var = c("class", "x")))

  expect_error(smote(circle_example, var = "x"))

  circle_example0 <- circle_example
  circle_example0[1, 1] <- NA

  expect_error(smote(circle_example0, var = "class"), "missing values")

  expect_error(smote(circle_example, var = "class", k = 0))

  expect_error(smote(circle_example, var = "class", k = -1))

  expect_error(smote(circle_example, var = "class", k = c(5, 10)))
  })
