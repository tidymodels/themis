test_that("adasyn() interfaces correctly", {

  expect_error(adasyn(circle_example, var = "class"), NA)

  expect_error(adasyn(circle_example, var = "Class"))

  expect_error(adasyn(circle_example, var = c("class", "x")))

  expect_error(adasyn(circle_example, var = "x"))

  circle_example0 <- circle_example
  circle_example0[1, 1] <- NA

  expect_error(adasyn(circle_example0, var = "class"), "missing values")

  expect_error(adasyn(circle_example, var = "class", k = 0))

  expect_error(adasyn(circle_example, var = "class", k = -1))

  expect_error(adasyn(circle_example, var = "class", k = c(5, 10)))
})
