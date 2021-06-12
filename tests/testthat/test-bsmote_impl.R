test_that("bsmote() interfaces correctly", {

  expect_error(bsmote(circle_example, var = "class"), NA)

  expect_error(bsmote(circle_example, var = "Class"))

  expect_error(bsmote(circle_example, var = c("class", "x")))

  expect_error(bsmote(circle_example, var = "x"))

  circle_example0 <- circle_example
  circle_example0[1, 1] <- NA

  expect_error(bsmote(circle_example0, var = "class"), "missing values")

  expect_error(bsmote(circle_example, var = "class", k = 0))

  expect_error(bsmote(circle_example, var = "class", k = -1))

  expect_error(bsmote(circle_example, var = "class", k = c(5, 10)))
})
