circle_example_num <- circle_example[, c("x", "y", "class")]

test_that("adasyn() interfaces correctly", {
  expect_no_error(adasyn(circle_example_num, var = "class"))

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "Class")
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example0, var = "class")
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 0)
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = -1)
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = c(5, 10))
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    adasyn(matrix())
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", over_ratio = TRUE)
  )
})
