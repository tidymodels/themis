test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    nearmiss(matrix())
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", under_ratio = TRUE)
  )
})
