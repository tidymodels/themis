test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    smotenc(matrix())
  )
  expect_snapshot(
    error = TRUE,
    smotenc(circle_example, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    smotenc(circle_example, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    smotenc(circle_example, var = "class", over_ratio = TRUE)
  )
})
