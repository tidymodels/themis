test_that("smotenc assigns categorical column by majority vote of minority neighbors", {
  df <- data.frame(
    x1 = c(0, 1, 2, 3, 100, 200, 201, 202, 203, 204, 205, 206),
    x2 = rep(0, 12),
    cat = factor(c("a", "a", "a", "a", "b", "x", "x", "x", "x", "x", "x", "x")),
    class = factor(c(rep("min", 5), rep("maj", 7)))
  )
  result <- smotenc(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  # x1=100 (cat="b") is too far to be a k=3 nearest neighbor of any cluster point
  # → every source point's 3 nearest minority neighbors all have cat="a"
  # → majority vote always returns "a", regardless of seed
  expect_true(all(as.character(synthetic$cat) == "a"))
})

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
