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

test_that("categorical vote uses all k neighbors, not the chosen partner", {
  # Minority points sit close together; the many numeric columns dilute the
  # categorical weight in the gower distance so the "b" point stays a genuine
  # nearest neighbor. Every minority point's 3 nearest neighbors then hold a
  # strict "a" majority, so the k-neighbor mode is always "a". Copying a single
  # random partner's category would instead emit "b" whenever the "b" point is
  # picked as an interpolation partner.
  x1 <- c(0, 1, 2, 3, 4, 100 + seq_len(10))
  df <- data.frame(
    x1 = x1,
    x2 = x1,
    x3 = x1,
    x4 = x1,
    x5 = x1,
    x6 = x1,
    x7 = x1,
    x8 = x1,
    cat = factor(c("a", "a", "b", "a", "a", rep("x", 10))),
    class = factor(c(rep("min", 5), rep("maj", 10)))
  )
  set.seed(1)
  result <- smotenc(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  expect_all_equal(as.character(synthetic$cat), "a")
})

test_that("fractional over_ratio target is rounded (#248)", {
  df <- data.frame(
    x = rnorm(95),
    cat = factor(sample(letters[1:3], 95, replace = TRUE)),
    class = factor(rep(c("min", "maj"), c(15, 80)))
  )
  # round(80 * 0.383) == 31, truncation would give 30
  result <- smotenc(df, var = "class", k = 5, over_ratio = 0.383)
  expect_equal(sum(result$class == "min"), 31)
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
