test_that("smoten assigns categorical columns by majority vote of neighbors", {
  df <- data.frame(
    x1 = factor(c(rep("a", 4), "b", rep("z", 7))),
    x2 = factor(rep("q", 12)),
    class = factor(c(rep("min", 5), rep("maj", 7)))
  )
  result <- smoten(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  # majority of the minority rows have x1 = "a", so voting favors "a"
  expect_true(all(as.character(synthetic$x1) == "a"))
  expect_true(all(as.character(synthetic$x2) == "q"))
})

test_that("generated values only use existing category levels", {
  df <- data.frame(
    x = factor(sample(letters[1:3], 100, replace = TRUE)),
    class = factor(c(rep("min", 20), rep("maj", 80)))
  )
  result <- smoten(df, var = "class")
  expect_true(all(levels(result$x) == levels(df$x)))
  expect_true(all(as.character(result$x) %in% letters[1:3]))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    smoten(matrix())
  )
  expect_snapshot(
    error = TRUE,
    smoten(circle_example, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    smoten(circle_example, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    smoten(circle_example, var = "class", over_ratio = TRUE)
  )
})

test_that("errors on numeric predictors", {
  df <- data.frame(
    x = 1:10,
    class = factor(c(rep("min", 3), rep("maj", 7)))
  )
  expect_snapshot(
    error = TRUE,
    smoten(df, var = "class")
  )
})
